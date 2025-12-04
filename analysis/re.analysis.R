library(ggplot2)
library(tidyverse)
library(tidytext)
library(readr)
library(lubridate)
setwd("output")

##### distribution
# 1. 새 CSV 읽기 ----------------------------------------------------------

# 파일 이름/경로는 형님 환경에 맞게 수정
imdb  <- read.csv("imdb_clean_nostop.csv", stringsAsFactors = FALSE)
naver <- read.csv("reviews_naver_merged.csv", stringsAsFactors = FALSE)


# 2. imdb / naver 공통 포맷으로 맞추기 -------------------------------------

make_reviews_data <- function(english, korean) {
  imdb_std <- english %>%
    mutate(
      platform = "imdb",
      # date 컬럼 이름이 다르면 여기 수정 (예: review_date 등)
      date     = as.Date(date),
      rating   = star_rating,   # 별점 컬럼 이름이 다르면 여기 수정
      # text 컬럼도 필요하면 맞춰두기 (분석에는 안 씀)
      text     = content_clean_nostop,
      upvote   = NA_integer_,   # imdb에는 추천/비추천 없으니까 NA
      downvote = NA_integer_
    ) %>%
    select(id, platform, date, rating, text, upvote, downvote)
  
  naver_std <- korean %>%
    mutate(
      platform = "naver",
      date     = as.Date(writing_date),  # 작성일자 컬럼
      rating   = star_rating,
      text     = comment
      # naver에는 upvote / downvote 있으면 그대로 두고,
      # 없으면 미리 NA로 만들어두셨을 수도 있음
    ) %>%
    select(id, platform, date, rating, text, upvote, downvote)
  
  bind_rows(imdb_std, naver_std) %>%
    arrange(date)
}

reviews_all <- make_reviews_data(imdb, naver)

# 확인용
# reviews_all %>% count(platform)
# reviews_all %>% count(platform, rating)


# 3. 플랫폼별 rating 분포 (비율) 계산 ---------------------------------------

rating_dist <- reviews_all %>%
  filter(!is.na(rating)) %>%
  mutate(
    rating = as.integer(rating),
    rating = factor(rating, levels = 1:10)   # 1~10 순서 고정
  ) %>%
  count(platform, rating) %>%               # 플랫폼 × 점수별 개수
  group_by(platform) %>%
  mutate(prop = n / sum(n)) %>%             # 각 플랫폼 내부 비율
  ungroup()

rating_dist

# 두 개 한번에 그리기
p_side_by_side <- ggplot(rating_dist, aes(x = rating, y = prop, fill = platform)) +
  geom_col(position = "dodge") +                   # imdb / naver 옆으로
  labs(
    x = "rating",
    y = "ratio within platform",
    fill = "Platform",
    title = "IMDB vs Naver rating distribution (ratio per platform)"
  ) +
  # 필요하면 y축 범위 조정 (예: 최대가 0.6 정도면)
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_minimal()
p_side_by_side

# # facet으로 따로 그리기기
# p_facet <- ggplot(rating_dist, aes(x = rating, y = prop, fill = platform)) +
#   geom_col(show.legend = FALSE) +          # 범례는 생략 (어차피 패널 제목으로 구분)
#   facet_wrap(~ platform, ncol = 1) +       # 세로로 imdb / naver 따로
#   coord_cartesian(ylim = c(0, 0.6)) +
#   labs(
#     x = "rating",
#     y = "ratio within platform",
#     title = "Rating distribution by platform"
#   ) +
#   theme_minimal()
# 
# p_facet

################# early-stop ######################

reviews_3m_summary <- reviews_all %>%
  mutate(
    date      = as.Date(date),
    period_3m = floor_date(date, "quarter")  # 3개월 단위(1/4분기, 2/4분기...)
  ) %>%
  group_by(platform, period_3m) %>%
  summarise(
    n_reviews = n(),
    .groups   = "drop"
  ) %>%
  arrange(period_3m, platform)

reviews_3m_summary %>% View()
  

plot_rating_early_late_norm <- function(data,
                                        cut_date,
                                        platform_filter = NULL) {
  df <- data %>%
    filter(!is.na(rating)) %>%
    mutate(
      period_3m = floor_date(date, "3 months"),
      phase = if_else(period_3m < cut_date, "early", "late"),
      rating = factor(as.integer(rating), levels = 1:10)
    )
  
  # 특정 플랫폼만 보고 싶으면 필터
  if (!is.null(platform_filter)) {
    df <- df %>% filter(platform %in% platform_filter)
  }
  
  # 1단계: phase(early/late) 안에서 rating density 계산
  dens_phase <- df %>%
    count(platform, phase, rating, name = "n") %>%
    group_by(platform, phase) %>%
    mutate(prop_phase = n / sum(n)) %>%   # 예: early에서 rating=10의 0.1, late에서 0.2
    ungroup()
  
  # 2단계: 같은 rating 안에서 early/late를 다시 정규화
  # 예: 0.1, 0.2 → 0.1 / (0.1+0.2) = 0.33, 0.2 / (0.3) = 0.67
  dens_norm <- dens_phase %>%
    group_by(platform, rating) %>%
    mutate(
      prop_norm = prop_phase / sum(prop_phase)  # sum= early+late
    ) %>%
    ungroup()
  
  ggplot(dens_norm, aes(x = rating, y = prop_norm, fill = phase)) +
    geom_col() +
    facet_wrap(~ platform) +   # 플랫폼별 패널; 한 가지만 보고 싶으면 위에서 filter
    labs(
      x = "rating",
      y = "normalized share (early vs late)",
      fill = "phase",
      title = paste0("Early vs late normalized per rating (cut = ", cut_date, ")")
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_minimal()
}

# 2020-04-01 분기를 기준으로 자르기
cut_date <- as.Date("2020-04-01")

# 1) 두 플랫폼 동시에 (각각 패널로)
plot_rating_early_late_norm(reviews_all, cut_date)

# 2) 네이버만
plot_rating_early_late_norm(reviews_all, cut_date, platform_filter = "naver")

# 3) IMDB만
plot_rating_early_late_norm(reviews_all, cut_date, platform_filter = "imdb")














# reviews_all <- reviews_all %>%
#   mutate(
#     period_3m = floor_date(date, "3 months")
#   )
# 
# # early / late 컷오프: 2020-04-01 분기가 late의 시작
# cut_date <- as.Date("2020-04-01")
# 
# reviews_all <- reviews_all %>%
#   mutate(
#     phase = if_else(period_3m < cut_date, "early", "late")
#   )
# 
# # sanity check
# reviews_all %>% count(platform, phase)
# 
# rating_phase <- reviews_all %>%
#   filter(!is.na(rating)) %>%
#   mutate(rating = factor(rating, levels = 1:10)) %>%
#   count(rating, phase)
# 
# ggplot(rating_phase, aes(x = rating, y = n, fill = phase)) +
#   geom_col(position = "fill") +          # 각 rating 막대의 전체 높이가 1이 되도록 비율화
#   labs(x = "rating", y = "ratio within rating", fill = "phase") +
#   theme_minimal()
# 
# rating_phase_plat <- reviews_all %>%
#   filter(!is.na(rating)) %>%
#   mutate(rating = factor(rating, levels = 1:10)) %>%
#   count(platform, phase, rating)   # 플랫폼 × phase × rating 개수
# 
# naver_rating_phase <- rating_phase_plat %>%
#   filter(platform == "naver")
# 
# ggplot(naver_rating_phase, aes(x = rating, y = n, fill = phase)) +
#   geom_col(position = "fill") +   # rating별로 early/late 비율(합 1)
#   labs(
#     title = "Naver: rating × phase",
#     x = "rating",
#     y = "ratio within rating",
#     fill = "phase"
#   ) +
#   theme_minimal()
# 
# imdb_rating_phase <- rating_phase_plat %>%
#   filter(platform == "imdb")
# 
# ggplot(imdb_rating_phase, aes(x = rating, y = n, fill = phase)) +
#   geom_col(position = "fill") +
#   labs(
#     title = "IMDB: rating × phase",
#     x = "rating",
#     y = "ratio within rating",
#     fill = "phase"
#   ) +
#   theme_minimal()
# 
# 
# ggplot(rating_phase_plat, aes(x = rating, y = n, fill = phase)) +
#   geom_col(position = "fill") +
#   facet_wrap(~ platform, ncol = 1) +
#   labs(
#     title = "Rating × phase by platform",
#     x = "rating",
#     y = "ratio within rating",
#     fill = "phase"
#   ) +
#   theme_minimal()
# 
# ggplot(rating_phase_plat, aes(x = rating, y = n, fill = platform)) +
#   geom_col(position = "fill") +
#   facet_wrap(~ phase) +
#   labs(
#     title = "Within each phase: platform mix by rating",
#     x = "rating",
#     y = "ratio within rating",
#     fill = "platform"
#   ) +
#   theme_minimal()
