# readme기준 1번 진행
# 기본적 분석 (Shiny)
# IMDB vs Naver 평점 분포 -- 어떻게 이 영화에 대해 평가하는지, 날짜 구간 입력해서 어떻게 변화하는지 확인할 수 있게 샤이니로 보여주면 좋음
# 재훈이형이 맨 처음에 준 데이터로 진행(preprocessing 진행 x)
# 이유: 어차피 평점 분포를 볼거라서.
# 데이터 사용: reviews_imdb.csv, reviews_naver.csv

library(ggplot2)
library(tidyverse)
library(tidytext)
library(readr)
library(lubridate)
setwd("output")

imdb <- read.csv("reviews_imdb.csv")
naver <- read.csv("reviews_naver.csv")

# imdb, naver 리뷰 일단 합치고고 colname 동일하게 하기.
make_reviews_data <- function(english, korean) {
  imdb_std <- english %>%
    mutate(
      platform = "imdb",
      date     = as.Date(date),     
      rating   = star_rating,
      text     = content,
      upvote   = NA_integer_, # imdb에는 따봉 없어서 일단 이렇게만 추가해 둠
      downvote = NA_integer_
    ) %>%
    select(id, platform, date, rating, text, upvote, downvote)
  
  naver_std <- korean %>%
    mutate(
      platform = "naver",
      date     = as.Date(writing_date),
      rating   = star_rating,
      text     = comment
    ) %>%
    select(id, platform, date, rating, text, upvote, downvote)
  
  bind_rows(imdb_std, naver_std) %>%
    arrange(date)
}
reviews_all <- make_reviews_data(imdb, naver)
reviews_all %>% View

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

# ggplot(aes(x = rating), data = reviews_all %>% filter(platform == "naver")) +
#   geom_density()
# ggplot(aes(x = rating), data = reviews_all %>% filter(platform == "imdb")) +
#   geom_density()
# ggplot(aes(x = rating), data = reviews_all) +
#   geom_density()

# 기본적인 density 그리는 함수
plot_rating_density <- function(data,
                                which_platform = c("both", "naver", "imdb")) {
  which_platform <- match.arg(which_platform)
  
  df <- data
  if (which_platform != "both") {
    df <- df %>% filter(platform == which_platform)
  }
  
  ggplot(df, aes(x = rating,
                 color = platform,
                 fill  = platform)) +
    geom_density(alpha = if (which_platform == "both") 0.3 else 0.5) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
    labs(
      title = "Rating density by platform",
      x = "Rating",
      y = "Density",
      color = "Platform",
      fill  = "Platform"
    ) +
    theme_minimal()
}
# 극찬 아니면 욕이노 ㄷㄷㄷ
plot_rating_density(reviews_all, "naver")
# 얘는 약간 우상향이네. 7-9가 ㄱㅊ게 분포
plot_rating_density(reviews_all, "imdb")
plot_rating_density(reviews_all, "both") # overlay!! not stack

# 이거는 히스토그램
plot_rating_hist <- function(data,
                             which_platform = c("both", "naver", "imdb"),
                             binwidth = 1) {
  which_platform <- match.arg(which_platform)
  
  df <- data
  if (which_platform != "both") {
    df <- df %>% filter(platform == which_platform)
  }
  
  position_val <- if (which_platform == "both") "identity" else "stack"
  alpha_val    <- if (which_platform == "both") 0.6 else 0.8
  
  ggplot(df, aes(x = rating, fill = platform)) +
    geom_histogram(
      binwidth = binwidth,
      boundary = 0,
      position = position_val,
      alpha    = alpha_val
    ) +
    scale_x_continuous(breaks = 1:10, limits = c(0.5, 10.5)) +
    labs(
      title = "Rating histogram by platform",
      x = "Rating",
      y = "Count",
      fill = "Platform"
    ) +
    theme_minimal()
}
plot_rating_hist(reviews_all, "naver")
plot_rating_hist(reviews_all, "imdb")
plot_rating_hist(reviews_all, "both") # overlay!! not stack
# plot 자체는 어쩔 수 없이 imdb가 sample size 많아서 더 크게 나옵니다.

# box plot
plot_rating_box <- function(data) {
  ggplot(data, aes(x = platform, y = rating, fill = platform)) +
    geom_boxplot(alpha = 0.6, width = 0.5, outlier.alpha = 0.3) +
    scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
    labs(
      title = "Rating distribution (boxplot)",
      x = "Platform",
      y = "Rating",
      fill = "Platform"
    ) +
    theme_minimal()
}
plot_rating_box(reviews_all)

# violin plot
plot_rating_violin <- function(data) {
  ggplot(data, aes(x = platform, y = rating, fill = platform)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.1, alpha = 0.6, outlier.shape = NA) +
    scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
    labs(
      title = "Rating distribution (violin + box)",
      x = "Platform",
      y = "Rating",
      fill = "Platform"
    ) +
    theme_minimal()
}
plot_rating_violin(reviews_all)

# 기술 통계량 table
summarise_ratings <- function(data) {
  data %>%
    group_by(platform) %>%
    summarise(
      n        = n(),
      mean     = mean(rating),
      sd       = sd(rating),
      median   = median(rating),
      p25      = quantile(rating, 0.25),
      p75      = quantile(rating, 0.75),
      prop_10  = mean(rating == 10), # 전체 중에 10인 비율
      prop_7_10 = mean(rating >= 7), # 전체 중에 7이상 비율
      prop_1_3  = mean(rating <= 3), # 전체 중에 쓰레기 비율율 
      .groups = "drop"
    )
}
summarise_ratings(reviews_all)

# 날짜 구간 별로!!!
# platform_sel: "both", "imdb", "naver"
filter_by_date_platform <- function(data,
                                    start_date = NULL,
                                    end_date   = NULL,
                                    platform_sel = c("both", "imdb", "naver")) {
  platform_sel <- match.arg(platform_sel)
  
  df <- data
  
  if (!inherits(df$date, "Date")) {
    df <- df %>% mutate(date = as.Date(date))
  }
  
  if (!is.null(start_date)) {
    df <- df %>% filter(date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    df <- df %>% filter(date <= as.Date(end_date))
  }
  
  if (platform_sel != "both") {
    df <- df %>% filter(platform == platform_sel)
  }
  
  df
}

# time_unit: "day", "week", "month"
summarise_rating_trend <- function(data,
                                   start_date = NULL,
                                   end_date   = NULL,
                                   platform_sel = c("both", "imdb", "naver"),
                                   time_unit  = c("month", "week", "day"),
                                   min_n      = 5) {
  platform_sel <- match.arg(platform_sel)
  time_unit    <- match.arg(time_unit)
  
  df <- filter_by_date_platform(data, start_date, end_date, platform_sel)
  
  if (nrow(df) == 0) return(df[0, ])  # 빈 tibble 반환
  
  df %>%
    mutate(
      period = case_when(
        time_unit == "day"   ~ date,
        time_unit == "week"  ~ floor_date(date, "week", week_start = 1),
        time_unit == "month" ~ floor_date(date, "month")
      )
    ) %>%
    group_by(platform, period) %>%
    summarise(
      mean_rating = mean(rating, na.rm = TRUE),
      sd_rating   = sd(rating, na.rm = TRUE),
      n           = n(),
      .groups     = "drop"
    ) %>%
    filter(n >= min_n)
}

plot_rating_trend <- function(data,
                              start_date = NULL,
                              end_date   = NULL,
                              platform_sel = c("both", "imdb", "naver"),
                              time_unit  = c("month", "week", "day"),
                              min_n      = 1) {
  time_unit <- match.arg(time_unit)
  
  trend_df <- summarise_rating_trend(
    data         = data,
    start_date   = start_date,
    end_date     = end_date,
    platform_sel = platform_sel,
    time_unit    = time_unit,
    min_n        = min_n
  )
  
  if (nrow(trend_df) == 0) {
    warning("No data after filtering.")
    return(invisible(NULL))
  }
  
  ## --- 여기서 span 보고 라벨 간격을 유동적으로 정함 ---
  range_period  <- range(trend_df$period)
  span_days     <- as.numeric(diff(range_period))
  span_months   <- span_days / 30.4
  
  date_breaks <- dplyr::case_when(
    span_months <= 6   ~ "1 month",   # 6개월 이하 → 한 달마다
    span_months <= 18  ~ "3 months",  # 1년 반 이하 → 3개월마다
    span_months <= 36  ~ "6 months",  # 3년 이하 → 6개월마다
    TRUE               ~ "1 year"     # 그 이상 → 1년마다
  )
  ## -----------------------------------------------------
  
  ggplot(trend_df, aes(x = period, y = mean_rating, color = platform)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
    scale_x_date(
      date_labels = "%Y.%m",   # 2020.01 형식
      date_breaks = date_breaks
    ) +
    labs(
      title = "시간에 따른 평균 평점 추이",
      x     = "Time",
      y     = "Mean rating",
      color = "Platform"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # 라벨 기울이기
    )
}



# 전체 기간, 둘 다, 월별
plot_rating_trend(reviews_all, time_unit = "month")

# 2019-06-01 ~ 2020-02-29, IMDB만, 주별
plot_rating_trend(reviews_all,
                  start_date   = "2019-06-01",
                  end_date     = "2020-02-29",
                  platform_sel = "imdb",
                  time_unit    = "week",
                  min_n        = 1)

# 2019-06-01 ~ 2020-02-29, naver만, 주별
plot_rating_trend(reviews_all,
                  start_date   = "2019-06-01",
                  end_date     = "2020-02-29",
                  platform_sel = "naver",
                  time_unit    = "week",
                  min_n        = 1)


# 전체 기간, 둘 다, 월별
plot_rating_trend(reviews_all, 
                  platform_sel = "naver", 
                  time_unit = "month")

# split_prop: (0,1) 사이. 예: 0.3 → 앞 30% vs 뒤 70%
add_early_late_group <- function(data,
                                 start_date = NULL,
                                 end_date   = NULL,
                                 platform_sel = c("both", "imdb", "naver"),
                                 split_prop  = 0.5) {
  platform_sel <- match.arg(platform_sel)
  
  df <- filter_by_date_platform(data, start_date, end_date, platform_sel)
  if (nrow(df) == 0) return(df)
  
  df <- df %>% arrange(date)
  
  cutoff_date <- quantile(df$date, probs = split_prop, type = 1)
  
  df %>%
    mutate(
      period_group = if_else(date <= cutoff_date, "early", "late"),
      cutoff_date  = cutoff_date  # 나중에 참고용으로 같이 남겨둠
    )
}

plot_early_late_density <- function(data,
                                    start_date = NULL,
                                    end_date   = NULL,
                                    platform_sel = c("both", "imdb", "naver"),
                                    split_prop  = 0.5) {
  df <- add_early_late_group(
    data         = data,
    start_date   = start_date,
    end_date     = end_date,
    platform_sel = platform_sel,
    split_prop   = split_prop
  )
  if (nrow(df) == 0) {
    warning("No data after filtering.")
    return(invisible(NULL))
  }
  
  cutoff <- unique(df$cutoff_date)
  
  ggplot(df, aes(x = rating, fill = period_group)) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
    facet_wrap(~ platform) +
    labs(
      title = paste0("초기 vs 후기 평점 분포 (cutoff: ", cutoff, ")"),
      x     = "Rating",
      y     = "Density",
      fill  = "Period"
    ) +
    theme_minimal()
}

# 전체 기간, 두 플랫폼, 앞 50% vs 뒤 50%
plot_early_late_density(reviews_all, split_prop = 0.5)

# 특정 기간, 네이버만, 앞 30% vs 뒤 70%
plot_early_late_density(reviews_all,
                        start_date   = "2019-06-01",
                        end_date     = "2020-03-01",
                        platform_sel = "naver",
                        split_prop   = 0.3)

plot_early_late_density(reviews_all,
                        start_date   = "2019-06-01",
                        end_date     = "2020-03-01",
                        platform_sel = "imdb",
                        split_prop   = 0.3)


# early vs late summary table
summarise_early_late <- function(data,
                                 start_date = NULL,
                                 end_date   = NULL,
                                 platform_sel = c("both", "imdb", "naver"),
                                 split_prop  = 0.5) {
  df <- add_early_late_group(
    data         = data,
    start_date   = start_date,
    end_date     = end_date,
    platform_sel = platform_sel,
    split_prop   = split_prop
  )
  if (nrow(df) == 0) return(df[0, ])
  
  df %>%
    group_by(platform, period_group) %>%
    summarise(
      n          = n(),
      mean       = mean(rating),
      sd         = sd(rating),
      median     = median(rating),
      p25        = quantile(rating, 0.25),
      p75        = quantile(rating, 0.75),
      prop_10    = mean(rating == 10),
      prop_9_10  = mean(rating >= 9),
      .groups    = "drop"
    )
}
summarise_early_late(reviews_all, split_prop = 0.5)
summarise_early_late(reviews_all,
                     start_date   = "2019-06-01",
                     end_date     = "2020-02-29",
                     platform_sel = "imdb",
                     split_prop   = 0.4)


#######################################################################
# 그냥 하면 ㅈ될거 같은데 싶어서...
reviews_3m_summary
reviews_3m_summary %>% filter(platform == "naver") %>% View()
reviews_3m_summary %>% filter(platform == "imdb") %>% View()

# 걍합친거
q_summary <- reviews_all %>%
  mutate(period_q = floor_date(date, "quarter")) %>%
  group_by(platform, period_q) %>%
  summarise(n = n(), .groups = "drop")

# 두 플랫폼 모두 n >= 20인 분기만 추출
valid_periods <- q_summary %>%
  group_by(period_q) %>%
  filter(all(n >= 20)) %>%
  distinct(period_q)

# 이 분기들에 속한 리뷰만 남기기
reviews_dense <- reviews_all %>%
  mutate(period_q = floor_date(date, "quarter")) %>%
  semi_join(valid_periods, by = "period_q")

reviews_dense$platform %>% table
# imdb naver 
# 1944   954 

# 이제는
plot_rating_trend(reviews_dense, time_unit = "month")






add_early_late_cutoff <- function(data,
                                  cutoff_date,
                                  start_date  = NULL,
                                  end_date    = NULL,
                                  platform_sel = c("both", "imdb", "naver")) {
  platform_sel <- match.arg(platform_sel)
  cutoff_date  <- as.Date(cutoff_date)
  
  df <- filter_by_date_platform(data, start_date, end_date, platform_sel)
  if (nrow(df) == 0) return(df)
  
  df %>%
    mutate(
      period_group = if_else(date < cutoff_date, "early", "late"),
      cutoff_date  = cutoff_date
    )
}

plot_early_late_density_cutoff <- function(data,
                                           cutoff_date,
                                           start_date  = NULL,
                                           end_date    = NULL,
                                           platform_sel = c("both", "imdb", "naver")) {
  df <- add_early_late_cutoff(
    data         = data,
    cutoff_date  = cutoff_date,
    start_date   = start_date,
    end_date     = end_date,
    platform_sel = platform_sel
  )
  
  if (nrow(df) == 0) {
    warning("No data after filtering.")
    return(invisible(NULL))
  }
  
  cutoff <- unique(df$cutoff_date)
  
  ggplot(df, aes(x = rating, fill = period_group)) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
    facet_wrap(~ platform) +
    labs(
      title = paste0("초기 vs 후기 평점 분포 (cutoff: ", cutoff, ")"),
      x     = "Rating",
      y     = "Density",
      fill  = "Period"
    ) +
    theme_minimal()
}

# 2020년 1분기까지 early, 이후 late
plot_early_late_density_cutoff(
  reviews_dense,
  cutoff_date  = "2020-04-01",  # 4월 1일 직전까지 early
  platform_sel = "both"
)

# 네이버만 보고 싶으면
plot_early_late_density_cutoff(
  reviews_dense,
  cutoff_date  = "2020-04-01",
  platform_sel = "naver"
)
