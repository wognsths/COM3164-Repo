library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)

# 단어별 빈도 테이블 만들기
imdb_words <- imdb_clean_nostop %>%
  select(id, content_clean_nostop) %>%
  unnest_tokens(word, content_clean_nostop) %>%   # 단어 토큰화
  filter(str_detect(word, "[a-z]")) %>%           # 영어 알파벳 포함된 것만
  filter(nchar(word) >= 2) %>%                    # 한 글자짜리 제거 (원하면)
  count(word, sort = TRUE)                        # 단어 빈도 세기

head(imdb_words)

set.seed(123)


#기본 워드 클라우드 
wordcloud(
  words      = imdb_words$word,
  freq       = imdb_words$n,
  min.freq   = 5,              # 너무 희귀한 단어는 제외 (필요에 따라 조정)
  max.words  = 200,            # 보여줄 단어 개수
  random.order = FALSE,        # 많이 나온 단어를 중앙 쪽에
  rot.per    = 0.1,            # 기울어진 단어 비율
  scale      = c(4, 0.5),      # 글자 크기 범위
  colors     = brewer.pal(8, "Dark2")
)


################################################################################
#평점별 워드클라우드 
################################################################################

library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)

#1️⃣ 단어 빈도 함수

# df: 데이터프레임 (예: imdb_clean_nostop)
# min_rating, max_rating: 별점 조건 (NULL이면 해당 조건 안 씀)
# min_chars: 사용할 최소 글자 수
get_word_freq_by_rating <- function(df,
                                    min_rating = NULL,
                                    max_rating = NULL,
                                    min_chars = 2) {
  data <- df
  
  # 별점 조건 걸기
  if (!is.null(min_rating)) {
    data <- dplyr::filter(data, star_rating >= min_rating)
  }
  if (!is.null(max_rating)) {
    data <- dplyr::filter(data, star_rating <= max_rating)
  }
  
  # 토큰화 + 전처리 + 단어 빈도 계산
  word_freq <- data %>%
    select(star_rating, content_clean_nostop) %>%
    unnest_tokens(word, content_clean_nostop) %>%
    filter(str_detect(word, "[a-z]")) %>%
    filter(nchar(word) >= min_chars) %>%
    count(word, sort = TRUE)
  
  return(word_freq)
}

#2️⃣ 워드클라우드 함수

# freq_tbl: get_word_freq_by_rating() 결과 (word, n 컬럼 필요)
# max_words: 최대 단어 개수
# min_freq: 최소 등장 빈도
# palette: RColorBrewer 팔레트 이름 (예: "Dark2", "Reds")
# scale: 글자 크기 범위
plot_wordcloud_from_freq <- function(freq_tbl,
                                     max_words = 200,
                                     min_freq = 3,
                                     palette = "Dark2",
                                     scale = c(3, 0.5)) {
  if (nrow(freq_tbl) == 0) {
    warning("단어가 없습니다. 별점 조건을 다시 확인하세요.")
    return(invisible(NULL))
  }
  
  top_tbl <- freq_tbl %>%
    slice_max(n, n = max_words)
  
  pal <- brewer.pal(8, palette)
  
  set.seed(123)
  wordcloud(
    words        = top_tbl$word,
    freq         = top_tbl$n,
    min.freq     = min_freq,
    max.words    = max_words,
    random.order = FALSE,
    rot.per      = 0.1,
    scale        = scale,
    colors       = pal
  )
}

#3️⃣ 실행 예시 
# 1) 고평가 리뷰 (예: 8점 이상)
freq_pos <- get_word_freq_by_rating(imdb_clean_nostop,
                                    min_rating = 8,
                                    max_rating = NULL)

plot_wordcloud_from_freq(freq_pos,
                         max_words = 200,
                         min_freq = 3,
                         palette = "Dark2")

# 2) 저평가 리뷰 (예: 3점 이하)
freq_neg <- get_word_freq_by_rating(imdb_clean_nostop,
                                    min_rating = NULL,
                                    max_rating = 3)

plot_wordcloud_from_freq(freq_neg,
                         max_words = 200,
                         min_freq = 3,
                         palette = "Reds")


################################################################################
#단어별 평점 분포
################################################################################

#1️⃣ 자주 등장하는 키워드 리스트 추출 함수

# df: imdb_clean_nostop 같은 데이터
# top_n: 상위 몇 개 단어를 가져올지
# min_chars: 단어 최소 글자 수
# min_freq: 최소 등장 빈도
get_top_keywords <- function(df,
                             top_n   = 100,
                             min_chars = 3,
                             min_freq  = 10) {
  df %>%
    unnest_tokens(word, content_clean_nostop) %>%
    filter(str_detect(word, "[a-z]")) %>%
    filter(nchar(word) >= min_chars) %>%
    count(word, sort = TRUE) %>%
    filter(n >= min_freq) %>%      # 너무 안 나오는 단어는 제거
    slice_max(n, n = top_n)
}

#2️⃣ 특정 키워드에 대해 평점 분포 계산 함수

# keyword: "family", "violence" 같은 하나의 단어
rating_dist_for_keyword <- function(df, keyword) {
  df %>%
    select(id, star_rating, content_clean_nostop) %>%
    unnest_tokens(word, content_clean_nostop) %>%
    filter(word == keyword) %>%                 # 해당 키워드가 등장하는 토큰만
    distinct(id, star_rating) %>%               # 같은 리뷰에 여러 번 나와도 1번만 카운트
    count(star_rating, name = "n") %>%          # 평점별 개수
    arrange(star_rating) %>%
    mutate(prop = n / sum(n))                   # 비율까지
}

#3️⃣ 평점 분포를 시각화하는 함수 (막대 그래프)
library(ggplot2)

plot_rating_dist_for_keyword <- function(df, keyword) {
  dist_tbl <- rating_dist_for_keyword(df, keyword)
  
  if (nrow(dist_tbl) == 0) {
    warning("해당 키워드가 포함된 리뷰가 없습니다.")
    return(invisible(NULL))
  }
  
  ggplot(dist_tbl, aes(x = factor(star_rating),
                       y = n)) +
    geom_col() +
    labs(
      title = paste0("Rating distribution for keyword: '", keyword, "'"),
      x = "Star rating",
      y = "Number of reviews"
    )
}

#4️⃣ 실행 예시

top_keywords_tbl <- get_top_keywords(imdb_clean_nostop,
                                     top_n = 100,
                                     min_chars = 3,
                                     min_freq = 10)

head(top_keywords_tbl)

# Shiny의 choices에 들어갈 벡터
keyword_choices <- top_keywords_tbl$word

dist_family <- rating_dist_for_keyword(imdb_clean_nostop, "boring")
dist_family
plot_rating_dist_for_keyword(imdb_clean_nostop, "boring")
