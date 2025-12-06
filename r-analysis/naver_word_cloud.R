library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(showtext)
library(ggplot2)
library(scales)

# 데이터 불러오기 -------------------------------------------------------------
data_path <- file.path("output", "reviews_naver_merged.csv")

naver_reviews <- readr::read_csv(
  data_path,
  locale = readr::locale(encoding = "UTF-8")
) %>%
  mutate(star_rating = as.numeric(star_rating))

# OS별 기본 한글 폰트 추정
default_korean_font <- function() {
  switch(tolower(Sys.info()[["sysname"]]),
         darwin  = "AppleGothic",
         windows = "Malgun Gothic",
         "NanumGothic")
}

# 한국어 스탑워드 (영화 리뷰용)
stopwords_kr <- c(
  "영화", "보다", "있다", "없다", "너무", "같다"
)

enable_showtext_nanum <- function(font_family = "nanumgothic") {
  if (!font_family %in% sysfonts::font_families()) {
    try(
      showtext::font_add_google(name = "Nanum Gothic", family = font_family),
      silent = TRUE
    )
  }

  if (font_family %in% sysfonts::font_families()) {
    showtext::showtext_auto()
    return(list(font_family = font_family, use_showtext = TRUE))
  }

  warning("구글 나눔고딕 로드 실패. 시스템 기본 폰트로 대체합니다.")
  list(font_family = default_korean_font(), use_showtext = FALSE)
}

################################################################################
# 단어 빈도 테이블 만들기 (평점 조건 가능)
################################################################################
get_word_freq_by_rating <- function(df,
                                    min_rating = NULL,
                                    max_rating = NULL,
                                    min_chars = 2,
                                    keep_korean_only = TRUE,
                                    stopwords = stopwords_kr) {
  data <- df

  if (!is.null(min_rating)) {
    data <- dplyr::filter(data, star_rating >= min_rating)
  }
  if (!is.null(max_rating)) {
    data <- dplyr::filter(data, star_rating <= max_rating)
  }

  tokens <- data %>%
    select(star_rating, normalized_text) %>%
    filter(!is.na(normalized_text)) %>%
    unnest_tokens(word, normalized_text, token = "words") %>%
    filter(nchar(word) >= min_chars)

  if (keep_korean_only) {
    tokens <- tokens %>%
      filter(str_detect(word, "[가-힣]"))
  }

  if (!is.null(stopwords) && length(stopwords) > 0) {
    tokens <- tokens %>%
      filter(!word %in% stopwords)
  }

  tokens %>%
    count(word, sort = TRUE)
}

################################################################################
# 워드클라우드 그리기
################################################################################
plot_wordcloud_from_freq <- function(freq_tbl,
                                     max_words = 200,
                                     min_freq = 3,
                                     palette = "Dark2",
                                     scale = c(4, 0.5),
                                     rot_per = 0.1,
                                     font_family = NULL,
                                     use_showtext = TRUE) {
  if (nrow(freq_tbl) == 0) {
    warning("단어가 없습니다. 평점 조건을 확인하세요.")
    return(invisible(NULL))
  }

  top_tbl <- freq_tbl %>%
    slice_max(n, n = max_words)

  pal <- tryCatch(
    brewer.pal(8, palette),
    error = function(e) {
      warning("팔레트 '", palette, "'를 찾을 수 없습니다. 'Dark2'로 대체합니다.")
      brewer.pal(8, "Dark2")
    }
  )

  if (use_showtext) {
    res <- enable_showtext_nanum(ifelse(is.null(font_family), "nanumgothic", font_family))
    font_family <- res$font_family
    use_showtext <- res$use_showtext
  } else if (is.null(font_family)) {
    font_family <- default_korean_font()
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(family = font_family)

  set.seed(123)
  wordcloud(
    words        = top_tbl$word,
    freq         = top_tbl$n,
    min.freq     = min_freq,
    max.words    = max_words,
    random.order = FALSE,
    rot.per      = rot_per,
    scale        = scale,
    colors       = pal,
    family       = font_family
  )
}

################################################################################
# 파일로 저장 (고해상도 PNG)
################################################################################
save_wordcloud_png <- function(freq_tbl,
                               file,
                               width = 2000,   # px
                               height = 2000,  # px
                               res = 300,
                               bg = "white",
                               ...) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename   = file,
      width      = width,
      height     = height,
      units      = "px",
      background = bg,
      res        = res
    )
  } else {
    png(
      filename = file,
      width    = width,
      height   = height,
      units    = "px",
      res      = res,
      bg       = bg,
      type     = "cairo"
    )
  }

  on.exit(dev.off(), add = TRUE)
  plot_wordcloud_from_freq(freq_tbl, ...)
  invisible(file)
}

################################################################################
# 키워드별 평점 분포
################################################################################
rating_dist_for_keyword <- function(df,
                                    keyword,
                                    min_chars = 1,
                                    stopwords = stopwords_kr) {
  tokens <- df %>%
    select(id, star_rating, normalized_text) %>%
    filter(!is.na(normalized_text)) %>%
    unnest_tokens(word, normalized_text, token = "words") %>%
    filter(nchar(word) >= min_chars)

  if (!is.null(stopwords) && length(stopwords) > 0) {
    tokens <- tokens %>% filter(!word %in% stopwords)
  }

  tokens %>%
    filter(word == keyword) %>%
    distinct(id, star_rating) %>%
    count(star_rating, name = "n") %>%
    arrange(star_rating) %>%
    mutate(prop = n / sum(n))
}

plot_rating_dist_for_keyword <- function(df,
                                         keyword,
                                         min_chars = 1,
                                         stopwords = stopwords_kr) {
  dist_tbl <- rating_dist_for_keyword(df, keyword, min_chars, stopwords)

  if (nrow(dist_tbl) == 0) {
    warning("해당 키워드가 포함된 리뷰가 없습니다: ", keyword)
    return(invisible(NULL))
  }

  ggplot(dist_tbl, aes(x = factor(star_rating), y = prop)) +
    geom_col(fill = "#2E86DE") +
    labs(
      title = paste0("Rating distribution for keyword: '", keyword, "'"),
      x = "Star rating",
      y = "Proportion"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    theme_minimal()
}

################################################################################
# 실행 예시 (env var RUN_NAVER_WORDCLOUD_EXAMPLE=1 로 켜기)
################################################################################
if (interactive() && identical(Sys.getenv("RUN_NAVER_WORDCLOUD_EXAMPLE"), "1")) {
  # 1) 고평점 리뷰 (예: 9점 이상)
  freq_pos <- get_word_freq_by_rating(
    naver_reviews,
    min_rating = 9,
    max_rating = NULL
  )

  plot_wordcloud_from_freq(
    freq_pos,
    max_words = 200,
    min_freq = 3,
    palette = "Dark2",
    font_family = default_korean_font(),
    use_showtext = TRUE
  )

  # 2) 저평점 리뷰 (예: 3점 이하)
  freq_neg <- get_word_freq_by_rating(
    naver_reviews,
    min_rating = NULL,
    max_rating = 3
  )

  plot_wordcloud_from_freq(
    freq_neg,
    max_words = 200,
    min_freq = 3,
    palette = "Reds",
    font_family = default_korean_font(),
    use_showtext = TRUE
  )
}

