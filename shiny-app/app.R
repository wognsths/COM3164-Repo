# IMDB / Naver rating + text explorer (no embeddings)
# - Platform toggle: both / Naver only / IMDB only
# - Date filter + rating histogram/trend + summary stats
# - Rating-conditioned word clouds and keyword rating distribution

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(showtext)

# -------------------------------------------------------------------
# Data load
# -------------------------------------------------------------------
data_dir <- file.path("..", "output")  # runApp("shiny-app") assumes cwd = shiny-app

imdb_reviews <- read_csv(
  file.path(data_dir, "reviews_imdb.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    id,
    platform = "imdb",
    date = as.Date(date),
    rating = as.numeric(star_rating),
    text = content
  )

naver_reviews <- read_csv(
  file.path(data_dir, "reviews_naver_merged.csv"),
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  transmute(
    id,
    platform = "naver",
    date = as.Date(writing_date),
    rating = as.numeric(star_rating),
    text = comment,
    upvote,
    downvote,
    normalized_text
  )

# Tokenized IMDB text (stopwords already removed)
imdb_tokens <- read_csv(
  file.path(data_dir, "imdb_clean_nostop.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    id,
    star_rating = as.numeric(star_rating),
    date = as.Date(date),
    content_clean_nostop
  )

# Combined reviews for rating/time plots
reviews_all <- bind_rows(imdb_reviews, naver_reviews)

date_min <- min(reviews_all$date, na.rm = TRUE)
date_max <- max(reviews_all$date, na.rm = TRUE)

# -------------------------------------------------------------------
# Helpers (filtering, summaries, plots)
# -------------------------------------------------------------------
filter_by_date_platform <- function(data, date_range, platform_sel) {
  df <- data

  if (!is.null(date_range[1]) && !is.na(date_range[1])) {
    df <- df %>% filter(date >= as.Date(date_range[1]))
  }
  if (!is.null(date_range[2]) && !is.na(date_range[2])) {
    df <- df %>% filter(date <= as.Date(date_range[2]))
  }
  if (platform_sel != "both") {
    df <- df %>% filter(platform == platform_sel)
  }
  df
}

plot_rating_hist <- function(data, platform_sel) {
  data <- data %>%
    filter(!is.na(rating)) %>%
    mutate(rating = factor(rating, levels = 1:10))

  df <- data %>%
    group_by(platform, rating) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()

  position_val <- if (platform_sel == "both") "dodge" else "stack"

  ggplot(df, aes(x = rating, y = prop, fill = platform)) +
    geom_col(position = position_val, color = "white", alpha = 0.9) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Rating distribution (share)",
      x = "Rating",
      y = "Share",
      fill = "Platform"
    ) +
    theme_minimal()
}

summarise_ratings <- function(data) {
  data %>%
    group_by(platform) %>%
    summarise(
      n        = n(),
      mean     = mean(rating, na.rm = TRUE),
      sd       = sd(rating, na.rm = TRUE),
      median   = median(rating, na.rm = TRUE),
      p25      = quantile(rating, 0.25, na.rm = TRUE),
      p75      = quantile(rating, 0.75, na.rm = TRUE),
      prop_10  = mean(rating == 10, na.rm = TRUE),
      prop_7_10 = mean(rating >= 7, na.rm = TRUE),
      prop_1_3  = mean(rating <= 3, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_rating_trend <- function(data, time_unit = c("month", "week", "day"), min_n = 5) {
  time_unit <- match.arg(time_unit)

  if (nrow(data) == 0) return(data.frame())

  data %>%
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
      n           = n(),
      .groups     = "drop"
    ) %>%
    filter(n >= min_n)
}

plot_rating_trend <- function(data, time_unit = c("month", "week", "day"), min_n = 5) {
  trend_df <- summarise_rating_trend(data, time_unit, min_n)
  if (nrow(trend_df) == 0) return(NULL)

  range_period  <- range(trend_df$period)
  span_days     <- as.numeric(diff(range_period))
  span_months   <- span_days / 30.4

  date_breaks <- dplyr::case_when(
    span_months <= 6   ~ "1 month",
    span_months <= 18  ~ "3 months",
    span_months <= 36  ~ "6 months",
    TRUE               ~ "1 year"
  )

  ggplot(trend_df, aes(x = period, y = mean_rating, color = platform)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
    scale_x_date(date_labels = "%Y.%m", date_breaks = date_breaks) +
    labs(
      title = "Mean rating over time",
      x     = "Time",
      y     = "Mean rating",
      color = "Platform"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -------------------------------------------------------------------
# Wordcloud/keyword helpers
# -------------------------------------------------------------------
get_word_freq_imdb <- function(df,
                               min_rating = 1,
                               max_rating = 10,
                               min_chars = 2) {
  data <- df %>%
    filter(
      (is.na(min_rating) | star_rating >= min_rating),
      (is.na(max_rating) | star_rating <= max_rating)
    )

  data %>%
    select(star_rating, content_clean_nostop) %>%
    unnest_tokens(word, content_clean_nostop) %>%
    filter(str_detect(word, "[a-z]")) %>%
    filter(nchar(word) >= min_chars) %>%
    count(word, sort = TRUE)
}

get_word_freq_naver <- function(df,
                                min_rating = 1,
                                max_rating = 10,
                                min_chars = 2,
                                stopwords = c("영화", "보다", "있다", "없다", "너무", "같다")) {
  data <- df %>%
    filter(
      (is.na(min_rating) | rating >= min_rating),
      (is.na(max_rating) | rating <= max_rating)
    )

  tokens <- data %>%
    select(rating, normalized_text) %>%
    filter(!is.na(normalized_text)) %>%
    unnest_tokens(word, normalized_text, token = "words") %>%
    filter(nchar(word) >= min_chars) %>%
    filter(str_detect(word, "[가-힣]"))

  if (!is.null(stopwords) && length(stopwords) > 0) {
    tokens <- tokens %>% filter(!word %in% stopwords)
  }

  tokens %>% count(word, sort = TRUE)
}

plot_wordcloud_from_freq <- function(freq_tbl,
                                     max_words = 200,
                                     min_freq = 3,
                                     palette = "Dark2",
                                     scale = c(3.5, 0.5),
                                     title = NULL,
                                     font_family = NULL) {
  if (nrow(freq_tbl) == 0) {
    plot.new()
    text(0.5, 0.5, "No words after filtering.")
    return(invisible(NULL))
  }

  top_tbl <- freq_tbl %>% slice_max(n, n = max_words)
  pal <- tryCatch(brewer.pal(8, palette), error = function(e) brewer.pal(8, "Dark2"))

  if (!is.null(font_family)) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(family = font_family)
  }

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
  if (!is.null(title)) {
    title(main = title, line = -1)
  }
}

get_top_keywords_naver <- function(top_n = 100) {
  get_word_freq_naver(naver_reviews, min_rating = 1, max_rating = 10, min_chars = 2) %>%
    filter(n >= 5) %>%
    slice_max(n, n = top_n) %>%
    pull(word)
}

get_top_keywords_imdb <- function(top_n = 100) {
  get_word_freq_imdb(imdb_tokens, min_rating = 1, max_rating = 10, min_chars = 3) %>%
    filter(n >= 5) %>%
    slice_max(n, n = top_n) %>%
    pull(word)
}

rating_dist_for_platform_keyword <- function(platform, keyword) {
  if (platform == "imdb") {
    imdb_tokens %>%
      select(id, rating = star_rating, text = content_clean_nostop) %>%
      unnest_tokens(word, text) %>%
      filter(word == keyword) %>%
      distinct(id, rating) %>%
      mutate(platform = "imdb", rating = as.integer(rating))
  } else {
    naver_reviews %>%
      select(id, rating, text = normalized_text) %>%
      filter(!is.na(text)) %>%
      unnest_tokens(word, text, token = "words") %>%
      filter(word == keyword) %>%
      distinct(id, rating) %>%
      mutate(platform = "naver", rating = as.integer(rating))
  }
}

rating_dist_for_keyword_combined <- function(keyword_naver, keyword_imdb) {
  imdb_tbl <- rating_dist_for_platform_keyword("imdb", keyword_imdb)
  naver_tbl <- rating_dist_for_platform_keyword("naver", keyword_naver)

  bind_rows(imdb_tbl, naver_tbl) %>%
    filter(!is.na(rating)) %>%
    mutate(
      platform = factor(platform, levels = c("naver", "imdb")),
      rating   = factor(rating, levels = 1:10)
    ) %>%
    count(platform, rating, name = "n") %>%
    tidyr::complete(
      platform = factor(c("naver", "imdb"), levels = c("naver", "imdb")),
      rating   = factor(1:10, levels = 1:10),
      fill     = list(n = 0)
    ) %>%
    group_by(platform) %>%
    mutate(prop = if (sum(n) > 0) n / sum(n) else rep(0, n())) %>%
    ungroup()
}

plot_keyword_distribution <- function(dist_tbl, keyword) {
  if (nrow(dist_tbl) == 0) {
    ggplot() + annotate("text", x = 0, y = 0, label = "No reviews contain this keyword.") +
      theme_void()
  } else {
    rating_levels <- as.character(1:10)
    dist_tbl <- dist_tbl %>%
      mutate(
        rating   = factor(rating, levels = rating_levels),
        platform = factor(platform, levels = c("naver", "imdb"))
      )

    ggplot(dist_tbl, aes(x = platform, y = prop, fill = rating)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
      scale_fill_manual(values = rev(RColorBrewer::brewer.pal(10, "Spectral"))) +
      labs(
        title = paste0("Rating distribution for keyword '", keyword, "'"),
        x = "Platform",
        y = "Share",
        fill = "Rating"
      ) +
      coord_flip() +
      theme_minimal()
  }
}

enable_showtext_safe <- function() {
  # Ensure a CJK-capable font is present; try loading Noto Sans KR from Google if missing.
  if (!"Noto Sans KR" %in% sysfonts::font_families()) {
    try(sysfonts::font_add_google("Noto Sans KR", "Noto Sans KR"), silent = TRUE)
  }
  showtext::showtext_auto(enable = TRUE)
}

default_multilang_font <- function() {
  # Prefer Noto Sans; fall back to common CJK fonts; else sans.
  candidates <- c(
    "Noto Sans KR",
    "Noto Sans CJK KR",
    "Noto Sans CJK JP",
    "Noto Sans CJK SC",
    "Noto Sans JP",
    "Noto Sans",
    "Apple SD Gothic Neo",
    "AppleGothic",
    "NanumGothic",
    "Malgun Gothic",
    "Arial Unicode MS",
    "sans"
  )
  available <- sysfonts::font_families()
  match <- candidates[candidates %in% available][1]
  if (!is.na(match) && length(match)) match else "sans"
}

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
ui <- navbarPage(
  "IMDB / Naver Review Dashboard",
  tabPanel(
    "Ratings & Trend",
    sidebarLayout(
      sidebarPanel(
        selectInput("platform", "Platform", choices = c("both", "naver", "imdb"), selected = "both"),
        dateRangeInput(
          "date_range", "Date range",
          start = date_min, end = date_max,
          min = date_min, max = date_max
        ),
        selectInput("time_unit", "Trend time unit", choices = c("month", "week", "day"), selected = "month")
      ),
      mainPanel(
        h3("Rating distribution (histogram)"),
        plotOutput("rating_hist", height = 300),
        h3("Rating trend"),
        plotOutput("rating_trend", height = 320),
        h3("Summary statistics"),
        tableOutput("rating_summary")
      )
    )
  ),
  tabPanel(
    "Wordcloud & Keywords",
    sidebarLayout(
      sidebarPanel(
      sliderInput("wc_rating", "Wordcloud rating filter", min = 1, max = 10, value = c(1, 10), step = 1),
      sliderInput("wc_max_words", "Wordcloud max words", min = 50, max = 300, value = 150, step = 25),
      sliderInput("wc_min_freq", "Wordcloud min frequency", min = 1, max = 20, value = 3, step = 1),
      selectizeInput("keyword_choice_naver", "NAVER keyword", choices = character(0), options = list(create = TRUE)),
      selectizeInput("keyword_choice_imdb", "IMDB keyword", choices = character(0), options = list(create = TRUE))
    ),
    mainPanel(
      h3("Wordclouds (rating filter applied)"),
      fluidRow(
        column(
            width = 6,
            strong("NAVER"),
            plotOutput("wordcloud_naver", height = 360)
          ),
          column(
            width = 6,
            strong("IMDB"),
            plotOutput("wordcloud_imdb", height = 360)
          )
        ),
        h3("Rating distribution by keyword"),
        plotOutput("keyword_dist", height = 340)
      )
    )
  )
)

# -------------------------------------------------------------------
# Server
# -------------------------------------------------------------------
server <- function(input, output, session) {
  filtered_reviews <- reactive({
    filter_by_date_platform(reviews_all, input$date_range, input$platform) %>%
      filter(!is.na(rating))
  })

  observe({
    nav_keys  <- get_top_keywords_naver()
    imdb_keys <- get_top_keywords_imdb()

    updateSelectizeInput(
      session, "keyword_choice_naver",
      choices  = nav_keys,
      selected = if (length(nav_keys)) nav_keys[[1]] else character(0),
      server   = TRUE
    )
    updateSelectizeInput(
      session, "keyword_choice_imdb",
      choices  = imdb_keys,
      selected = if (length(imdb_keys)) imdb_keys[[1]] else character(0),
      server   = TRUE
    )
  })

  output$rating_hist <- renderPlot({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    plot_rating_hist(df, input$platform)
  })

  output$rating_trend <- renderPlot({
    df <- filtered_reviews()
    req(nrow(df) > 0)
    plot_rating_trend(df, input$time_unit, min_n = 3)
  })

  output$rating_summary <- renderTable({
    df <- filtered_reviews()
    if (nrow(df) == 0) return(tibble())
    summarise_ratings(df)
  })

  output$wordcloud_naver <- renderPlot({
    rating_range <- input$wc_rating
    enable_showtext_safe()
    font_family <- default_multilang_font()
    freq_naver <- get_word_freq_naver(
      naver_reviews,
      min_rating = rating_range[1],
      max_rating = rating_range[2]
    )
    if (nrow(freq_naver) == 0) {
      plot.new()
      text(0.5, 0.5, "No Naver words after filtering.")
    } else {
      plot_wordcloud_from_freq(
        freq_naver,
        max_words = input$wc_max_words,
        min_freq = input$wc_min_freq,
        title = NULL,
        font_family = font_family
      )
    }
  })

  output$wordcloud_imdb <- renderPlot({
    rating_range <- input$wc_rating
    enable_showtext_safe()
    font_family <- default_multilang_font()
    freq_imdb <- get_word_freq_imdb(
      imdb_tokens,
      min_rating = rating_range[1],
      max_rating = rating_range[2]
    )
    if (nrow(freq_imdb) == 0) {
      plot.new()
      text(0.5, 0.5, "No IMDB words after filtering.")
    } else {
      plot_wordcloud_from_freq(
        freq_imdb,
        max_words = input$wc_max_words,
        min_freq = input$wc_min_freq,
        title = NULL,
        font_family = font_family
      )
    }
  })

  output$keyword_dist <- renderPlot({
    keyword_nav  <- input$keyword_choice_naver
    keyword_imdb <- input$keyword_choice_imdb
    req(keyword_nav, keyword_imdb)

    dist_tbl <- rating_dist_for_keyword_combined(keyword_nav, keyword_imdb)
    plot_keyword_distribution(dist_tbl, paste0("NAVER: '", keyword_nav, "' / IMDB: '", keyword_imdb, "'"))
  })
}

shinyApp(ui, server)
