library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)

# 1) 기본 영어 불용어 불러오기 (tidytext 내장)
data("stop_words")  # stop_words: word, lexicon

movie_stopwords <- c(
  # 영화 자체를 가리키는 말
  "movie", "movies", "film", "films", "cinema",
  "series", "season", "seasons", "episode", "episodes", "parasite",
  
  # 리뷰/시청 관련 표현
  "watch", "watched", "watching", "rewatch", "rewatched",
  "see", "seeing", "saw", "seen",
  "view", "viewed", "viewer", "viewers",
  
  # 작품 메타 정보
  "director", "directors", "directed",
  "actor", "actors", "actress", "actresses",
  "cast", "crew", "screenplay",
  
  # 영화 구성 요소
  "scene", "scenes", "shot", "shots",
  "character", "characters",
  "story", "stories", "plot", "subplot",
  "ending", "endings", "opening",
  
  # 평가용 흔한 단어 (이미 stop_words에 있는 것도 있지만 한 번 더 추가)
  "really", "very", "quite", "lot", "lots",
  
  # 플랫폼/리뷰 사이트
  "imdb", "netflix", "hbo", "disney", "marvel",
  "dvd", "blu", "blu ray", "bluray",
  
  # 스포일러 관련
  "spoiler", "spoilers",
  
  # 기타 자주 나오지만 의미 약한 표현
  "thing", "things", "kind", "sort"
)


# 2) 영화 전용 불용어를 tibble로 만들기
movie_stop_tbl <- tibble(
  word = movie_stopwords
)

# 3) 기존 stop_words + 영화 전용 불용어 합치기
all_stopwords <- stop_words %>%
  bind_rows(movie_stop_tbl) %>%
  distinct(word, .keep_all = TRUE)

# 4) 리뷰 토큰화 + 불용어 제거
imdb_clean_nostop <- imdb_clean %>%
  mutate(content_clean = str_to_lower(content_clean)) %>%   # 소문자 통일
  unnest_tokens(word, content_clean) %>%                    # 단어 단위 토큰화
  anti_join(all_stopwords, by = "word") %>%                 # 불용어 제거
  group_by(id, star_rating, title, date, site) %>%          # 같은 리뷰 단위로 다시 묶기
  summarise(
    content_clean_nostop = paste(word, collapse = " "),     # 단어들을 다시 문장으로
    .groups = "drop"
  )

# imdb_clean_nostop를 output 폴더에 저장
write.csv(
  imdb_clean_nostop,
  file = "output/imdb_clean_nostop.csv",
  row.names = FALSE
)