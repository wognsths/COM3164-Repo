# COM3164 Review Analysis

영화 리뷰 데이터를 Shiny로 살펴보기 위한 리포지토리입니다. 현재 `shiny-app/`은 평점 분포/추이, 워드클라우드, 키워드별 평점 분포를 한 페이지에서 보여주는 형태이며, 파이썬 코드는 크롤링/전처리용으로만 남겨두었습니다.

## 바로 실행 (Shiny)
- R 패키지: `install.packages(c("shiny", "dplyr", "stringr", "readr", "tidytext", "wordcloud", "RColorBrewer", "lubridate", "scales", "showtext", "tidyr"))`
- 리포지토리 루트에서 실행  
`R -e "shiny::runApp('shiny-app')"`  
앱은 `output/`에 이미 들어있는 CSV/RDS 파일을 사용합니다.

## 디렉터리 개요
- `shiny-app/` — 평점·단어 분석 Shiny 앱 (`app.R`).
- `r-analysis/` — 워드클라우드, 평점 비교 등 R 실험 스크립트 모음.
- `data_pipeline/` — 크롤러, Kiwi 전처리, 임베딩 생성/NPZ→RDS 변환 등 파이프라인 코드(Shiny 실행에는 불필요). 자세한 사용법은 `data_pipeline/README.md`.
- `output/` — 미리 크롤링/전처리된 CSV와 임베딩(RDS/NPZ). Shiny 앱은 CSV만 사용합니다.

필요한 추가 작업이나 데이터 생성은 `data_pipeline/README.md`를 참고해 주세요.
