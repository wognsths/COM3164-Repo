## 데이터 파이프라인 (Shiny 앱에는 불필요)

`output/` 폴더에 이미 샘플 데이터/임베딩이 있으므로 Shiny 실행만 할 때는 이 파이프라인을 돌릴 필요가 없습니다. 새 데이터를 만들고 싶을 때만 아래 순서로 실행하세요. 모든 명령은 리포지토리 루트에서 수행하는 것을 전제로 합니다.

1) 파이썬 의존성 설치  
`pip install -r data_pipeline/requirements.txt`

2) 리뷰 크롤링 (옵션, Selenium/Chrome 필요)  
```
python -m data_pipeline.crawler.main naver --scroll-rounds 50
python -m data_pipeline.crawler.main imdb  --max-reviews 300
```
기본 출력 경로는 `output/`.

3) 한글 리뷰 정규화 (Kiwi)  
```
python data_pipeline/korean_review_process.py \
  --input output/reviews_naver.csv \
  --output output/reviews_naver_normalized.csv
```
`data_pipeline/stopwords.txt` 파일을 만들어두면 추가 불용어를 적용합니다.

4) 텍스트 임베딩 생성 (Upstage API 필요, 네트워크 필요)  
```
export UPSTAGE_API_KEY="YOUR_KEY"
python data_pipeline/build_embeddings.py
```
NPZ 결과는 `output/embeddings/`에 저장됩니다.

5) R에서 사용하기 위해 NPZ → RDS 변환  
```
Rscript data_pipeline/r/convert_embeddings_to_rds.R
```

RDS 파일은 `shiny-app`에서 바로 읽어옵니다.
