```
# repo 루트에서 실행
python -m data_pipeline.crawler.main naver --scroll-rounds 50
python -m data_pipeline.crawler.main imdb  --max-reviews 300
```

파이썬 의존성 설치 (가상환경 추천):
```
pip install -r data_pipeline/requirements.txt
```
테스트한 파이썬 버전: 3.11
