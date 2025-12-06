# Convert NPZ embeddings to RDS for use in R.
# Requires: reticulate, numpy (Python), files produced by data_pipeline/build_embeddings.py (run from repo root).

library(reticulate)

np <- import("numpy")

embed_dir <- file.path("output", "embeddings")
naver_npz <- file.path(embed_dir, "naver_embeddings.npz")
imdb_npz  <- file.path(embed_dir, "imdb_embeddings.npz")

if (!file.exists(naver_npz) || !file.exists(imdb_npz)) {
  stop("NPZ files not found. Run data_pipeline/build_embeddings.py first from the repo root.")
}

dir.create(embed_dir, showWarnings = FALSE, recursive = TRUE)

convert_npz <- function(npz_path, rds_path) {
  z <- np$load(npz_path, allow_pickle = TRUE)
  ids <- py_to_r(z$f[["ids"]])
  emb <- py_to_r(z$f[["embeddings"]])
  saveRDS(list(ids = ids, embeddings = emb), rds_path)
  message("Saved: ", rds_path)
}

convert_npz(naver_npz, file.path(embed_dir, "naver_embeddings.rds"))
convert_npz(imdb_npz,  file.path(embed_dir, "imdb_embeddings.rds"))
