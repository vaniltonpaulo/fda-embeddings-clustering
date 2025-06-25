############################################################
## Clustering Sparse CD4 Counts Data ##
############################################################

## 0 ─── Packages ───────────────────────────────────────────
library(face)
library(refund)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(tf)
library(tidyfun)

## 1 ─── Clustering helpers ────────────────────────────────
tf_cluster_kmeans <- function(score_mat, K) {
  km <- kmeans(score_mat, centers = K)
  list(cluster = km$cluster, centers = km$centers)
}

tf_cluster_hclust <- function(score_mat, method = "ward.D2", K = 3) {
  hc <- hclust(dist(score_mat), method = method)
  list(cluster = cutree(hc, k = K), hc = hc)
}

## 2 ─── Centres → tf curves  (simplified!) ────────────────
tf_centers_from_scores <- function(centers_mat, arggrid) {
  tfd(centers_mat, arg = arggrid)
}

## 3 ─── tidyfun helpers (join + plot) ─────────────────────
tf_join_clusters_tf <- function(pred_tf, cluster_vec) {
  tibble(
    id      = seq_along(pred_tf),
    cluster = factor(cluster_vec),
    curve   = pred_tf
  )
}

tf_plot_clusters_tf <- function(cluster_tbl, centers_tbl) {
  ggplot() +
    ## highlight centres
    geom_spaghetti(
      data = centers_tbl,
      aes(y = curve, group = cluster),
      colour = "black", linewidth = 3.2#, lineend = "round"
    ) +
    geom_spaghetti(
      data = centers_tbl,
      aes(y = curve, colour = cluster, group = cluster),
      linewidth = 2.4#, lineend = "round"
    ) +
    ## individual subjects
    geom_spaghetti(
      data = cluster_tbl,
      aes(y = curve, colour = cluster, group = id),
      alpha = .30, linewidth = .6
    ) +
    scale_color_manual(
      values = c("#D55E00", "#009E73", "#F0E442"),
      name   = "Cluster"
    ) +
    labs(
      x = "Time from seroconversion (months)",
      y = "Log CD4 counts"
    ) +
    theme_minimal(base_size = 14)
}

## 4 ─── Load & preprocess data ────────────────────────────
data(cd4)
n <- nrow(cd4); T <- ncol(cd4)

dat <- tibble(
  y       = log(as.vector(t(cd4))),
  argvals = rep(-18:42,  times = n),
  subj    = rep(seq_len(n), each = T)
) |>
  filter(!is.na(y) & y > 4.5)

## 5 ─── FACE smoothing & prediction grid ──────────────────
tgrid <- -20:40
fit   <- face.sparse(
  dat,
  argvals.new      = tgrid,
  calculate.scores = TRUE,
  newdata          = dat,
  pve              = 0.95
)

k        <- length(tgrid)
Pred_mat <- matrix(NA_real_, n, k)
uid      <- unique(dat$subj)

for (i in seq_len(n)) {
  dat_i <- filter(dat, subj == uid[i])
  
  new_i <- tibble(
    y       = c(dat_i$y,       rep(NA_real_, k)),
    argvals = c(dat_i$argvals, tgrid),
    subj    = c(dat_i$subj,    rep(uid[i], k))
  )
  
  yhat  <- predict(fit, new_i)$y.pred
  Pred_mat[i, ] <- tail(yhat, k)
}

## 6 ─── K-means clustering ────────────────────────────────
set.seed(202200228)
km_res <- tf_cluster_kmeans(Pred_mat, K = 3)

## 7 ─── Build tidyfun objects & plot ──────────────────────
centers_tf  <- tf_centers_from_scores(km_res$centers, tgrid)
centers_tbl <- tibble(cluster = factor(seq_len(nrow(km_res$centers))),
                      curve   = centers_tf)

pred_tf     <- tfd(Pred_mat, arg = tgrid)
cluster_tbl <- tf_join_clusters_tf(pred_tf, km_res$cluster)

tf_plot_clusters_tf(cluster_tbl, centers_tbl)
