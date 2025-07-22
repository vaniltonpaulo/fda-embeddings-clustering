#chapter 9

#─────────────────── Clustering Sparse CD4 Counts Data ──────────────────────── 

#Focus:
#Here we use the CD4 counts data and the face.sparse function. 
#CD4 observations are sparse, which makes direct clustering of the observed data impossible.
#Instead, we are predicting each curve at a grid of observations and use these predicted functions in clustering software. 
#We also obtain the scores, which could be used for clustering, but we use the predicted functions for illustration.

# ─── Packages ───────────────────────────────────────────
library(face)
library(refund)
library(tidyverse)  
library(tidyfun)


# ─── Data ────────────────────────────
data(cd4)


# ─── Clustering helpers ────────────────────────────────
tf_cluster_kmeans <- function(score_mat, K) {
  km <- kmeans(score_mat, centers = K)
  list(cluster = km$cluster, centers = km$centers)
}

tf_cluster_hclust <- function(score_mat, method = "ward.D2", K = 3) {
  hc <- hclust(dist(score_mat), method = method)
  list(cluster = cutree(hc, k = K), hc = hc)
}

# ─── Centres → tf curves  (simplified!) ────────────────
tf_centers_from_scores <- function(centers_mat, arggrid) {
  tfd(centers_mat, arg = arggrid)
}

# ─── tidyfun helpers (join + plot) ─────────────────────
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
      colour = "black", linewidth = 1
    ) +
    geom_spaghetti(
      data = centers_tbl,
      aes(y = curve, colour = cluster, group = cluster),
      linewidth = 1
    ) +
    ## individual subjects
    geom_spaghetti(
      data = cluster_tbl,
      aes(y = curve, colour = cluster, group = id),
      alpha = .15, linewidth = .7
    ) +
    scale_color_manual(
      #values = c("#D55E00", "#009E73", "#F0E442"),
      values = c("darkred", "darkorange", "darkgreen"),
      name   = "Cluster"
    ) +
    labs(
      x = "Time from seroconversion (months)",
      y = "Log CD4 counts"
    ) +
    theme_minimal(base_size = 14)
}


# ─── Preprocess data ────────────────────────────

n <- nrow(cd4)
T <- ncol(cd4)

#we put everything we need in a nice tibble for better format
#Organize tibble as outcome, time, subject ID 
dat <- tibble(
  y       = log(as.vector(t(cd4))),
  argvals = rep(-18:42,  times = n),
  subj    = rep(seq_len(n), each = T)
) |>
  filter(!is.na(y) & y > 4.5)

#Although not the best approach to check , we can say that up to row  333 everything its alrigth
#dat == data

#so far so good

# ─── FACE smoothing & prediction grid ──────────────────
tgrid <- -20:40
#Also took this from the book.Again dont need to reinvent the wheel
fit   <- face.sparse(
  dat,
  argvals.new      = tgrid,
  calculate.scores = TRUE,
  newdata          = dat,
  pve              = 0.95
)

k <- length(tgrid)
Pred_mat <- matrix(NA_real_, n, k)
uid <- unique(dat$subj)

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

# ─── K-means clustering ────────────────────────────────
#we have to keep the same seed number otherwise we get some other values
set.seed(202200228)
km_res <- tf_cluster_kmeans(Pred_mat, K = 3)

##  ─── Build tidyfun objects ──────────────────────
centers_tf  <- tf_centers_from_scores(km_res$centers, tgrid)
centers_tbl <- tibble(cluster = factor(seq_len(nrow(km_res$centers))),
                      curve   = centers_tf)

pred_tf     <- tfd(Pred_mat, arg = tgrid)
cluster_tbl <- tf_join_clusters_tf(pred_tf, km_res$cluster)


# ─── Plot ────────────────────────────────

tf_plot_clusters_tf(cluster_tbl, centers_tbl)
