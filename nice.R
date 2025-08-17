# ──────────────────────────────────────────────────────────────────────────────
# Clustering sparse CD4 curves on a common grid using FACE (via wrapper)
# ──────────────────────────────────────────────────────────────────────────────

# ─── Libraries ────────────────────────────────────────────────────────────────
library(face)
library(refund)
library(tidyverse)
library(tidyfun)

# (optional rendering prefs)
options(pillar.sigfig = 10)
options(tfb_signif   = 15)

# ─── Data ─────────────────────────────────────────────────────────────────────
data(cd4)

# ─── Helper functions (clustering + plotting) ─────────────────────────────────
tf_cluster_kmeans <- function(score_mat, K) {
  km <- kmeans(score_mat, centers = K)
  list(cluster = km$cluster, centers = km$centers)
}

tf_cluster_hclust <- function(score_mat, method = "ward.D2", K = 3) {
  hc <- hclust(dist(score_mat), method = method)
  list(cluster = cutree(hc, k = K), hc = hc)
}

# centres (matrix rows) -> tidyfun curves on a given grid
tf_centers_from_scores <- function(centers_mat, arggrid) {
  tfd(centers_mat, arg = arggrid)
}

# tidyfun helpers (join + plot)
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
      values = c("darkred", "darkorange", "darkgreen"),
      name   = "Cluster"
    ) +
    labs(
      x = "Time from seroconversion (months)",
      y = "Log CD4 counts"
    ) +
    theme_minimal(base_size = 14)
}

# ─── Preprocess: long tibble & tidyfun object ────────────────────────────────
#Load the data
data(cd4)
n <- nrow(cd4)
T <- ncol(cd4)

dat <- data.frame(
  y       = log(as.vector(t(cd4))),
  argvals = rep(-18:42,  times = n),
  subj    = rep(seq_len(n), each = T)
) |>
  filter(!is.na(y) & y > 4.5) |>
  arrange(subj, argvals)  # ensure stable order

# Build tidyfun object (one function per subject)
cd4_tf <- tfd(dat, id = "subj", arg = "argvals", value = "y")

# ─── FACE wrapper (fixed to honor grid and return FPCA pieces) ────────────────
face_sparse_wrapper <- function(data, arg, pve = 0.95, ...) {
  df <- as.data.frame(data, unnest = TRUE) |>
    rename(y = value, argvals = arg, subj = id) |>
    filter(!is.na(y)) |>
    arrange(subj, argvals)
  
  fit <- face.sparse(
    data             = df,
    newdata          = df,     # used to estimate subject-specific scores
    argvals.new      = arg,    # <-- IMPORTANT: force predictions to this grid
    pve              = pve,
    calculate.scores = TRUE,
    ...
  )
  
  list(
    mu          = as.numeric(fit$mu.new),              # length = length(arg)
    efunctions  = as.matrix(fit$eigenfunctions),       # k x npc on 'arg'
    scores      = as.matrix(fit$rand_eff$scores),      # n x npc, subj order
    npc         = ncol(fit$eigenfunctions),
    arg         = arg
  )
}

# ─── Fit FACE & build predicted functions on the common grid ─────────────────
arg <- -20:40  # fixed grid used throughout
fpca_res <- face_sparse_wrapper(cd4_tf, arg = arg)
# Predicted functions for each subject on 'arg':
# Pred = mu + scores %*% t(efunctions)
# scores: n x npc, efunctions: k x npc -> t(efunctions): npc x k
Pred_mat <- fpca_res$scores %*% t(fpca_res$efunctions)
kred_mat <- sweep(Pred_mat, 2, fpca_res$mu, `+`)  # add mean per grid point
# Tidyfun object of predicted curves
#pred_tf <- tfd(Pred_mat, arg = arg)
pred_tf <-tfb_fpc(kred_mat,arg)
pred_tf
as.matrix(pred_tf)
# ─── K-means on predicted functions (matches original pipeline) ──────────────
set.seed(202200228)
km_res <- tf_cluster_kmeans(kred_mat, K = 3)

centers_tf  <- tf_centers_from_scores(km_res$centers, arg)
centers_tbl <- tibble(cluster = factor(seq_len(nrow(km_res$centers))),
                      curve   = centers_tf)
cluster_tbl <- tf_join_clusters_tf(pred_tf, km_res$cluster)
tf_plot_clusters_tf(cluster_tbl, centers_tbl)





