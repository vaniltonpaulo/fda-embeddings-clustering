# ─── Libraries ───────────────────────────────────────────
library(face)
library(refund)
library(tidyverse)

#Load the data
data(cd4)
n <- nrow(cd4)
T <- ncol(cd4)

#we put everything we need in a nice tibble for better format
#Organize tibble as outcome, time, subject ID 
dat <- tibble(
  y       = log(as.vector(t(cd4))),
  argvals = rep(-20:40,  times = n),
  subj    = rep(seq_len(n), each = T)
) |>
  filter(!is.na(y) & y > 4.5)

head(dat)
# --- wrapper: make FACE look like tf's FPCA "engine" ---
# tfb_fpc(method = <function>) expects the function to accept (data, arg, pve, ...)
# and to return a list with at least: mu, efunctions, scores, npc
# (optionally a scoring_function for new data; we provide one too).
fpca_face_sparse_wrapper <- function(data, arg, pve = 0.99, ...) {
  # 'data' comes in long format with columns: id, arg, value (from tfb_fpc)
  df <- data.frame(
    y       = data$value,
    argvals = data$arg,
    subj    = data$id
  )
  
  # run FACE on a reasonable grid (here: the observed unique time points)
  fit <- face::face.sparse(
    data            = df,
    argvals.new     = sort(unique(df$argvals)),
    calculate.scores = TRUE,   # so we get per-subject FPC scores
    pve             = pve,
    ...
  )
  
  # weights for L2-orthonormality & scoring (trapezoid rule on arg grid)
  arg_new <- fit$argvals.new
  w <- c( diff(arg_new), 0 ); w <- (w + c(0, diff(arg_new)))/2  # trapezoid Δ_i
  
  # scoring function for new data matrices (rows=subjects, cols=arg grid)
  scoring_function <- function(data_matrix, efunctions, mean, weights) {
    w_mat <- matrix(weights, nrow = nrow(data_matrix), ncol = length(weights), byrow = TRUE)
    w_mat[is.na(data_matrix)] <- 0
    data_matrix[is.na(data_matrix)] <- 0
    data_wc <- t((t(data_matrix) - mean) * sqrt(t(w_mat)))
    t(qr.coef(qr(efunctions), t(data_wc) / sqrt(weights)))
  }
  
  list(
    mu         = fit$mu.new,                # mean function on arg_new
    efunctions = fit$eigenfunctions,        # columns = FPCs on arg_new
    scores     = fit$rand_eff$scores,       # per-subject scores (FACE computed)
    npc        = ncol(fit$eigenfunctions),  # number of components
    scoring_function = scoring_function,    # to score new data consistently
    arg        = arg_new,                   # (optional) helpful to keep
    weights    = w                          # (optional) used by scorer
  )
}

# --- run FPCA via tfb_fpc, but using FACE under the hood ---
dat2 <- arrange(dat, subj, argvals)

fpca_tf <- tfb_fpc(
  data   = dat2,
  id     = "subj",
  arg    =  "argvals",
  value  = "y",
  method = fpca_face_sparse_wrapper,  # <-- the key bit
  pve    = .995                       # keep enough components to explain 99% var
)

 fpca_tf
# tfb[366] on (-20,40) in basis representation:
#   using  2 FPCs 
# 1: (-20,7);(-19,7);(-18,7); ...
# 2: (-20,6);(-19,7);(-18,7); ...
# 3: (-20,7);(-19,7);(-18,7); ...
# 4: (-20,7);(-19,7);(-18,7); ...
# 5: (-20,7);(-19,7);(-18,7); ...
# [....]   (361 not shown)
Pred_mat <- as.matrix(fpca_tf)

set.seed(202200228)
cl_kmeans_CD4 <- kmeans(Pred_mat, centers = 3)
cl_ind_CD4 <- cl_kmeans_CD4$cluster
cl_cen_CD4 <- cl_kmeans_CD4$centers



# keep cluster levels consistent between curves & centers
curves_tibble <- tibble(
id = 1:length(fpca_tf), 
cluster = factor(cl_ind_CD4),
curve = fpca_tf)


centers_tibble <- tibble(
cluster = factor(1:nrow(cl_cen_CD4)),
centers = tfd(cl_cen_CD4)
)


cluster_colors <- c("1" = "darkred", "2" = "darkorange", "3" = "darkgreen")

ggplot() +
  # individual curves
  geom_spaghetti(
    data = curves_tibble,
    aes(y = curve, group = id, color = cluster),alpha = .10
  ) +
  geom_spaghetti(
    data = centers_tibble,
    aes(y = centers ,color = cluster),alpha = 10
  )+
  scale_color_manual(values = cluster_colors) +
  labs(x = "Time from seroconversion (months)",
       y = "log CD4 counts") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


# 
# 
# # ───────────── helpers ───────────────────
# tf_cluster_kmeans <- function(score_mat, K) {
#   km <- kmeans(score_mat, centers = K)
#   list(cluster = km$cluster, centers = km$centers)
# }
# 
# tf_cluster_hclust <- function(score_mat, method = "ward.D2", K = 3) {
#   hc <- hclust(dist(score_mat), method = method)
#   list(cluster = cutree(hc, k = K), hc = hc)
# }
# 
# # centres (matrix rows) -> tidyfun curves on a given grid
# tf_centers_from_scores <- function(centers_mat, arggrid) {
#   tfd(centers_mat, arg = arggrid)
# }
# 
# # tidyfun helpers (join + plot)
# tf_join_clusters_tf <- function(pred_tf, cluster_vec) {
#   tibble(
#     id      = seq_along(pred_tf),
#     cluster = factor(cluster_vec),
#     curve   = pred_tf
#   )
# }
# 
# tf_plot_clusters_tf <- function(cluster_tbl, centers_tbl) {
#   ggplot() +
#     ## highlight centres
#     geom_spaghetti(
#       data = centers_tbl,
#       aes(y = curve, group = cluster),
#       colour = "black", linewidth = 1
#     ) +
#     geom_spaghetti(
#       data = centers_tbl,
#       aes(y = curve, colour = cluster, group = cluster),
#       linewidth = 1
#     ) +
#     ## individual subjects
#     geom_spaghetti(
#       data = cluster_tbl,
#       aes(y = curve, colour = cluster, group = id),
#       alpha = .15, linewidth = .7
#     ) +
#     scale_color_manual(
#       values = c("darkred", "darkorange", "darkgreen"),
#       name   = "Cluster"
#     ) +
#     labs(
#       x = "Time from seroconversion (months)",
#       y = "Log CD4 counts"
#     ) +
#     theme_minimal(base_size = 14)
# }