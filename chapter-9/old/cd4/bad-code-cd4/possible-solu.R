# ─── Libraries ───────────────────────────────────────────
library(face)
library(refund)
library(tidyverse)
library(tidyfun)

# ─── Data ────────────────────────────────────────────────
options(pillar.sigfig = 10)
options(tfb_signif = 15)


data(cd4)
n <- nrow(cd4)
T <- ncol(cd4)

dat <- tibble(
  y       = log(as.vector(t(cd4))),
  argvals = rep(-20:40, times = n),
  subj    = rep(seq_len(n), each = T)
) %>%
  filter(!is.na(y) & y > 4.5)

head(dat)

# ─── Convert to tf object ────────────────────────────────
cd4_tf <- tfd(dat, id = "subj", arg = "argvals", value = "y")
cd4_tf
# ─── face_sparse_wrapper as you had it ───────────────────
face_scoring_function <- function(data_matrix, efunctions, mean, weights = NULL) {
  sweep(data_matrix, 2, mean) %*% efunctions
}

face_sparse_wrapper <- function(data, arg, pve = 0.95, ...) {
  df <- as.data.frame(data, unnest = TRUE) %>%
    rename(y = value, argvals = arg, subj = id) %>%
    filter(!is.na(y))
  arg_grid <- sort(unique(df$argvals))
  fit <- face.sparse(
    data             = df,
    newdata          = df,
    argvals.new      = arg_grid,
    pve              = pve,
    calculate.scores = TRUE,
    ...
  )
  list(
    mu               = as.numeric(fit$mu.new),
    efunctions       = as.matrix(fit$eigenfunctions),
    scores           = as.matrix(fit$rand_eff$scores),
    npc              = ncol(fit$eigenfunctions),
    scoring_function = face_scoring_function
  )
}

# ─── Get FPCA scores & components ───────────────
fpca_res <- face_sparse_wrapper(cd4_tf, arg = -20:40, pve = 0.95)
fpca_res
# ─── Reconstruct predicted curves (for plotting) ─
cd4_fpca <- tfb_fpc(cd4_tf, method = face_sparse_wrapper, pve = .95,signif = 15)
cd4_fpca
print(cd4_fpca, signif = 4)

# ───  K-means on the scores ──────────────────────
set.seed(202200228)
km_res <- kmeans(fpca_res$scores, centers = 3)

# ─── Reconstruct the function for each cluster‐center ─
mu_grid   <- fpca_res$mu                   
psi_mat   <- fpca_res$efunctions           
#arg_grid  <- seq(-20, 40, by = 1)
arg_grid <- sort(unique(dat$argvals))

# for each center k, f_k(t) = mu(t) + sum_ℓ ψ_ℓ(t) * score[k,ℓ]
centers_mat <- t(
  sapply(seq_len(nrow(km_res$centers)), function(k) {
    mu_grid + psi_mat %*% km_res$centers[k, ]
  })
)  # result: 3 × 51

centers_tf <- tfd(centers_mat, arg = arg_grid,signif = 4)
centers_tf
# ─── Build your tibbles for plotting ────────────────────
centers_tbl <- tibble(
  cluster = factor(seq_len(nrow(km_res$centers))),
  curve   = centers_tf
)

cluster_tbl <- tibble(
  id      = seq_along(cd4_fpca),
  cluster = factor(km_res$cluster),
  curve   = cd4_fpca
)

as.matrix(centers_tbl$curve)
# ─── STEP 5: Plot ───────────────────────────────────────
tf_plot_clusters_tf(cluster_tbl, centers_tbl) +
  #scale_color_brewer("Cluster", palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(
    title = "CD4 Trajectories clustered via sparse FPCA + k-means",
    x     = "Time since seroconversion (days)",
    y     = "log(CD4 count)"
  )


# 
# 
# tf_plot_clusters_tf <- function(cluster_tbl, centers_tbl) {
#   # pastel for individuals  
#   pal_ind  <- c("1" = "#E69A8DFF",
#                 "2" = "#F6D55C",
#                 "3" = "#2A9D8F")
#   # dark for centers  
#   pal_cent <- c("1" = "darkred",
#                 "2" = "darkorange",
#                 "3" = "darkgreen")
#   
#   ggplot() +
#     ## individual subjects in pastel
#     geom_spaghetti(
#       data       = cluster_tbl,
#       aes(y      = curve, colour = cluster, group = id),
#       alpha      = .15, 
#       linewidth  = .7
#     ) +
#     scale_color_manual(
#       name   = "Cluster",
#       values = pal_ind
#     ) +
#     
#     ## cluster centers in dark colors  
#     geom_spaghetti(
#       data      = filter(centers_tbl, cluster == "1"),
#       aes(y     = curve, group = cluster),
#       color     = pal_cent["1"],
#       linewidth = 1.5,
#       show.legend = FALSE
#     ) +
#     geom_spaghetti(
#       data      = filter(centers_tbl, cluster == "2"),
#       aes(y     = curve, group = cluster),
#       color     = pal_cent["2"],
#       linewidth = 1.5,
#       show.legend = FALSE
#     ) +
#     geom_spaghetti(
#       data      = filter(centers_tbl, cluster == "3"),
#       aes(y     = curve, group = cluster),
#       color     = pal_cent["3"],
#       linewidth = 1.5,
#       show.legend = FALSE
#     ) +
#     
#     labs(
#       x = "Time from seroconversion (months)",
#       y = "Log CD4 counts"
#     ) +
#     theme_minimal(base_size = 14)
# }
