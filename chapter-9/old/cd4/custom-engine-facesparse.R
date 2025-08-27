# ─── Libraries ───────────────────────────────────────────
library(face)
library(refund)
library(tidyverse)
library(tidyfun)

# ─── options(they dont work at the moment) ────────────────────────────────────────────────
options(pillar.sigfig = 10)
options(tfb_signif = 15)


# ─── Convert to tf object ────────────────────────────────
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

#CD4 observations are sparse hence we put into tfd object
cd4_tf <- tfd(dat, id = "subj", arg = "argvals", value = "y")

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

centers_mat <- t(
  sapply(seq_len(nrow(km_res$centers)), function(k) {
    mu_grid + psi_mat %*% km_res$centers[k, ]
  })
)  

centers_tf <- tfd(centers_mat, arg = arg_grid,signif = 4)
centers_tf


centers <- tibble(
  cluster = factor(seq_len(nrow(km_res$centers))),
  curve   = centers_tf
)

clusters <- tibble(
  id      = seq_along(cd4_fpca),
  cluster = factor(km_res$cluster),
  curve   = cd4_fpca
)
as.matrix(cd4_fpca[1,])
#I need to check the centers if they look good
as.matrix(centers_tbl$curve)
# ─── Plot ───────────────────────────────────────
tf_plot_clusters_tf(clusters, centers) +
  #scale_color_brewer("Cluster", palette = "Set1") +
  theme_minimal(base_size = 14) +
  labs(
    title = "",
    x     = "Time since seroconversion (days)",
    y     = "log(CD4 count)"
  )

