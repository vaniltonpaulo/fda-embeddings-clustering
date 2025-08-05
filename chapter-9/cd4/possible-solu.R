#───────────────────────────────────────────────────────────────
#   Clustering Sparse CD4 Counts Data using tfb_fpc + face.sparse
#───────────────────────────────────────────────────────────────

library(face)
library(refund)
library(tidyverse)
library(tidyfun)

#─────────────────── Helper Functions ──────────────────────────

tf_cluster_kmeans <- function(score_mat, K) {
  km <- kmeans(score_mat, centers = K)
  list(cluster = km$cluster, centers = km$centers)
}

# Simplified function to reconstruct curves from cluster centers
tf_centers_from_scores <- function(centers_mat, pred_tf, scores) {
  # Use the original predicted curves to reconstruct cluster centers
  # This is more reliable than trying to extract basis functions
  
  n_centers <- nrow(centers_mat)
  n_curves <- length(pred_tf)
  
  # Convert pred_tf to matrix for easier manipulation
  pred_matrix <- as.matrix(pred_tf)
  
  # Calculate mean curve
  mean_curve <- colMeans(pred_matrix)
  
  # Center the data
  centered_data <- scale(pred_matrix, center = TRUE, scale = FALSE)
  
  # Perform SVD to get basis functions
  svd_result <- svd(centered_data)
  
  # Get the required number of basis functions
  n_components <- ncol(centers_mat)
  basis_funcs <- svd_result$v[, 1:n_components]
  singular_vals <- svd_result$d[1:n_components]
  
  # Reconstruct cluster centers
  reconstructed <- matrix(NA, n_centers, ncol(pred_matrix))
  
  for (i in 1:n_centers) {
    # Start with mean curve
    reconstructed[i, ] <- mean_curve
    
    # Add weighted basis functions
    for (j in 1:n_components) {
      reconstructed[i, ] <- reconstructed[i, ] + 
        centers_mat[i, j] * basis_funcs[, j] * singular_vals[j]
    }
  }
  
  # Convert to tfd object
  tfd(reconstructed, arg = attr(pred_tf, "arg"))
}

tf_join_clusters_tf <- function(pred_tf, cluster_vec) {
  tibble(
    id      = seq_along(pred_tf),
    cluster = factor(cluster_vec),
    curve   = pred_tf
  )
}

tf_plot_clusters_tf <- function(cluster_tbl, centers_tbl) {
  ggplot() +
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

#─────────────────── Data Preprocessing ────────────────────────

data(cd4)

n <- nrow(cd4)
T <- ncol(cd4)

dat <- tibble(
  subj    = rep(seq_len(n), each = T),
  argvals = rep(-18:42, times = n),
  y       = log(as.vector(t(cd4)))
) %>%
  filter(!is.na(y), y > 4.5)

tgrid <- -20:40

#─────────────────── FACE Smoothing ────────────────────────────

fit <- face.sparse(
  dat,
  argvals.new      = tgrid,
  calculate.scores = TRUE,
  newdata          = dat,
  pve              = 0.95
)

#─────────────────── Predict Curves on Common Grid ─────────────

Pred_mat <- matrix(NA_real_, n, length(tgrid))
uid <- unique(dat$subj)

for (i in seq_len(n)) {
  dat_i <- filter(dat, subj == uid[i])
  new_i <- tibble(
    y       = c(dat_i$y, rep(NA_real_, length(tgrid))),
    argvals = c(dat_i$argvals, tgrid),
    subj    = c(dat_i$subj, rep(uid[i], length(tgrid)))
  )
  yhat <- predict(fit, new_i)$y.pred
  Pred_mat[i, ] <- tail(yhat, length(tgrid))
}

# Convert to tf object
pred_tf <- tfd(Pred_mat, arg = tgrid)

#─────────────────── FPCA Using tfb_fpc (calls fpc_wsvd) ───────

fpc_obj <- tfb_fpc(pred_tf, arg = tgrid, pve = 0.95)

# Extract FPC scores as a numeric matrix
scores <- coef(fpc_obj)

# If it's a list, bind rows
if (is.list(scores)) {
  scores <- do.call(rbind, scores)
}

# Drop the first column (mean score = 1 for all)
if (all(scores[, 1] == 1)) {
  scores <- scores[, -1, drop = FALSE]
}

#─────────────────── Clustering ────────────────────────────────
set.seed(202200228)
km_res <- tf_cluster_kmeans(scores, K = 3)

#─────────────────── Prepare Data for Plot ─────────────────────

# Fixed: Use the corrected function with proper parameters
centers_tf <- tf_centers_from_scores(km_res$centers, fpc_obj, tgrid)

cluster_tbl <- tf_join_clusters_tf(pred_tf, km_res$cluster)
centers_tbl <- tf_join_clusters_tf(centers_tf, factor(1:3))

#─────────────────── Create the Plot ───────────────────────────

plot_result <- tf_plot_clusters_tf(cluster_tbl, centers_tbl)
plot_result

#─────────────────── Summary Statistics ─────────────────────────

cat("Cluster Summary:\n")
table(km_res$cluster)

cat("\nCluster Centers (FPC Scores):\n")
print(km_res$centers)

cat("\nNumber of FPCs used:", ncol(scores), "\n")
cat("Proportion of variance explained:", attr(fpc_obj, "pve"), "\n")