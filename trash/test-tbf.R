set.seed(13121)
x <- tf_rgp(25, nugget = .02)
x_pc <- tfb_fpc(x, pve = .9)
x_pc
plot(x, lwd = 3)
lines(x_pc, col = 2, lty = 2)
x_pc_full <- tfb_fpc(x, pve = .995)
x_pc_full
lines(x_pc_full, col = 3, lty = 2)
# partially missing data on common grid:
x_mis <- x |> tf_sparsify(dropout = .05)
x_pc_mis <- tfb_fpc(x_mis, pve = .9)
x_pc_mis
plot(x_mis, lwd = 3)
lines(x_pc_mis, col = 4, lty = 2)
# extract FPC basis --
# first "eigenvector" in black is (always) the mean function
x_pc |> tf_basis(as_tfd = TRUE) |> plot(col = 1:5)





# Apply FPCA for sparse, irregular data using refund::fpca.sc:
set.seed(99290)
# create small, sparse, irregular data:
x_irreg <- x[1:8] |>
  tf_jiggle() |> tf_sparsify(dropout = 0.3)
plot(x_irreg)
x_df <- x_irreg |>
  as.data.frame(unnest = TRUE)
# wrap refund::fpca_sc for use as FPCA method in tfb_fpc --
# 1. define scoring function (simple weighted LS fit)
fpca_scores <- function(data_matrix, efunctions, mean, weights) {
  w_mat <- matrix(weights, ncol = length(weights), nrow = nrow(data_matrix),
                  byrow = TRUE)
  w_mat[is.na(data_matrix)] <- 0
  data_matrix[is.na(data_matrix)] <- 0
  data_wc <- t((t(data_matrix) - mean) * sqrt(t(w_mat)))
  t(qr.coef(qr(efunctions), t(data_wc) / sqrt(weights)))
}
# 2. define wrapper for fpca_sc:
fpca_sc_wrapper <- function(data, arg, pve = 0.995, ...) {
  data_mat <- tfd(data) |> as.matrix(interpolate = TRUE)
  fpca <- refund::fpca.sc(
    Y = data_mat, argvals = attr(data_mat, "arg"), pve = pve, ...
  )
  c(fpca[c("mu", "efunctions", "scores", "npc")],
    scoring_function = fpca_scores)
}
x_pc <- tfb_fpc(x_df, method = fpca_sc_wrapper)
lines(x_pc, col = 2, lty = 2)