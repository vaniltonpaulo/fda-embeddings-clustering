# ───── Display the first two right singular vectors─────────────────────────────

#here instead of scale I used subtracting the mean curve because it helps compare 
#against the variable  W in the book.But they do the same thing
centered_states_cum_tf <- states_cum_tf - mean_curve

# FPCA
covid_pc <- tfb_fpc(centered_states_cum_tf, pve = 0.99)
covid_pc

basis_functions <- tf_basis(covid_pc, as_tfd = TRUE)
basis_functions

# ───── Books approach for comparison(manual svd approach)─────────────────────────────

#Construct the de-meaned data. The columns of W have mean zero
W <- scale(cum_mat, center = TRUE, scale = FALSE)
# SVD
svd_W <- svd(W)

# Eigenfunctions (right singular vectors)
V <- svd_W$v
V


# ───── Plot(Comparison)─────────────────────────────


par(mar = c(5, 4, 4, 8) + 0.1,
    xpd = TRUE)    



plot(num_grid(current_date), V[,1], type="l", col="coral", lwd=2.5,
     ylab="Eigenfunctions", xlab="2020 calendar year (in days)",
     ylim = c(-0.35, 0.35), 
     cex=1, bty="n")

lines(basis_functions[2],   col="coral",  lwd=2, lty=2)   # FPCA PC1
lines(num_grid(current_date), V[,2],      col="coral4", lwd=2)   # SVD PC2
lines(basis_functions[3],   col="coral4", lwd=2, lty=2)   # FPCA PC2




legend("topright",
       legend = c("SVD PC1", "FPCA PC1", "SVD PC2", "FPCA PC2"),
       col = c("coral", "coral", "coral4", "coral4"),
       lty = c(1, 2, 1, 2),
       lwd = 2,
       bty = "n",
       cex = 0.7,
       ncol = 2)  # Two columns to save space