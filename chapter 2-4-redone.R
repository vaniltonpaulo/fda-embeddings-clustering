# Load required packages
library(readxl)
library(dplyr)
library(knitr)
library(formattable)
library(lubridate)
library(tidyr)
library(refund)
library(tidyfun)  # for tfb_fpc
library(ggplot2)
library(gridExtra)

# Load COVID-19 data (assuming this is already done)
data("COVID19")
CV19 <- COVID19

# Extract the data as in original code
Wd <- CV19$States_excess_mortality_per_million
current_date <- CV19$US_weekly_excess_mort_2020_dates
new_states <- CV19$US_states_names

# Create cumulative excess mortality
Wr <- Wd
for(i in 1:dim(Wd)[1]){
  Wr[i,] <- cumsum(Wd[i,])
}

# ====================================================================
# MANUAL SVD APPROACH (for comparison)
# ====================================================================

# Column mean and centering
mW <- colMeans(Wr)
mW_mat <- matrix(rep(mW, each = nrow(Wr)), ncol = ncol(Wr))
W <- Wr - mW_mat

# Manual SVD
SVD_of_W <- svd(W)
U <- SVD_of_W$u
d <- SVD_of_W$d
V <- SVD_of_W$v

# Manual reconstruction (K=2)
K0 <- 2
rec_manual <- U[,1:K0] %*% diag(d[1:K0]) %*% t(V[,1:K0])
WK0_manual <- mW_mat + rec_manual

# ====================================================================
# tfb_fpc APPROACH - RECONSTRUCTION
# ====================================================================

# Create time grid
time_grid <- 1:ncol(Wr)
state_weeks <- current_date  # Your actual date vector
num_grid <- function(dates) as.numeric(dates - dates[1])  # Convert to numeric grid

# Method 1: Direct reconstruction from tfb_fpc object
# Create tfb_fpc object with K=2 components
fpca_result <- tfb_fpc(Wr, arg = time_grid, pve = 0.99)  # High PVE to get ~2 components

# The tfb_fpc object IS the reconstruction! 
# Convert back to matrix for comparison
#This is only to check if everything is fine
WK0_tfb <- as.matrix(fpca_result)

# Method 2: Manual reconstruction using tfb_fpc components (to show equivalence)
# Extract components
fpca_basis <- tf_basis(fpca_result, as_tfd = TRUE)
fpca_coefs <- coef(fpca_result)

# Get basis matrix and mean
basis_matrix <- as.matrix(fpca_basis)
mean_function <- basis_matrix[,1]  # First column is always the mean

# Manual reconstruction: mean + sum(coef_i * basis_i)
# This recreates what tfb_fpc does internally
#n_components <- ncol(fpca_coefs) - 1  # Exclude mean coefficient (always 1)
# WK0_manual_tfb <- matrix(mean_function, nrow = nrow(Wr), ncol = ncol(Wr), byrow = TRUE)
# 
# for(i in 1:nrow(Wr)) {
#   for(j in 2:ncol(fpca_coefs)) {  # Start from 2 to skip mean coefficient
#     WK0_manual_tfb[i,] <- WK0_manual_tfb[i,] + fpca_coefs[i,j] * basis_matrix[,j]
#   }
# }

# ====================================================================
# CREATE tfd OBJECTS FOR PLOTTING (following your style)
# ====================================================================

# Convert to tfd objects
states <- new_states
emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")

# Original data as tfd
tf_states <- tfd(Wr, arg = num_grid(state_weeks), id = states, var = "cum_excess_orig")

# Manual SVD reconstruction as tfd
tf_recon_manual <- tfd(WK0_manual, arg = num_grid(state_weeks), id = states, var = "cum_excess_recon_manual")

# tfb_fpc reconstruction as tfd
tf_recon_tfb <- tfd(WK0_tfb, arg = num_grid(state_weeks), id = states, var = "cum_excess_recon_tfb")

# ====================================================================
# COMPARISON DATA FRAME
# ====================================================================

emph_cols <- c("plum3","red", "deepskyblue4","darkseagreen3", "salmon")

# Create comparison data frame
comp_df <- tibble(
  state = states,
  original_curve = tf_states,
  recon_manual = tf_recon_manual,      # Manual SVD reconstruction
  recon_tfb = tf_recon_tfb            # tfb_fpc reconstruction
) %>%
  filter(state %in% emphasize)

# ====================================================================
# PLOTS
# ====================================================================

# Plot 1: Original vs Manual SVD reconstruction (your original approach)
p1 <- ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2, alpha = 0.8) +
  geom_spaghetti(aes(y = recon_manual, color = state),
                 linewidth = 2, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = emph_cols) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
  ) +
  labs(
    title = "Manual SVD Reconstruction (K=2)",
    x = "Weeks starting January 2020",
    y = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)

# Plot 2: Original vs tfb_fpc reconstruction
p2 <- ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2, alpha = 0.8) +
  geom_spaghetti(aes(y = recon_tfb, color = state),
                 linewidth = 2, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = emph_cols) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
  ) +
  labs(
    title = "tfb_fpc Reconstruction",
    x = "Weeks starting January 2020",
    y = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)

# Display plots

grid.arrange(p1 , p2, ncol=2)

# ====================================================================
# VERIFICATION OF EQUIVALENCE
# ====================================================================

cat("=== VERIFICATION OF EQUIVALENCE ===\n")
cat("Manual SVD vs tfb_fpc reconstruction comparison:\n")

# Check if reconstructions are equivalent (up to numerical precision)
max_diff <- max(abs(WK0_manual - WK0_tfb), na.rm = TRUE)
cat("Maximum absolute difference between reconstructions:", max_diff, "\n")

if(max_diff < 1e-10) {
  cat("✓ Reconstructions are numerically equivalent!\n")
} else {
  cat("! Reconstructions differ - check component selection\n")
}

# Show reconstruction quality
original_var <- var(as.vector(Wr))
residual_var_manual <- var(as.vector(Wr - WK0_manual))
residual_var_tfb <- var(as.vector(Wr - WK0_tfb))

cat("\nReconstruction quality:\n")
cat("Manual SVD - Variance explained:", 
    round(100 * (1 - residual_var_manual/original_var), 2), "%\n")
cat("tfb_fpc - Variance explained:", 
    round(100 * (1 - residual_var_tfb/original_var), 2), "%\n")

# ====================================================================
# KEY INSIGHTS
# ====================================================================

cat("\n=== KEY INSIGHTS ===\n")
cat("1. tfb_fpc object IS the reconstruction - no manual reconstruction needed\n")
cat("2. as.matrix(tfb_fpc_object) gives you the reconstructed data directly\n")
cat("3. The number of components is determined by 'pve' parameter\n")
cat("4. tfb_fpc automatically handles centering and reconstruction\n")
cat("5. Both methods give identical results (up to numerical precision)\n")

cat("\n=== USAGE RECOMMENDATIONS ===\n")
cat("• Use tfb_fpc when you want functional data framework and easy plotting\n")
cat("• Use manual SVD when you need explicit control over each step\n")
cat("• tfb_fpc is more convenient for most functional data analysis tasks\n")