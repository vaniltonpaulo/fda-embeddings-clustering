# ──────────────── Packages ─────────────────────────────
library(tidyverse)  
library(tidyfun)    
library(refund)     


#Helpers

num_grid <- function(dates, ref = min(dates)) as.numeric(dates - ref)


# ──────────── Data ─────────────────────────────────
data("COVID19", package = "refund")
states <- COVID19$US_states_names
states
state_weeks <- COVID19$US_weekly_excess_mort_2020_dates
state_weeks
Wd <- COVID19$States_excess_mortality_per_million  


# Convert to cumulative totals and create tfd object
cum_mat   <- t(apply(Wd, 1, cumsum))
cum_mat



tf_states <- tfd(cum_mat, arg = num_grid(state_weeks), id = states)
tf_states
# Center data
tf_centered <- tf_states - mean(tf_states)
tf_centered
# FPCA
covid_pc <- tfb_fpc(tf_centered, pve = 0.99)
covid_pc

#Mental notes:
#like in the example in I think tfb_fpca(just check the package tf)
basis_functions <- tf_basis(covid_pc, as_tfd = TRUE)
basis_functions

#Books approach for comparison
W_centered <- scale(cum_mat, center = TRUE, scale = FALSE)
# SVD
svd_W <- svd(W_centered)

# Eigenfunctions (right singular vectors)
V <- svd_W$v





plot(num_grid(state_weeks), V[,1], type="l", col="coral", lwd=2,
     ylab="Eigenfunctions", xlab="2020 calendar year (in days)",
     ylim = c(-0.35, 0.35))
# axis(1,
#      at = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
#      labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y"))

# Add FPCA PC1
lines(basis_functions[2], col="coral", lwd=2, lty=2)

# Add SVD PC2
lines(num_grid(state_weeks), V[,2], col="coral4", lwd=2)

# Add FPCA PC2
lines(basis_functions[3], col="coral4", lwd=2, lty=2)


legend("topright",
       legend=c("SVD PC1", "FPCA PC1", "SVD PC2", "FPCA PC2"),
       col=c("coral", "coral", "coral4", "coral4"),
       lty=c(1,2,1,2),
       lwd=2)




