#The logic here was taken from the book

#Calculate the SVD of 𝑊, the corresponding eigenvalues, as well as the individual
#and cumulative proportion of variance explained.

W_mat  <- as.matrix(tf_centered)
SVDofW <- svd(W_mat)
d      <- SVDofW$d
V      <- SVDofW$v


lambda    <- d^2
prop_var  <- round(100 * lambda / sum(lambda), 1)
cum_var   <- cumsum(prop_var)


# ─────────────────── Turn into tfd ─────────────

tf_v <- tfd(
  # two right singular vectors
  t(V[, 1:2]),                  
  #arg = as.numeric(state_weeks)  
  arg = num_grid(state_weeks)
)


# ───────────────────── Plot ────────────────────────

#Display the first two right singular vectors


ggplot() +
  geom_spaghetti(aes(y = tf_v[1]), color = "coral", linewidth = 1, inherit.aes = FALSE) +
  geom_spaghetti(aes(y = tf_v[2]), color = "coral4", linewidth = 1, inherit.aes = FALSE) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
    #breaks = as.numeric(state_weeks[seq(1, length(state_weeks), by = 13)]),
    #labels = format(state_weeks[seq(1, length(state_weeks), by = 13)], "%b %Y")
  ) +
  coord_cartesian(ylim = c(-0.35, 0.35)) +
  labs(
    x = "Week",
    y = "Right singular vectors"
  ) +
  theme_classic(base_size = 13)+theme(legend.position = "none")



# svd_tbl <- tibble(
#   PC         = seq_along(lambda),
#   eigenvalue = lambda,
#   prop_var   = prop_var,
#   cum_var    = cum_var
# )
# 
# svd_tbl