# ── 5 · SVD on the centered matrix W ─────────────────────────────────────────

#The logic here was taken from the book
W_mat  <- as.matrix(tf_centered)
SVDofW <- svd(W_mat)
d      <- SVDofW$d
V      <- SVDofW$v


lambda    <- d^2
prop_var  <- round(100 * lambda / sum(lambda), 1)
cum_var   <- cumsum(prop_var)


tf_v <- tfd(
  t(V[, 1:2]),                  # two right singular vectors
  arg = as.numeric(dates)  # numeric x-axis (required)
)




  ggplot() +
  geom_spaghetti(aes(y = tf_v[1]), color = "coral", linewidth = 2, inherit.aes = FALSE) +
  geom_spaghetti(aes(y = tf_v[2]), color = "coral4", linewidth = 2, inherit.aes = FALSE) +
scale_x_continuous(
    breaks = as.numeric(dates[seq(1, length(dates), by = 13)]),
    labels = format(dates[seq(1, length(dates), by = 13)], "%b %Y")
  ) +
  coord_cartesian(ylim = c(-0.35, 0.35)) +
  labs(
    x = "Week",
    y = "Right singular vectors"
  ) +
  theme_classic(base_size = 13)+theme(legend.position = "none")





svd_tbl <- tibble(
  PC         = seq_along(lambda),
  eigenvalue = lambda,
  prop_var   = prop_var,
  cum_var    = cum_var
)

svd_tbl



# ── 6 · rank-2 reconstruction & add mean back ───────────────────────────────
U       <- SVDofW$u
W2      <- U[,1:2] %*% diag(d[1:2]) %*% t(V[,1:2])
rec_ctr <- tfd(W2,   arg = arg_num, id = states, var = "cum_excess_recon")
rec_full<- rec_ctr + mean_curve

# ── 7 · overlay real vs rank-2 recon for five states ───────────────────────
comp_df <- tibble(
  state          = states,
  original_curve = tf_states,
  recon_curve    = rec_full
) %>%
  filter(state %in% emphasize)

ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2) +
  geom_spaghetti(aes(y = recon_curve,   color = state),
                 linewidth = 2, linetype = "dashed") +
  scale_color_manual(values = emph_colors) +
  scale_x_continuous(
    breaks = arg_num[seq(1, length(arg_num), 13)],
    labels = format(dates[seq(1, length(dates), 13)], "%b\n%Y")
  ) +
  labs(
    x     = "Weeks starting January 2020",
    y     = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)




# ── Plot the scores on the first and second singular vectors ───────────────────────────────


scores_df <- tibble(
  state = states,
  PC1 = U[,1],
  PC2 = U[,2],
  emphasized = state %in% emphasize
)

# Create the scatter plot
ggplot(scores_df, aes(x = PC1, y = PC2)) +
  geom_point(data = filter(scores_df, !emphasized), 
             size = 2, color = "black") +  # Non-emphasized states in black
  geom_point(data = filter(scores_df, emphasized), 
             aes(color = state), size = 3) +  # Emphasized states in color
  scale_color_manual(values = emph_colors) +
  labs(
    x = "Scores on first right singular vector",
    y = "Scores on second right singular vector"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")






