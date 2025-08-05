# ── Reconstruction of the original data ───────────────────────────────


#a lot of the logic was taken from the book
#I dont think I need to reinvent the wheel for everything 


#Set the approximation rank
K0 <- 2

U <- SVDofW$u

#Reconstruct the de-meaned data using the first K0 right singular vectors
rec <- U[,1:K0] %*% diag(d[1:K0]) %*% t(V[,1:K0])

# ─────────────────── Turn into tfd ─────────────

rec_ctr <- tfd(rec,   arg = num_grid(state_weeks), id = states, var = "cum_excess_recon")

#Add the mean to the rank K0 approximation of W
WK0 <- rec_ctr + mean_curve


emph_cols <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")


# ── Compare the real and reconstructed data ───────────────────────

#overlay real vs rank-2 recon for five states
#Now everything is ready to be ploted
comp_df <- tibble(
  state          = states,
  original_curve = tf_states,
  recon_curve    = WK0
) %>%
  filter(state %in% emphasize)



# ── Plot ──────────────────────────────────

ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2, alpha = 2) +
  geom_spaghetti(aes(y = recon_curve,   color = state),
                 linewidth = 2, linetype = "dashed",  alpha = 2) +
  scale_color_manual(values = emph_cols) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
  ) +
  labs(
    x     = "Weeks starting January 2020",
    y     = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)

