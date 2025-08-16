# ──────────────────── Compare the real and reconstructed data ──────────────────────
#Book approach for comparison later
#I  need to make some adjustmests to the previous section chapter2-3(we just need it here)
mW <- colMeans(cum_mat)

#Construct a matrix of same dimension as cum_mat with the mean of cum_mat on each row
mW_mat <- matrix(rep(mW, each = nrow(cum_mat)), ncol = ncol(cum_mat))


# Manual reconstruction (K=2)
K0 <- 2
rec <- svd_W$u[,1:K0] %*% diag(svd_W$d[1:K0]) %*% t(V[,1:K0])
WK0 <- mW_mat + rec

#Mental note: I dont know exactly what I am doing.I dont know how to apply tfb_fpc here.
#I am very confused
#Need help

#Update I fixed.Soory Prof Scheipl, I was making a silly mistake.:)
states_cum_tf

# Manual SVD reconstruction as tfd
manual_svd <- tfd(WK0, arg = num_grid(current_date), id = new_states, var = "cum_excess")
manual_svd
svd_tfb <- tfb_fpc(states_cum_tf, pve = .9)
svd_tfb





emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
emph_cols <- c("plum3","red", "deepskyblue4","darkseagreen3", "salmon")

# Create comparison data frame
comp_df <- tibble(
  state = new_states,
  original_curve = states_cum_tf,
  recon_manual = manual_svd,      # Manual SVD reconstruction
  recon_tfb = svd_tfb            # tfb_fpc reconstruction
) %>%
  filter(state %in% emphasize)

# ───── Plot ────────────────────────────────────


# Original vs Manual SVD reconstruction 

p1 <- ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2, alpha = 0.8) +
  geom_spaghetti(aes(y = recon_manual, color = state),
                 linewidth = 2, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = emph_cols) +
  scale_x_continuous(
    name = "Weeks starting January 2020",
    breaks = as.numeric(seq(reference_date, as.Date("2021-01-01"), by = "3 months") - reference_date),
    labels = format(seq(reference_date, as.Date("2021-01-01"), by = "3 months"), "%b %Y")
  ) +
  # scale_x_continuous(
  #   breaks = num_grid(current_date)[seq(1, length(num_grid(current_date)), 13)],
  #   labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y")
  # ) +
  labs(
    title = "Manual SVD  (K=2)",
    x = "Weeks starting January 2020",
    y = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)

#Original vs tfb_fpc 
p2 <- ggplot(comp_df) +
  geom_spaghetti(aes(y = original_curve, color = state),
                 linewidth = 2, alpha = 0.8) +
  geom_spaghetti(aes(y = recon_tfb, color = state),
                 linewidth = 2, linetype = "dashed", alpha = 0.8) +
  scale_color_manual(values = emph_cols) +
  scale_x_continuous(
    name = "Weeks starting January 2020",
    breaks = as.numeric(seq(reference_date, as.Date("2021-01-01"), by = "3 months") - reference_date),
    labels = format(seq(reference_date, as.Date("2021-01-01"), by = "3 months"), "%b %Y")
  ) +
  # scale_x_continuous(
  #   breaks = num_grid(current_date)[seq(1, length(num_grid(current_date)), 13)],
  #   labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y")
  # ) +
  labs(
    title = "tfb_fpc Reconstruction",
    x = "Weeks starting January 2020",
    y = "Cumulative excess deaths per million",
    color = NULL
  ) +
  theme_classic(base_size = 14)

# Display plots

grid.arrange(p1 , p2, ncol=2)

