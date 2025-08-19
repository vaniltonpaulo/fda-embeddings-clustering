J <- ncol(MIMS_mat)
arg01 <- seq(0, 1, length.out = J)
arg_min <- 0:(J-1)
n_pc <- 4

# Run methods
fpca_2s_res <- fpca2s(Y = MIMS_mat, argvals = arg01, npc = n_pc)
# assuming this is fpca.face result:
# fpca_face_res <- fpca.face(Y = MIMS_mat, argvals = arg01, npc = n_pc)

Phi_face <- fpca_MIMS_subj$efunctions[, 1:n_pc, drop = FALSE]
Phi_2s   <- fpca_2s_res$efunctions[, 1:n_pc, drop = FALSE]

# Optional: L2 normalize on [0,1]
Delta <- 1 / J
scale_l2 <- function(M) sweep(M, 2, sqrt(colSums(M^2) * Delta), `/`)
Phi_face <- scale_l2(Phi_face)
Phi_2s   <- scale_l2(Phi_2s)

# Align signs to face
for (k in 1:n_pc) if (cor(Phi_2s[,k], Phi_face[,k]) < 0) Phi_2s[,k] <- -Phi_2s[,k]

# Wrap as tidyfun, same argument grid for both
pc_face_tf <- tfd(t(Phi_face), arg = arg_min) %>% set_names(paste0("PC", 1:n_pc))
pc_2s_tf   <- tfd(t(Phi_2s),   arg = arg_min) %>% set_names(paste0("PC", 1:n_pc))


#combined tibble 
df_face <- tibble(
  Method    = "FACE",
  Component = names(pc_face_tf),
  curve     = pc_face_tf
)
df_2s <- tibble(
  Method    = "2S",
  Component = names(pc_2s_tf),
  curve     = pc_2s_tf
)
plot_df <- bind_rows(df_face, df_2s)


# ─────────────────── Plot + comparsion plots ─────────────

# Facet labels and time-axis settings
method_labeller <- as_labeller(c(FACE = "(A) FACE", `2S` = "(B) Two-Stage"))
time_breaks <- c(1, 6*60, 12*60, 18*60, 23*60)
time_labels <- c("01:00", "06:00", "12:00", "18:00", "23:00")


# FACE plot
p_face <- ggplot(df_face, aes(y = curve, color = Component)) +
  geom_spaghetti() +
  scale_x_continuous(
    breaks = time_breaks,
    labels = time_labels,
    expand = c(0, 0)
  ) +
  labs(
    title = "(A) FACE",
    x = "Time of Day",
    y = expression(phi[k](s)),
    color = "Component"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    legend.position = c(0.9, 0.9),
    legend.background = element_blank()
  )

# Two-stage plot
p_2s <- ggplot(df_2s, aes(y = curve, color = Component)) +
  geom_spaghetti() +
  scale_x_continuous(
    breaks = time_breaks,
    labels = time_labels,
    expand = c(0, 0)
  ) +
  labs(
    title = "(B) Two-Stage",
    x = "Time of Day",
    y = expression(phi[k](s)),
    color = "Component"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    legend.position = c(0.9, 0.9),
    legend.background = element_blank()
  )

# Combine plots with independent y-axes
p_face + p_2s + plot_layout(ncol = 2)

