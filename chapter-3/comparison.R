#This is  a comparison of fpca.face to refund::fpca2s
#inspiration: https://cran.r-project.org/web/packages/refund/refund.pdf
#page 69.Prof.Scheipl is referenced



argvals <- seq(0, 1, length.out = ncol(MIMS_mat))


# Run fPCA via two-stage method (refund::fpca2s)
#    - Stage 1: SVD, Stage 2: smooth right singular vectors
#    - Provide explicit argvals; override default npc if needed
default_res <- fpca2s(Y = MIMS_mat, argvals = argvals)
n_pc <- 4
if (default_res$npc < n_pc) {
  fpca_2s_res <- fpca2s(Y = MIMS_mat, argvals = argvals, npc = n_pc)
} else {
  fpca_2s_res <- default_res
}

#Prepare argument grids for plotting
arg_face <- 1:ncol(MIMS_mat)


# Use the same argvals defined above, scaled to minutes
arg_2s <- argvals * ncol(MIMS_mat)


# ─────────────────── Turn into tfd ─────────────

# Lets extract and wrap the first n_pc eigenfunctions as tidyfun obj (that usually works)
pc_face_tf <- tfd(
  t(fpca_MIMS_subj$efunctions[, 1:n_pc]),
  arg = arg_face
) %>%
  set_names(paste0("PC", 1:n_pc))

pc_2s_tf <- tfd(
  t(fpca_2s_res$efunctions[, 1:n_pc]),
  arg = arg_2s
) %>%
  set_names(paste0("PC", 1:n_pc))

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
