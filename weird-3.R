
library(tidyverse)
library(tidyfun)
library(refund)

# 1) Load and filter the NHANES data
df_subj <- read_rds(here::here("data","nhanes_fda_with_r.rds")) %>%
  filter(age >= 5, age < 80)

# 2) Extract the subject-average MIMS profiles as a matrix
MIMS_mat <- unclass(df_subj$MIMS)  # dimensions: n_subjects × 1440

# 3) Run fPCA via FACE (refund::fpca.face)
fpca_face_res <- fpca.face(MIMS_mat)

# 4) Run fPCA via two-stage method (refund::fpca2s)
#    - Stage 1: P-spline smoothing, Stage 2: PCA
#    - Default npc may be small; override if needed
n_pc <- 4
tmp <- fpca2s(Y = MIMS_mat)
if (tmp$npc < n_pc) {
  fpca_2s_res <- fpca2s(Y = MIMS_mat, npc = n_pc)
} else {
  fpca_2s_res <- tmp
}

# 5) Prepare argument grids for plotting
arg_face <- 1:ncol(MIMS_mat)
# Scale fpca2s argvals (0–1) to minutes (1:1440)
arg_2s <- fpca_2s_res$argvals * ncol(MIMS_mat)

# 6) Extract and wrap the first n_pc eigenfunctions as tidyfuns
pc_face_tf <- tfd(
  t(fpca_face_res$efunctions[, 1:n_pc]),
  arg = arg_face
) %>%
  set_names(paste0("PC", 1:n_pc))

pc_2s_tf <- tfd(
  t(fpca_2s_res$efunctions[, 1:n_pc]),
  arg = arg_2s
) %>%
  set_names(paste0("PC", 1:n_pc))

# 7) Build a combined tibble for plotting
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

# 8) Facet labels and time-axis settings
method_labeller <- as_labeller(c(FACE = "(A) FACE", `2S` = "(B) Two-Stage"))
time_breaks <- c(1, 6*60, 12*60, 18*60, 23*60)
time_labels <- c("01:00", "06:00", "12:00", "18:00", "23:00")

# 9) Plot separately and combine
library(patchwork)

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
    x = "Time of Day (min)",
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
    x = "Time of Day (min)",
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
