library(tidyverse)
library(tidyfun)
library(refund)

# Read in the data
df_subj <- read_rds(here::here("data","nhanes_fda_with_r.rds")) %>%
  filter(age >= 5, age < 80)

# Create matrix of subject-average MIMS profiles
MIMS_mat <- unclass(df_subj$MIMS)

# 1) fPCA via FACE (refund::fpca.face)
fpca_face_res <- fpca.face(MIMS_mat)

# 2) fPCA via two-stage method (refund::fpca2s)
#    - Default: penalized spline smoothing in stage 1, PCA in stage 2
#    - Note: fpca2s may extract fewer components by default
fpca_2s_res <- fpca2s(Y = MIMS_mat)

# If we need more components, we can specify npc parameter
if (fpca_2s_res$npc < n_pc) {
  cat("fpca2s only extracted", fpca_2s_res$npc, "components. Re-running with npc =", n_pc, "\n")
  fpca_2s_res <- fpca2s(Y = MIMS_mat, npc = n_pc)
}

# Inspect the structure of fpca2s results
cat("Structure of fpca2s results:\n")
str(fpca_2s_res)

# Check what's in the functions component
cat("\nStructure of fpca2s functions:\n")
str(fpca_2s_res$functions)

# 3) Extract eigenfunctions properly
n_pc <- 4
arg_pts <- 1:ncol(MIMS_mat)

# FACE components
pc_face_tf <- tfd(
  t(fpca_face_res$efunctions[,1:n_pc]), arg = arg_pts
) %>% set_names(paste0("PC", 1:n_pc))

# 2S components - fpca2s uses 'efunctions' not 'functions'
# Also note: fpca2s only extracted 1 component, so we need to request more
# Let's first work with what we have, then re-run with more components

# Check how many components were actually extracted
n_pc_2s <- min(n_pc, ncol(fpca_2s_res$efunctions))
cat("fpca2s extracted", n_pc_2s, "components\n")

if (n_pc_2s > 0) {
  pc_2s_tf <- tfd(
    t(fpca_2s_res$efunctions[,1:n_pc_2s]), arg = arg_pts
  ) %>% set_names(paste0("PC", 1:n_pc_2s))
} else {
  cat("No components extracted by fpca2s\n")
  pc_2s_tf <- NULL
}

# Alternative approach if the above doesn't work:
# Sometimes fpca2s stores results differently, so let's check all components
cat("\nAll components in fpca2s result:\n")
names(fpca_2s_res)

# If functions doesn't work, try other possible names like:
# - efunctions (similar to fpca.face)
# - vectors
# - eigenfunctions
# You might need to adjust based on what's actually returned

# 4) Create comparison data frame
fpca_face_df <- tibble(Method = "FACE", Component = names(pc_face_tf), curve = pc_face_tf)

if (!is.null(pc_2s_tf)) {
  fpca_2s_df <- tibble(Method = "2S", Component = names(pc_2s_tf), curve = pc_2s_tf)
  pc_df <- bind_rows(fpca_face_df, fpca_2s_df)
} else {
  pc_df <- fpca_face_df
  cat("Warning: No 2S components to plot\n")
}

# 5) Facet labels
method_labeller <- as_labeller(c(
  FACE = "(A) FACE",
  `2S` = "(B) Two-Stage"
))

# 6) Clock-time breaks
time_breaks <- c(1, 6*60, 12*60, 18*60, 23*60)
time_labels <- c("01:00","06:00","12:00","18:00","23:00")

# 7) The plot
ggplot(pc_df, aes(y = curve, color = Component)) +
  geom_spaghetti() +
  facet_grid(~Method, scales = "free_y", labeller = method_labeller) +
  scale_x_continuous(
    breaks = time_breaks,
    labels = time_labels,
    expand = c(0,0)
  ) +
  labs(
    x = "Time of Day (s)",
    y = expression("Estimated Eigenfunctions (" * phi[k](s) * ")"),
    color = "Eigenfunction"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    # remove all grid lines
    panel.spacing = unit(1, "cm"),    # increase vertical space
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # draw axes
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1),
    # facet labels bold
    strip.text = element_text(face = "bold", size = 18, hjust = 0),
    # legend inside top-right, 2 columns
    legend.position = c(0.9, 0.9),
    legend.background = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))

# 8) Compare other aspects
cat("\nComparison of key results:\n")
cat("FACE - Number of components:", ncol(fpca_face_res$efunctions), "\n")
cat("FACE - Explained variance:", fpca_face_res$evalues[1:n_pc], "\n")

# For fpca2s, the variance explained might be stored differently
if ("evalues" %in% names(fpca_2s_res)) {
  cat("2S - Explained variance:", fpca_2s_res$evalues[1:n_pc], "\n")
} else if ("values" %in% names(fpca_2s_res)) {
  cat("2S - Explained variance:", fpca_2s_res$values[1:n_pc], "\n")
} else {
  cat("2S - Variance information not found in standard location\n")
}

# Compare scores if available
if ("scores" %in% names(fpca_2s_res)) {
  cat("2S - Score dimensions:", dim(fpca_2s_res$scores), "\n")
}
cat("FACE - Score dimensions:", dim(fpca_face_res$scores), "\n")