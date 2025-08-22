# ─── packages  ───────────────────
library(tidyverse)
library(refund)
library(tidyfun)
library(gridExtra)


# ─── Load data  ───────────────────
df_subj <- read_rds(here("data", "nhanes_fda_with_r.rds"))

# ─── Data Preparation ───────────────────
## filter out participants 80+ and younger than 5
df_subj <- df_subj %>% filter(age >= 5, age < 80)
MIMS_mat <- unclass(df_subj$MIMS) 

# ─── fpca.face ───────────────────
# Do fPCA on the subject-average MIMS profiles
fpca_face_obj <- fpca.face(MIMS_mat)
fpca_face_tf <- tfd(t(fpca_face_obj$efunctions[, 1:4]), arg = 1:1440) %>%
  set_names(paste0("PC", 1:4))

plot(fpca_face_tf)

# ─── fpca2s ───────────────────
argvals <- seq(0, 1, length.out = ncol(MIMS_mat))
fpca2s_obj <- fpca2s(Y = MIMS_mat, npc = 4, argvals = argvals)
fpca2s_tf <- tfd(t(fpca2s_obj$efunctions[, 1:4]), arg = seq(1, 1440)) %>%
  set_names(paste0("PC", 1:4))

plot(fpca2s_tf)

# ─── tfb_fpc ───────────────────
tfb_fpc_obj <- tfb_fpc(MIMS_mat)  # default is SVD-based + spline smoothing
basis_functions <- tf_basis(tfb_fpc_obj, as_tfd = TRUE)
# skip the mean (first function), extract PC1 to PC4
tfb_tf <- -basis_functions[2:5] %>%
  set_names(paste0("PC", 1:4))  

tf_integrate(basis_functions[2:5]^2)

plot(tfb_tf)


# ─── Common plot bits ─────────────
time_breaks <- c(1, 6 * 60, 12 * 60, 18 * 60, 23 * 60)
time_labels <- c("01:00", "06:00", "12:00", "18:00", "23:00")

make_pc_plot <- function(dat, title_txt) {
  ggplot(dat, aes(y = curve, color = Component)) +
    geom_spaghetti() +   # one line per tf curve (PC) 
    scale_x_continuous(breaks = time_breaks, labels = time_labels, expand = c(0, 0)) +
    labs(
      title = title_txt,
      x = "Time of Day (s)",
      y = expression("Estimated Eigenfunctions (" * phi[k](s) * ")"),
      color = "Eigenfunction"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid      = element_blank(),
      axis.line       = element_line(linewidth = 1),
      axis.ticks      = element_line(linewidth = 1),
      plot.title      = element_text(face = "bold", size = 18, hjust = 0),
      legend.position = c(0.9, 0.9),
      legend.background = element_blank()
    ) +
    guides(color = guide_legend(ncol = 2))
}

# ─── Build data frames per method ─
df_face   <- tibble(Component = names(fpca_face_tf), curve = fpca_face_tf)
df_fpca2s <- tibble(Component = names(fpca2s_tf),    curve = fpca2s_tf)
df_tfb    <- tibble(Component = names(tfb_tf),       curve = tfb_tf)

# ─── Three individual plots ───────
p_face   <- make_pc_plot(df_face,   "(A) fPCA (face)")
p_fpca2s <- make_pc_plot(df_fpca2s, "(B) fPCA (fpca2s)")
p_tfb    <- make_pc_plot(df_tfb,    "(C) fPCA (tfb_fpc)")

# Print individually (e.g., in the console or knit document)
p_face +p_tfb +p_fpca2s

