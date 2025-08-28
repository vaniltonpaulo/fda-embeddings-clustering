# --- Libraries ---
library(tidyverse)
library(refund)     
library(tidyfun)    

# --- Data ---
## read in the data (assumes data are in the current working directory)
df_subj <- read_rds(here::here("data","nhanes_fda_with_r.rds"))
## filter out participants 80+ and younger than 5
df_subj <-
  df_subj %>% 
  filter(age >= 5, age < 80)

# --- FPCA via refund::fpca.face ---
MIMS_mat <- unclass(df_subj$MIMS)
MIMS_mat[1,] - mean(MIMS_mat)
MIMS_mat - mean(MIMS_mat)
fpca_MIMS_subj <- fpca.face(MIMS_mat)

# Wrap the first 4 eigenfunctions from fpca.face into a tfd for plotting
fpca_tf <- tfd(t(fpca_MIMS_subj$efunctions[, 1:4]), arg = 1:1440) %>%
  set_names(paste0("PC", 1:4))
fpca_tf
# --- FPCA via tf::tfb_fpc (weighted SVD by default) ---
# Note: tfb_fpc internally centers and returns mean + FPCs as the basis.
tfb_fpc_obj <- tfb_fpc(MIMS_mat)  
basis_functions <- tf_basis(tfb_fpc_obj, as_tfd = TRUE)  # tfd of [mean, PC1, PC2, ...]
tfb_tf <- -basis_functions[2:5] %>%                     # skip mean -> take PC1..PC4
  set_names(paste0("PC", 1:4))

# --- Assemble plotting data (tidyfun likes a tfd column) ---
pc_df <- bind_rows(
  tibble(Method = "fPCA (refund)", Component = names(fpca_tf), curve = fpca_tf),
  tibble(Method = "fPCA (tf)",     Component = names(tfb_tf),  curve = tfb_tf)
)

# Facet labels
method_labeller <- as_labeller(c(
  "fPCA (refund)" = "(A) fPCA (refund::fpca.face)",
  "fPCA (tf)" = "(B) fPCA (tf::tfb_fpc)"
))

# Clock-time ticks
time_breaks <- c(1, 6*60, 12*60, 18*60, 23*60)
time_labels <- c("01:00","06:00","12:00","18:00","23:00")

# --- Plot ---
ggplot(pc_df, aes(y = curve, color = Component)) +
  geom_spaghetti() +
  facet_grid(~Method, scales = "free_y", labeller = method_labeller) +
  scale_x_continuous(breaks = time_breaks, labels = time_labels, expand = c(0,0)) +
  labs(
    x = "Time of Day (min)",
    y = expression("Estimated Eigenfunctions (" * phi[k](s) * ")"),
    color = "Eigenfunction"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.spacing = unit(1, "cm"),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 1),
    axis.ticks = element_line(linewidth = 1),
    strip.text = element_text(face = "bold", size = 18, hjust = 0),
    legend.position = c(0.9, 0.9),
    legend.background = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))
