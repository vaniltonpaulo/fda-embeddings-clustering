library(tidyverse)
library(tidyfun)


## read in the data (assumes data are in the current working directory)
df_subj <- read_rds(here::here("data","nhanes_fda_with_r.rds"))
df_subj
## filter out participants 80+ and younger than 5
df_subj <-
  df_subj %>% 
  filter(age >= 5, age < 80)

df_subj
## Do fPCA on the subject-average MIMS profiles
MIMS_mat <- unclass(df_subj$MIMS)
MIMS_mat
fpca_MIMS_subj <- fpca.face(MIMS_mat)
fpca_MIMS_subj

## Do PCA on the subject-average MIMS profiles
# subtract column (minute) means to center the "varianbles"
MIMS_mn     <- colMeans(MIMS_mat)
MIMS_mn
MIMS_mat_cn <- sweep(MIMS_mat, MARGIN=2, STATS=MIMS_mn, FUN="-")
MIMS_mat_cn
# then do SVD
# ── ──────────────────────────────

#I commented this out due to run time
#svd_MIMS_subj <- svd(MIMS_mat_cn)
# ── ──────────────────────────────



# … your code to read df_subj, build MIMS_mat, run fpca.face() into fpca_res
# … and run svd(MIMS_mat_cn) into svd_res goes here …

# 1) wrap as tf objects (flip sign on PCA to match the book):
fpca_tf <- tfd(t(fpca_res$efunctions[,1:4]), arg = 1:1440) %>%
  set_names(paste0("PC", 1:4))    # we'll call them PC1–PC4 in both panels
pca_tf  <- tfd(t(-svd_res$v[,1:4]), arg = 1:1440) %>%
  set_names(paste0("PC", 1:4))

# 2) build a one‐row‐per‐function tibble:
fpca_df <- tibble(Method = "fPCA", Component = names(fpca_tf), curve = fpca_tf)
pca_df  <- tibble(Method = "PCA",  Component = names(pca_tf),  curve = pca_tf)
pc_df   <- bind_rows(fpca_df, pca_df)

# 3) facet labels
method_labeller <- as_labeller(c(
  fPCA = "(A) fPCA",
  PCA  = "(B) PCA"
))

# 4) clock‐time breaks
time_breaks <- c(1, 6*60, 12*60, 18*60, 23*60)
time_labels <- c("01:00","06:00","12:00","18:00","23:00")

# 5) the plot
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
    panel.spacing     = unit(1, "cm"),    # increase vertical space
    panel.grid.major     = element_blank(),
    panel.grid.minor     = element_blank(),
    # draw axes
    axis.line            = element_line(size = 1),
    axis.ticks           = element_line(size = 1),
    # facet labels bold
    strip.text           = element_text(face = "bold", size = 18, hjust = 0),
    # legend inside top‐right, 2 columns
    legend.position      = c(0.9, 0.9),
    legend.background    = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))




library(tidyverse)
library(tidyfun)

## ── Step 1: Same seed + 0–1 time grid ────────────────────────────────────
set.seed(1983)
K     <- 4
n_plt <- 10
sind  <- seq(0, 1, length.out = 1440)

# clock‐time breaks on the 0–1 scale
xinx     <- (c(1,6,12,18,23)*60 + 1) / 1440
xinx_lab <- c("01:00","06:00","12:00","18:00","23:00")


## ── Step 2: Templates with “PC 1” etc. ───────────────────────────────────
df_plt_ind <- expand.grid(
  sind = sind,
  id   = 1:n_plt,
  high = c("low","high"),
  PC   = paste0("PC ", 1:K),
  stringsAsFactors = FALSE
) %>% 
  mutate(high = factor(high, levels = c("low","high")))

df_plt_ind_mu <- expand.grid(
  sind  = sind,
  high  = c("low","high"),
  PC    = paste0("PC ", 1:K),
  value = NA,
  stringsAsFactors = FALSE
) %>% 
  mutate(high = factor(high, levels = c("low","high")))


## ── Step 3: Loop *on* the FPCA‐fitted curves Yhat ─────────────────────────

# run your FPCA once:
df_subj   <- read_rds(here::here("data","nhanes_fda_with_r.rds")) %>% 
  filter(age >= 5, age < 80)
MIMS_mat  <- unclass(df_subj$MIMS)
fpca_res  <- fpca.face(MIMS_mat)               # from refund::fpca.face()

mu_vec    <- c()
ind_vec   <- c()

for(k in 1:K) {
  # 3a) 10th / 90th score cutoffs
  sc   <- fpca_res$scores[,k]
  q    <- quantile(sc, c(0.1, 0.9))
  lo   <- which(sc <= q[1])
  hi   <- which(sc >  q[2])
  
  # 3b) group means from *Yhat*
  mu_lo   <- colMeans(fpca_res$Yhat[lo,  ])
  mu_hi   <- colMeans(fpca_res$Yhat[hi,  ])
  mu_vec  <- c(mu_vec, mu_lo, mu_hi)
  
  # 3c) sample n_plt curves from Yhat
  sam_lo  <- sample(lo, size = n_plt)
  sam_hi  <- sample(hi, size = n_plt)
  ind_lo  <- as.vector(t(fpca_res$Yhat[sam_lo, ]))
  ind_hi  <- as.vector(t(fpca_res$Yhat[sam_hi, ]))
  ind_vec <- c(ind_vec, ind_lo, ind_hi)
}

df_plt_ind_mu$value <- mu_vec
df_plt_ind$value    <- ind_vec



# ── Step 4: Wrap as tfd + Plot (CORRECTED) ──────────────────────────────

# 4a) individuals → one row per (PC, high, id)
ind_tf <- df_plt_ind %>%
  group_by(PC, high, id) %>%
  summarize(curve = tfd(value, arg = sind), .groups = "drop")

# 4b) means → one row per (PC, high)
mu_tf <- df_plt_ind_mu %>%
  group_by(PC, high) %>%
  summarize(curve = tfd(value, arg = sind), .groups = "drop")

# 4c) final plot - CORRECTED VERSION
ggplot() +
  # thin & transparent individual curves
  geom_spaghetti(
    data      = ind_tf,
    aes(y = curve, color = high),  # Use 'y' instead of 'tf'
    linewidth = 0.5,
    alpha     = 0.4
  ) +
  # thick & opaque mean curves
  geom_spaghetti(
    data      = mu_tf,
    aes(y = curve, color = high),  # Use 'y' instead of 'tf'
    linewidth = 2
  ) +
  facet_wrap(~ PC, ncol = 2, scales = "free_y") +
  scale_x_continuous(
    breaks = time_breaks,
    labels = time_labels,
    expand = c(0, 0)
  ) +
  labs(
    x     = "Time of Day (hh:mm)",
    y     = expression("MIMS: " * W[i](s)),
    color = "Group"
  ) +
  scale_color_manual(
    values = c(low = "steelblue", high = "tomato"),
    labels = c("Bottom 10%", "Top 10%")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.spacing    = unit(0.8, "cm"),
    panel.grid       = element_blank(),
    axis.line        = element_line(linewidth = 1),
    axis.ticks       = element_line(linewidth = 1),
    strip.text       = element_text(face = "bold", size = 16, hjust = 0),
    legend.position  = "bottom",
    legend.background = element_blank()
  )