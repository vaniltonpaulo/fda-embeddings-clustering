library(tidyverse)
library(here)
library(tidyfun)

# 1. Read & filter (you already have this)
df_subj <- read_rds(here("data","nhanes_fda_with_r.rds")) %>%
  filter(age >= 5, age < 80)

# 2. Turn your existing MIMS_mat into a fun_data object
#    (we’ll pull argvals straight from 0…1 over 1440 points)
MIMS_mat <- unclass(df_subj$MIMS)
mims_fd <- as_fun_mat(
  MIMS_mat,
  argvals = seq(0, 1, length.out = ncol(MIMS_mat)),
  id      = df_subj$SEQN,
  yname   = "MIMS"
)

# 3. (Optional) a quick spaghetti plot of all raw subjects
ggplot(mims_fd, aes(x = arg, y = value, group = id)) +
  geom_spaghetti(alpha = 0.2) +
  labs(
    x = "Time of Day (fraction)",
    y = "Raw MIMS activity",
    title = "Spaghetti of raw MIMS profiles"
  ) +
  theme_minimal()