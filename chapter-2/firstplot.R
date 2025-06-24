# ────────────────── Topic ───────────────────────────
#1 ·Chapter 2
# SVD for US Excess Mortality
#
# ─────────────────────────────────────────────


# ──────────────── Packages we need ─────────────────────────────
library(tidyverse)  # dplyr, ggplot2, tidyr, etc.
library(tidyfun)    # tfd(), geom_spaghetti(), stat_tf()
library(tf)         # mean(), scale() Not sure if we need to call this package
library(refund)     # COVID19 data



# ──────────── Data prep─────────────────────────────────
data("COVID19", package = "refund")

states    <- COVID19$US_states_names
pm_weekly <- COVID19$States_excess_mortality_per_million  
dates     <- COVID19$US_weekly_excess_mort_2020_dates

#arg_num   <- as.numeric(dates - min(dates))
arg_num <- num_grid(dates, ref = min(dates)) #tfd needs numbers hence we need the helper




# ── 1 · original cumulative curves → tfd ────────────────────────────────────

#Converts weekly excess deaths to cumulative totals per state
cum_mat   <- t(apply(pm_weekly, 1, cumsum))


tf_states <- tfd(
  cum_mat,
  arg = arg_num,
  id  = states,
  var = "cum_excess"
)

# ── 2 · grand‐mean & centered curves ─────────────────────────────────────────
#average curve across all states
mean_curve  <- mean(tf_states)
#subtracts the mean curve from each state's curve, so that we  can analyze deviations from the national pattern.
tf_centered <- scale(tf_states, scale = FALSE)

# ── 3 · Plot A: grey spaghetti + red mean ──────────────────────────────────
#Goal:Visualise the data for each state and territory (gray solid lines) and the mean (dark red solid lines).

ggplot() +
  geom_spaghetti(aes(y = tf_states),
                 colour = "grey20", alpha = 0.15) +
  geom_spaghetti(aes(y = mean_curve),
                 colour = "darkred", linewidth = 1.4) +
  scale_x_continuous(
    breaks = arg_num[seq(1, length(arg_num), 13)],
    labels = format(dates[seq(1, length(dates), 13)], "%b\n%Y")
  ) +
  labs(
    x     = "Weeks starting January 2020",
    y     = "Cumulative excess deaths per million"
  ) +
  theme_classic(base_size = 13)

