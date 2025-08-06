#Chapter 2

# ──────────────────────────── SVD for US Excess Mortality ───────────────────────────


# ────────────────────── Weekly all-cause mortality data ───────────────────────
#Here we focus on showing how to conduct SVD and PCA decompositions based on the cumulative 
#weekly all cause mortality in 52 states and territories in the US.

# ─ Packages ─────────────────────────────
library(tidyverse)  
library(tidyfun)    
library(refund)
library(gridExtra)


# ─────────────helpers───────────────────

#Conversion of the dates into  numbers (days since the very first week)
#because the args of tfd  need numbers, not dates.
num_grid <- function(dates, ref = min(dates)) as.numeric(dates - ref)
reference_date <- as.Date("2020-01-01")


# ─ Data ─────────────────────────────────
data("COVID19", package = "refund")

new_states <- COVID19$US_states_names
current_date <- COVID19$US_weekly_excess_mort_2020_dates
Wd <- COVID19$States_excess_mortality_per_million  

# ── Data Transformation ───────────────────

#Converts weekly excess deaths to cumulative totals per state (all good)
#This is the same as Wr
cum_mat   <- t(apply(Wd, 1, cumsum))

# ─────────────────── Turn into tfd ─────────────

states_cum_tf <- tfd(
  cum_mat,
  arg = num_grid(current_date),
  id  = new_states,
  var = "cum_excess"
)
states_cum_tf

# ── grand‐mean & centered curves ─────────────────────────────────────────
#average curve across all states
mean_curve  <- mean(states_cum_tf)


#subtracts the mean curve from each state's curve, so that we  can analyze deviations from the national pattern.
centered_states_cum_tf <- scale(states_cum_tf, scale = FALSE)
centered_states_cum_tf

# ── Plot ────────────────
#Visualise the data for each state and territory (gray solid lines) and the mean (dark red solid lines).

ggplot() +
  geom_spaghetti(aes(y = states_cum_tf),
                 colour = "grey20", alpha = 0.15) +
  geom_spaghetti(aes(y = mean_curve),
                 colour = "darkred", linewidth = 1.4) +
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
    x     = "Weeks starting January 2020",
    y     = "Cumulative excess deaths per million"
  ) +
  theme_classic(base_size = 13)

