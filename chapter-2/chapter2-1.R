#Chapter 2

# ──────────────────────────── SVD for US Excess Mortality ───────────────────────────


# ────────────────────── Weekly all-cause mortality data ───────────────────────
#Here we focus on showing how to conduct SVD and PCA decompositions based on the cumulative 
#weekly all cause mortality in 52 states and territories in the US.

# ──────────────── Packages ─────────────────────────────
library(tidyverse)  
library(tidyfun)    
library(refund)     



# ──────────── Data ─────────────────────────────────
data("COVID19", package = "refund")

states        <- COVID19$US_states_names
pop_states    <- COVID19$US_states_population
excess_states <- COVID19$States_excess_mortality
state_weeks   <- COVID19$US_weekly_excess_mort_2020_dates
Wd <- COVID19$States_excess_mortality_per_million  



#arg_num   <- as.numeric(state_weeks - min(state_weeks))
#arg_num <- num_grid(state_weeks, ref = min(state_weeks)) #tfd needs numbers hence we need the helper





#Converts weekly excess deaths to cumulative totals per state (all good)
#This is the same as Wr
cum_mat   <- t(apply(Wd, 1, cumsum))

# ─────────────────── Turn into tfd ─────────────

tf_states <- tfd(
  cum_mat,
  arg = num_grid(state_weeks),
  id  = states,
  var = "cum_excess"
)
tf_states
# ── grand‐mean & centered curves ─────────────────────────────────────────
#average curve across all states
mean_curve  <- mean(tf_states)
mean_curve
#subtracts the mean curve from each state's curve, so that we  can analyze deviations from the national pattern.
tf_centered <- scale(tf_states, scale = FALSE)
tf_centered

# ── Plot the data and the mean ──────────────────────────────────
#Visualise the data for each state and territory (gray solid lines) and the mean (dark red solid lines).

ggplot() +
  geom_spaghetti(aes(y = tf_states),
                 colour = "grey20", alpha = 0.15) +
  geom_spaghetti(aes(y = mean_curve),
                 colour = "darkred", linewidth = 1.4) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
  ) +
  labs(
    x     = "Weeks starting January 2020",
    y     = "Cumulative excess deaths per million"
  ) +
  theme_classic(base_size = 13)

# ─────────────────────────────────────────────
