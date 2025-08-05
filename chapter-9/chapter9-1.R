#chapter 9

# ────────────────US all-cause excess and Covid-19 mortality──────────────────────────────

#Clustering approaches for functional data and it is applied to the US Covid-19 weekly
#all-cause excess mortality data



# ─── Packages ───────────────────────────────────────────
library(refund)    
library(tidyfun)   
library(tidyverse)  
library(scales)    

# ─── Data ────────────────────────────

data("COVID19", package = "refund")

states <- COVID19$US_states_names
dates  <- COVID19$US_weekly_excess_mort_2020_dates
Wd     <- COVID19$States_excess_mortality_per_million

# Force reference to Jan 1, 2020
num_grid_v2 <- function(dates, ref = as.Date("2020-01-01")) as.numeric(dates - ref)


# Use their exact dates and numeric conversion with fixed reference
#dates <- as.Date(CV19$US_weekly_excess_mort_2020_dates)
#reference_date <- as.Date("2020-01-01") 
#as.numeric(dates - reference_date)

tnum <- num_grid_v2(dates)


  
# ─────────────────── tibble (Turn into tfd) ─────────────

#just a tibble to organize highlighted the states and their curves
df_tf <- tibble(
  state     = states,
  #the curves
  mortality = tfd(Wd, arg = tnum)  
) %>%
  mutate(
    highlight = if_else(
      state %in% c("New Jersey","Louisiana","California","Maryland","Texas"),
      state, "Other"
    ),
    highlight = factor(highlight, levels = c("Other",
                                             "New Jersey","Louisiana","California","Maryland","Texas"))
  )

df_tf

# the colors in the book were so pale, needed something more vibrant
cols <- c(
  "Other"      = "grey85",   
  "New Jersey" = "darkseagreen3",  
  "Louisiana"  = "red",  
  "California" = "plum3",  
  "Maryland"   = "deepskyblue4",  
  "Texas"      = "salmon"   
)

# ─── Plot ────────────────────────────────
#Exploratory plots and analyses

ggplot(df_tf) +
  geom_spaghetti(aes(y = mortality, colour = highlight, group = state)) +
  scale_colour_manual(values = cols, name = "States") +
  scale_x_continuous(
    breaks = function(x) {
      # Use 2020-01-01 as reference for quarterly breaks
      reference_date <- as.Date("2020-01-01")
      start_date <- reference_date  # Start from Jan 1, 2020
      end_date <- max(dates)
      quarterly_dates <- seq(from = start_date, to = end_date, by = "3 months")
      # Convert to your numeric scale (days since 2020-01-01)
      as.numeric(quarterly_dates - reference_date)
    },
    labels = function(x) {
      # Convert back to dates using 2020-01-01 as reference
      reference_date <- as.Date("2020-01-01")
      d <- reference_date + x
      format(d, "%b %Y")
    },
    name = "Weeks starting January 2020"
  )+
  labs(y = "US states weekly excess deaths / million") +
  theme_minimal()


# ────────── Summary of Analysis──────────────────────


Wd_tfd <- tfd(Wd, arg = tnum)  

# Extract values at week 10 for all states
# Correct extraction
week10_vals <- df_tf$mortality[, tnum[10]]

mean_week10   <- mean(week10_vals, na.rm = TRUE)
median_week10 <- median(week10_vals, na.rm = TRUE)

mean_week10
median_week10

week10 <- Wd[, 10]
week10

week10_vals == week10



ind_out <- which.max(week10_vals)
ind_out

# Get the name of that state
state_out <- states[ind_out]
state_out

# Extract values for weeks 6 to 14 for that state
val_out <- round(df_tf$mortality[ind_out, tnum[6:14]], 1)
val_out




# ---- Top 5 states at week 20 ----
vals20 <- df_tf$mortality[, tnum[20]]
ord20  <- order(vals20)
topweek20 <- round(vals20[ord20][48:52], 1)
states_top_20 <- states[ord20][48:52]

topweek20
states_top_20


# ---- Top 5 states at week 30 ----
vals30 <- df_tf$mortality[, tnum[30]]
ord30  <- order(vals30)
topweek30 <- round(vals30[ord30][48:52], 1)
states_top_30 <- states[ord30][48:52]

topweek30
states_top_30


# ---- Top 5 states at week 40 ----
vals40 <- df_tf$mortality[, tnum[40]]
ord40  <- order(vals40)
topweek40 <- round(vals40[ord40][48:52], 1)
states_top_40 <- states[ord40][48:52]

topweek40
states_top_40
