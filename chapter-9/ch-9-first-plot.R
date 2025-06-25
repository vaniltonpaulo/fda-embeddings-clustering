############################################################
## Exploratory plots and analyses ##
############################################################



library(refund)    # COVID19 data
library(tidyfun)   # tfd(), geom_spaghetti()
library(dplyr)     # tibble(), mutate()
library(ggplot2)   # plotting
library(scales)    # pretty_breaks()

data("COVID19", package = "refund")

# 1 · Extract and wrap (following the alternative's approach)
states <- COVID19$US_states_names
#dates  <- COVID19$US_weekly_excess_mort_2020_dates
Wd     <- COVID19$States_excess_mortality_per_million

# Use their exact dates and numeric conversion with fixed reference
dates <- as.Date(CV19$US_weekly_excess_mort_2020_dates)
reference_date <- as.Date("2020-01-01")  # Force reference to Jan 1, 2020
tnum <- as.numeric(dates - reference_date)



df_tf <- tibble(
  state     = states,
  mortality = tfd(Wd, arg = tnum)  # Use dates directly as argvals
) %>%
  mutate(
    highlight = if_else(
      state %in% c("New Jersey","Louisiana","California","Maryland","Texas"),
      state, "Other"
    ),
    highlight = factor(highlight, levels = c("Other",
                                             "New Jersey","Louisiana","California","Maryland","Texas"))
  )

# 2 · Colors (same as alternative)
cols <- c(
  "Other"      = "grey85",   # keep background subtle
  "New Jersey" = "#00C853",  # fluorescent green
  "Louisiana"  = "#D50000",  # deep vivid red
  "California" = "#2962FF",  # electric blue
  "Maryland"   = "#AA00FF",  # intense purple
  "Texas"      = "#FF6D00"   # blazing orange
)


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
#The logic and code was taken directly from the book

# extract week 10 (i.e. the 10th column)
week10 <- Wd[, 10]

# compute mean and median, dropping any NAs
meanWd10   <- mean(week10, na.rm = TRUE)
medianWd10 <- median(week10, na.rm = TRUE)

meanWd10
medianWd10



ind_out <- which.max(Wd[,10])
state_out <- states[ind_out]
val_out <- round(Wd[ind_out, 6:14], digits = 1)



topweek20 <- round(Wd[order(Wd[,20])[48:52],20], digits = 1)
states_top_20 <- states[order(Wd[,20])[48:52]]


topweek30 <- round(Wd[order(Wd[,30])[48:52],30], digits = 1)
states_top_30 <- states[order(Wd[,30])[48:52]]



topweek40 <- round(Wd[order(Wd[,40])[48:52],40], digits = 1)
states_top_40 <- states[order(Wd[,40])[48:52]]


