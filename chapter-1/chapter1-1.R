#Chapter 1

# ──────────────Focus: COVID-19 US Mortality Data──────────────────

# Exploratory plots for all-cause and COVID-19 weekly mortality data in the US

#Goals:
#.   To visualize all-cause total and excess mortality (mortality in a week in 2020 minus
#      mortality in the coresponding week in 2019)

#.  To visualize total COVID-19 mortality as a function of time.

# ─Packages───────────────────
#  Importing all the packages 

library(refund)      
library(tidyverse)   
library(tidyfun)     


# ────Overview───────────────────
data("COVID19", package = "refund") 
names(COVID19)
glimpse(COVID19)


# ─────────────helpers───────────────────

#Conversion of the dates into  numbers (days since the very first week)
#because the args of tfd  need numbers, not dates.
num_grid <- function(dates, ref = min(dates)) as.numeric(dates - ref)


# ─────────────────── Weekly all-cause mortality in the US ─────────────
#Data
counts_state <- COVID19$US_weekly_mort
date_state <- COVID19$US_weekly_mort_dates


# ───Data Transformation ─────────────


#Its standard in tidyfun to create a tibble(part of the pipeline)
nat_weekly <- tibble(
  # week-start dates
  date   = date_state,
  #the weekly all-cause death counts  
  #Divide by 1000 to indicate numbers in thousands
  deaths = counts_state / 1000       
)


# Shading rectangles: actual full weeks of 2019 and 2020
#The red area is 2020 and the blue area is 2019

shade <- nat_weekly %>%
  filter(year(date) %in% c(2019, 2020)) %>%
  mutate(
    year = year(date),
    day_num = num_grid(date, ref = min(nat_weekly$date))
  ) %>%
  group_by(year) %>%
  summarise(
    #For each year, get the min and max week-index
    xmin = min(day_num),
    xmax = max(day_num),
    .groups = "drop"
  ) %>%
  mutate(
    band = as.character(year),
    colour = c("blue", "red")
  )

# X-axis year labels at first week of each year
year_ticks <- nat_weekly %>%
#we  add two new columns
  mutate(
    year = year(date),
    day_num = num_grid(date)
  ) %>%
  group_by(year) %>%
  #within each year, we take the row with the smallest date for the plot
  slice_min(date, n = 1) %>%
  ungroup()



#  Plot weekly all-cause US mortality 2017 to 2020
#. The reason for the gap is because if you run this:
# print(nat_weekly %>%
#         filter(year(date) %in% c(2019, 2020)),n =104)
#You will see that there is a week between the last date of 2019 and the first date of 2020


# ─────────────────── Turn into tfd ─────────────
nat_weekly_tfd <- tfd(
  #We reshape the deaths into a 1-row matrix so it’s literally one curve through 2017 to 2020.
  matrix(nat_weekly$deaths, nrow = 1),
  arg = num_grid(nat_weekly$date)
)


# ─────────────────── Plot ─────────────

ggplot() +
  geom_rect(
    data = shade,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = colour),
    inherit.aes = FALSE, alpha = 0.15
  ) +
  scale_fill_identity() +
  geom_meatballs(
    data = tibble(y = nat_weekly_tfd),
    aes(y = y),
    colour = "steelblue", size = 2, alpha = 0.6
  ) +
  scale_x_continuous(
    breaks = year_ticks$day_num,
    labels = year_ticks$year,
    expand = c(0, 0)
  ) +
  labs(
    x = "Weeks starting in January 2017",
    y = "Weekly all-cause deaths in the US (thousands)"
  ) +
  theme_classic() 

