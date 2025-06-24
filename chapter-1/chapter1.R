#  ----------------------------------
#  0 · Packages needed + Data
#  ----------------------------------
#Some of the packages we need
library(refund)      # provides the COVID-19 mortality data
library(tidyverse)   # data wrangling & ggplot2
library(tidyfun)     # tf objects + pasta-themed geoms
library(lubridate)   # nicer date helpers (optional)


data("COVID19", package = "refund")   

#Here we are getting a Overview of the dataset
glimpse(COVID19)
#dim(COVID19). its a list so we get Null
#View(COVID19)
names(COVID19)


# helper: convert a Date vector to a numeric grid for tidyfun
# we need a numeric  vector. 
#We convert the dates into  numbers (days since the very first week), because the args of tfd  need numbers, not dates.
num_grid <- function(dates, ref = min(dates)) as.numeric(dates - ref)


# ────────────────────────────────
#  1 · Weekly US all‐cause mortality (2017–2020)
#
# ────────────────────────────────

#Its standard in tidyfun to create a tibble(part of the pipeline)

#In a long format
nat_weekly <- tibble(
  # week-start dates
  date   = COVID19$US_weekly_mort_dates,
  #the weekly all-cause death counts  
  #Divide by 1000 to indicate numbers in thousands
  deaths = COVID19$US_weekly_mort / 1000       
)


nat_weekly

nat_tf <- tfd(
  #We reshape the deaths into a 1-row matrix so it’s literally one curve through 2017 to 2020.
  matrix(nat_weekly$deaths, nrow = 1),
  arg = num_grid(nat_weekly$date)
)



# Shading rectangles: actual full weeks of 2019 and 2020
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

# Plot
ggplot() +
  geom_rect(
    data = shade,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = colour),
    inherit.aes = FALSE, alpha = 0.15
  ) +
  scale_fill_identity() +
  geom_meatballs(
    data = tibble(y = nat_tf),
    aes(y = y),
    colour = "steelblue", size = 2
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

