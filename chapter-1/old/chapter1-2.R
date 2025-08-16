# ──────────────Weekly US COVID-19 mortality data──────────────────
#Visualization of COVID-19 and all-cause mortality


# ───Data ──────────────────
cv19_deaths     <- COVID19$US_weekly_mort_CV19
week_diff    <- COVID19$US_weekly_excess_mort_2020
current_date <- COVID19$US_weekly_excess_mort_2020_dates



# ─── Turn into tfd ──────────────────

death_tf <- tfd(
  rbind(week_diff, cv19_deaths),
  arg = num_grid(current_date)
)

# ── Prepare data for plotting ──────────────────

excess_tibble <- tibble(
  death_type = c("All-cause excess", "COVID-19"),
  curve      = death_tf
)

# ──────────────── Plot ────────────────
##Plot excess weekly US all-cause excess mortality and COVID-19 mortality in 2019

ggplot(excess_tibble, aes(y = curve, colour = death_type)) +
  geom_meatballs(size = 3, alpha = 0.6) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_continuous(
    name = "Weeks starting January 2020",
    breaks = as.numeric(seq(reference_date, as.Date("2021-01-01"), by = "3 months") - reference_date),
    labels = format(seq(reference_date, as.Date("2021-01-01"), by = "3 months"), "%b %Y")
  ) +
  # scale_x_continuous(
  #   limits = c(0, max(num_grid(current_date))),  # force axis to start at 0
  #   breaks = num_grid(current_date)[seq(1, length(current_date), 13)],
  #   labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y"),
  #   expand = c(0.01,0.1) # This line removes the gap(not really)
  # ) + #coord_cartesian(ylim = c(0, NA), expand = FALSE)+
  labs(
    x = "Weeks starting in January 2020",
    y = "All-cause excess and COVID-19 deaths in the US"
  ) +
  theme_classic() 