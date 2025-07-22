# ──────────────Weekly US COVID-19 mortality data──────────────────
#Visualization of COVID-19 and all-cause mortality


# ────────────── Data ──────────────────
covid     <- COVID19$US_weekly_mort_CV19
excess    <- COVID19$US_weekly_excess_mort_2020
weeks2020 <- COVID19$US_weekly_excess_mort_2020_dates


#num_grid <- function(dates) as.numeric(dates - min(dates))

# ────────────── Turn into tfd ──────────────────

death_tf <- tfd(
  rbind(excess, covid),
  arg = num_grid(weeks2020)
)

plot_df <- tibble(
  death_type = c("All-cause excess", "COVID-19"),
  curve      = death_tf
)

# ──────────────── Plot ────────────────
##Plot excess weekly US all-cause excess mortality and COVID-19 mortality in 2019

ggplot(plot_df, aes(y = curve, colour = death_type)) +
  geom_meatballs(size = 3) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_continuous(
    limits = c(0, max(num_grid(weeks2020))),  # force axis to start at 0
    breaks = num_grid(weeks2020)[seq(1, length(weeks2020), 13)],
    labels = format(weeks2020[seq(1, length(weeks2020), 13)], "%b\n%Y"),
    expand = c(0.01,0.1) # This line removes the gap(not really)
  ) + #coord_cartesian(ylim = c(0, NA), expand = FALSE)+
labs(
    x = "Weeks starting in January 2020",
    y = "All-cause excess and COVID-19 deaths in the US"
  ) +
  theme_classic() 
