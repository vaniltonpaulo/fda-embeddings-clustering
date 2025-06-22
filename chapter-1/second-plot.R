# ────────────────────────────────
#  2 · 2020 excess deaths vs. COVID-19 deaths
# ────────────────────────────────
covid     <- COVID19$US_weekly_mort_CV19

excess    <- COVID19$US_weekly_excess_mort_2020
weeks2020 <- COVID19$US_weekly_excess_mort_2020_dates


num_grid <- function(dates) as.numeric(dates - min(dates))


death_tf <- tfd(
  rbind(excess, covid),
  arg = num_grid(weeks2020)
)

plot_df <- tibble(
  death_type = c("All-cause excess", "COVID-19"),
  curve      = death_tf
)

ggplot(plot_df, aes(y = curve, colour = death_type)) +
  geom_meatballs(size = 2) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_x_continuous(
    breaks = num_grid(weeks2020)[seq(1, length(weeks2020), 13)],
    labels = format(weeks2020[seq(1, length(weeks2020), 13)], "%b\n%Y"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Weeks starting in January 2020",
    y = "All-cause excess and COVID-19 deaths in the US"
  ) +
  theme_classic() +
  theme(legend.position.inside = c(.15, .90))
