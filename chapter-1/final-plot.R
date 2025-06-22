# ────────────────────────────────
#  4 · Cumulative COVID-19 deaths per million by state
#     (highlight the same five states)
# ────────────────────────────────
states_covid <- COVID19$States_CV19_mortality
state_names  <- COVID19$US_states_names
state_pop    <- COVID19$US_states_population
week_dates   <- COVID19$US_weekly_excess_mort_2020_dates

per_mil_mat  <- sweep(states_covid, 1, state_pop / 1e6, "/")
cum_covid    <- t(apply(per_mil_mat, 1, function(x) cumsum(replace_na(x, 0))))

covid_tf <- tfd(cum_covid, arg = num_grid(week_dates))

covid_df <- tibble(
  state = state_names,
  curve = covid_tf
)

ggplot() +
  # background curves
  geom_spaghetti(
    data   = covid_df,
    aes(y = curve),
    colour = "black", alpha = 0.1, size = 0.7
  ) +
  # highlighted states
  geom_spaghetti(
    data   = filter(covid_df, state %in% emph_states),
    aes(y = curve, colour = state),
    size   = 1.3
  ) +
  scale_colour_manual(values = setNames(emph_cols, emph_states)) +
  scale_x_continuous(
    breaks = num_grid(week_dates)[seq(1, length(week_dates), 13)],
    labels = format(week_dates[seq(1, length(week_dates), 13)], "%b\n%Y"),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(
    x   = "Weeks starting January 2020",
    y   = "Cumulative COVID-19 deaths per million (US states)",
    col = NULL
  ) +
  theme_classic() +
  theme(legend.position = "right")