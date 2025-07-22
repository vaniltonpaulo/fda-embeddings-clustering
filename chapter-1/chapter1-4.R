# ───────────────── COVID-19 cumulative data ───────────────
#  Cumulative COVID-19 deaths per million by state
#     (highlight the same five states as in the previous script)



# ──────────────────── Data ────────────
states_covid <- COVID19$States_CV19_mortality

per_mil_mat  <- sweep(states_covid, 1, pop_states / 1e6, "/")
cum_covid    <- t(apply(per_mil_mat, 1, function(x) cumsum(replace_na(x, 0))))


# ────────────── Turn into tfd ──────────────────

covid_tf <- tfd(cum_covid, arg = num_grid(state_weeks))

covid_df <- tibble(
  state = states,
  curve = covid_tf
)


# ──────────────── Plot ────────────────
#Plot of COVID-19 cumulative data

ggplot() +
  geom_spaghetti(
    data   = covid_df,
    aes(y = curve),
    colour = "black", alpha = 0.1
  ) +
  # highlighted states
  geom_spaghetti(
    data   = filter(covid_df, state %in% emph_states),
    aes(y = curve, colour = state)
  ) +
  scale_colour_manual(values = setNames(emph_cols, emph_states)) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(state_weeks), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y"),
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