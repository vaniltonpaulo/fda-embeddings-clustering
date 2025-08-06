# ───────────────── Plot of COVID-19 cumulative data───────────────
#  Cumulative COVID-19 deaths per million by state
#     (highlight the same five states as in the previous script)



# ── Data ────────────
states_CV19_mort <- COVID19$States_CV19_mortality


# ── Data Transformation ───────────────────
per_mil_mat  <- sweep(states_CV19_mort, 1, state_population / 1e6, "/")
cum_covid    <- t(apply(per_mil_mat, 1, function(x) cumsum(replace_na(x, 0))))


# ────────────── Turn into tfd ──────────────────

covid_tf <- tfd(cum_covid, arg = num_grid(current_date))

# ── Data Transformation ───────────────────
covid_df <- tibble(
  state = new_states,
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
    aes(y = curve, colour = state), alpha = 2
  ) +
  scale_colour_manual(values = setNames(emph_cols, emph_states)) +
  scale_x_continuous(
    breaks = num_grid(current_date)[seq(1, length(current_date), 13)],
    labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y"),
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