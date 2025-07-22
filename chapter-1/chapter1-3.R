# ──────────────Plot of all-cause cumulative excess data──────────────────
#   · Cumulative excess deaths per million by state
#     (highlight five states)


# ───────────── Data ───────────────────
states        <- COVID19$US_states_names
pop_states    <- COVID19$US_states_population
excess_states <- COVID19$States_excess_mortality
state_weeks   <- COVID19$US_weekly_excess_mort_2020_dates

#We have, for each state and each week, a raw count of excess deaths.
#But states have very different population sizes. 
#To compare them fairly, we turn those counts into excess deaths per million people.

#Normalize to rate per 1 million people
rate_mat <- sweep(excess_states, 1, pop_states / 1e6, "/")

#row by row , and replace each row with its cumulative sum.
#We transpose it back so that rows = states, columns = weeks again.
cum_mat  <- t(apply(rate_mat, 1, cumsum))

# ────────────── Turn into tfd ──────────────────

#So now we end up with a tfd object where each row(curve represents a state)
state_tf <- tfd(cum_mat, arg = num_grid(state_weeks))

state_df <- tibble(
  state = states,
  curve = state_tf
)


# ──────────────── Plot ────────────────


emph_states <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
emph_cols   <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")

ggplot() +
  geom_spaghetti(
    data   = state_df,
    aes(y = curve),
    colour = "grey20", alpha = .15
  ) +
  geom_spaghetti(
    data   = filter(state_df, state %in% emph_states),
    aes(y = curve, colour = state)
  ) +
  scale_colour_manual(values = setNames(emph_cols, emph_states)) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(state_weeks), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y"),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(-50, 2500)) +
  labs(
    x = "Weeks starting January 2020",
    y = "US states cumulative excess deaths per million"
  ) +
  theme_classic() +
  theme(legend.position = "right")
