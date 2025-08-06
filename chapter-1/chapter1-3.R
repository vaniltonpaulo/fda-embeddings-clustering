# ──────────────Plot of all-cause cumulative excess data──────────────────
#   · Cumulative excess deaths per million by state
#     (highlight five states)


# ── Data ───────────────────
new_states        <- COVID19$US_states_names
state_population    <- COVID19$US_states_population
states_excess <- COVID19$States_excess_mortality

#We have, for each state and each week, a raw count of excess deaths.
#But states have very different population sizes. 
#To compare them fairly, we turn those counts into excess deaths per million people.

# ── Data Transformation ───────────────────

#Normalize to rate per 1 million people
rate_mat <- sweep(states_excess, 1, state_population / 1e6, "/")

#row by row , and replace each row with its cumulative sum.
#We transpose it back so that rows = states, columns = weeks again.
cum_mat  <- t(apply(rate_mat, 1, cumsum))

# ────────────── Turn into tfd ──────────────────

#So now we end up with a tfd object where each row(curve represents a state)
state_tf <- tfd(cum_mat, arg = num_grid(current_date))


# ── Prepare data for plotting ──────────────────

state_df <- tibble(
  state = new_states,
  curve = state_tf
)


# ── Plot ────────────────


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
    aes(y = curve, colour = state), alpha = 5
  ) +
  scale_colour_manual(values = setNames(emph_cols, emph_states)) +
  scale_x_continuous(
    breaks = num_grid(current_date)[seq(1, length(current_date), 13)],
    labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y"),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(-50, 2500)) +
  labs(
    x = "Weeks starting January 2020",
    y = "US states cumulative excess deaths per million"
  ) +
  theme_classic() +
  theme(legend.position = "right")
