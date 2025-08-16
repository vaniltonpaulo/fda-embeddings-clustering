# ─────────────────────── Display the de-meaned data ──────────────────────
# Display the de-meaned data
# We now show the effect of the subtracting the mean from the original data.
# We also emphasize a few states: New Jersey (green), 
#Louisiana (red), California (plum), Maryland (blue), and Texas (salmon).



centered_cum_states_tf_v2 <- tfd(
  t(apply(centered_states_cum_tf, 1, identity)),
  arg = num_grid(current_date),
  id = new_states
)

centered_cum_states_tf_v2
# ── Plot ────────────────

ggplot() +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2),
                 colour = "grey60", alpha = 0.2, linewidth = 0.5) +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2[which(new_states == "New Jersey")]),
                 colour = "darkseagreen3", linewidth = 1.2, alpha = 3) +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2[which(new_states == "Louisiana")]),
                 colour = "red", linewidth = 1.2, alpha = 3) +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2[which(new_states == "California")]),
                 colour = "plum3", linewidth = 1.2, alpha = 3) +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2[which(new_states == "Maryland")]),
                 colour = "deepskyblue4", linewidth = 1.2, alpha = 3) +
  geom_spaghetti(aes(y = centered_cum_states_tf_v2[which(new_states == "Texas")]),
                 colour = "salmon", linewidth = 1.2, alpha = 3) +
  scale_x_continuous(
    name = "Weeks starting January 2020",
    breaks = as.numeric(seq(reference_date, as.Date("2021-01-01"), by = "3 months") - reference_date),
    labels = format(seq(reference_date, as.Date("2021-01-01"), by = "3 months"), "%b %Y")
  ) +
  # scale_x_continuous(
  #   breaks = num_grid(current_date)[seq(1, length(num_grid(current_date)), 13)],
  #   labels = format(current_date[seq(1, length(current_date), 13)], "%b\n%Y")
  # ) +
  labs(
    x = "Weeks starting January 2020",
    y = "US states centered cumulative excess deaths/million"
  ) +
  theme_classic(base_size = 13)



