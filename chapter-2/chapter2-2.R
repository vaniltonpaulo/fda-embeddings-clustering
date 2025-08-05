# ─────────────────────── Display the de-meaned data ──────────────────────

#I have to todo this: fixed varibale naming

# Display the de-meaned data
# We now show the effect of the subtracting the mean from the original data.
# We also emphasize a few states: New Jersey (green), Louisiana (red), California (plum), Maryland (blue), and Texas (salmon).



tf_centered_new <- tfd(
  t(apply(tf_centered, 1, identity)),
  arg = num_grid(state_weeks),
  id = states
)

tf_centered_new

ggplot() +
  geom_spaghetti(aes(y = tf_centered_new),
                 colour = "grey60", alpha = 0.3, linewidth = 0.5) +
  geom_spaghetti(aes(y = tf_centered_new[which(states == "New Jersey")]),
                 colour = "darkseagreen3", linewidth = 1.2) +
  geom_spaghetti(aes(y = tf_centered_new[which(states == "Louisiana")]),
                 colour = "red", linewidth = 1.2) +
  geom_spaghetti(aes(y = tf_centered_new[which(states == "California")]),
                 colour = "plum3", linewidth = 1.2) +
  geom_spaghetti(aes(y = tf_centered_new[which(states == "Maryland")]),
                 colour = "deepskyblue4", linewidth = 1.2) +
  geom_spaghetti(aes(y = tf_centered_new[which(states == "Texas")]),
                 colour = "salmon", linewidth = 1.2) +
  scale_x_continuous(
    breaks = num_grid(state_weeks)[seq(1, length(num_grid(state_weeks)), 13)],
    labels = format(state_weeks[seq(1, length(state_weeks), 13)], "%b\n%Y")
  ) +
  labs(
    x = "Weeks starting January 2020",
    y = "US states centered cumulative excess deaths/million"
  ) +
  theme_classic(base_size = 13)



