# ────────────────── Topic ───────────────────────────
#Display the de-meaned data
#
# ─────────────────────────────────────────────



# ──  ·  centered curves + five highlights ───────────────────────────
long_centered <- as_tibble(as.matrix(tf_centered)) %>%
  setNames(as.character(dates)) %>%
  mutate(state = states) %>%
  pivot_longer(-state, names_to = "date", values_to = "centered") %>%
  mutate(date = as.Date(date))

emphasize   <- c("New Jersey","Louisiana","California","Maryland","Texas")
emph_colors <- c("darkgreen","red","plum3","deepskyblue4","salmon")
names(emph_colors) <- emphasize

ggplot(long_centered, aes(x = date, y = centered, group = state)) +
  geom_line(colour = alpha("black", 0.1), size = 0.5) +
  geom_line(data = filter(long_centered, state %in% emphasize),
            aes(color = state), size = 1.2) +
  scale_color_manual(values = emph_colors, guide = guide_legend(title = NULL)) +
  scale_x_date(
    breaks = seq(min(dates), max(dates), by = "3 months"),
    labels = format(seq(min(dates), max(dates), by = "3 months"), "%b %Y")
  ) +
  labs(
    x = "Weeks starting January 2020",
    y = "Centered cumulative excess deaths/million"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid        = element_blank(),
    axis.line         = element_line(colour = "black"),
    axis.ticks.length = unit(4, "pt"),
    legend.position   = "bottom"
  )
