
# ── Plot the scores on the first and second singular vectors ───────────────────────────────
# This the last plot and I dont know how to improve it using tidyfun.I NEED HELP!!!!!

scores_df <- tibble(
  state = states,
  PC1 = U[,1],
  PC2 = U[,2],
  emphasized = state %in% emphasize
)

# Create the scatter plot
ggplot(scores_df, aes(x = PC1, y = PC2)) +
  geom_point(data = filter(scores_df, !emphasized), 
             size = 2, color = "black") +  # Non-emphasized states in black
  geom_point(data = filter(scores_df, emphasized), 
             aes(color = state), size = 3) +  # Emphasized states in color
  scale_color_manual(values = emph_cols) +
  labs(
    x = "Scores on first right singular vector",
    y = "Scores on second right singular vector"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")






