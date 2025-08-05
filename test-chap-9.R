canada <- data.frame(
  place = fda::CanadianWeather$place,
  region = fda::CanadianWeather$region,
  lat = fda::CanadianWeather$coordinates[, 1],
  lon = -fda::CanadianWeather$coordinates[, 2]
)

canada$temp <- tfd(t(fda::CanadianWeather$dailyAv[, , 1]), arg = 1:365)
canada$precipl10 <- tfd(t(fda::CanadianWeather$dailyAv[, , 3]), arg = 1:365) |>
  tf_smooth()
## using f = 0.15 as smoother span for lowess

canada_map <-
  data.frame(maps::map("world", "Canada", plot = FALSE)[c("x", "y")])


ggplot(canada, aes(x = lon, y = lat)) +
  geom_capellini(aes(tf = precipl10),
                 width = 4, height = 5, colour = "blue",
                 line.linetype = 1
  ) +
  geom_capellini(aes(tf = temp),
                 width = 4, height = 5, colour = "red",
                 line.linetype = 1
  ) +
  geom_path(data = canada_map, aes(x = x, y = y), alpha = 0.1) +
  coord_quickmap()







#test us map



#map_df <- map_data("state")  # This has long/lat/group


#us_map_df

# ggplot() +
#   # First add the map layer using geom_sf
#   geom_sf(data = us_map_df, 
#           fill = "white", 
#           color = "black", 
#           size = 0.2) +
#   # Then add the functional data glyphs
#   geom_capellini(data = state_plot_df,
#                  aes(x = lon, y = lat, tf = curve, colour = cluster),
#                  width = 3.5, height = 4.5, line.linetype = 1) +
#   scale_color_manual(values = c("#AA00FF", "green", "#FF6D00"), name = "Cluster") +
#   coord_sf() +  # Use coord_sf() instead of coord_quickmap()
#   theme_minimal() +
#   labs(title = "US States Excess Mortality Curves with Cluster Colors")

