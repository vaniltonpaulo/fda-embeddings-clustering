# ──────────────── K-means clustering of the functional data ──────────────────────────────

rownames(Wd) <- states
set.seed(1000)
km    <- kmeans(Wd, centers = 3)
cl_ind <- km$cluster
cl_ind
cl_cen <- km$centers
cl_cen


##  ─── Build tidyfun objects ──────────────────────
pred_tf    <- tfd(Wd, arg = tnum)
pred_tf
centers_tf <- tfd(cl_cen, arg = tnum)
centers_tf

# Create tibbles
cluster_tbl <- tf_join_clusters_tf(pred_tf, cl_ind)
centers_tbl <- tibble(
  cluster = factor(seq_len(nrow(cl_cen))),
  curve   = centers_tf
)

# Plot with corrected date handling
tf_plot_clusters_tf(cluster_tbl, centers_tbl) +
  scale_color_manual(
    values = c("#AA00FF","green", "#FF6D00"),
    name   = "Cluster"
  ) +
  scale_x_continuous(
    breaks = function(x) {
      # Use 2020-01-01 as reference for quarterly breaks
      reference_date <- as.Date("2020-01-01")
      start_date <- reference_date  # Start from Jan 1, 2020
      end_date <- max(current_date)
      quarterly_dates <- seq(from = start_date, to = end_date, by = "3 months")
      # Convert to your numeric scale (days since 2020-01-01)
      as.numeric(quarterly_dates - reference_date)
    },
    labels = function(x) {
      # Convert back to dates using 2020-01-01 as reference
      reference_date <- as.Date("2020-01-01")
      d <- reference_date + x
      format(d, "%b %Y")
    },
    name = "Weeks starting January 2020"
  ) +
  labs(
    y = "Excess mortality per million",
    title = "K-Means Clustering of State Excess-Mortality Curves"
  ) +
  theme_minimal(base_size = 14)





##  ─── US MAP PLOT ──────────────────────

# color per group as per the book
colset <- c(rgb(0.41, 0.05, 0.68), rgb(0, 1, 0), rgb(1, .55, 0))

## load state date which contains FIPS code
data("statepop")

## create a data frame to plot based on the input requirement
state_cluster <- data.frame(full = names(cl_ind), cluster = unname(cl_ind))
state_cluster
data_cluster <- statepop %>%
  left_join(state_cluster, by = "full") %>%
  select(fips, cluster)
data_cluster$cluster <- as.factor(data_cluster$cluster)

## make the US map
p1_9 <- plot_usmap(regions = "states", data = data_cluster, values = "cluster") +
  scale_fill_manual(name = "Cluster", values = colset) +
  labs(title = "") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold"))





# Start with default state centers
state_centers <- data.frame(
  state = tolower(state.name),
  lon = state.center$x,
  lat = state.center$y
)


# Modify locations for Alaska and Hawaii (inset display)
state_centers <- state_centers %>%
  mutate(
    lon = case_when(
      state == "alaska" ~ -116,
      state == "hawaii" ~ -103,
      TRUE ~ lon
    ),
    lat = case_when(
      state == "alaska" ~ 28,
      state == "hawaii" ~ 28,
      TRUE ~ lat
    )
  )

# Add Distric of Columbia and Puerto Rico
state_centers <- rbind(
  state_centers,
  data.frame(state = "district of columbia", lon = -77.03, lat = 38.89),
  data.frame(state = "puerto rico", lon = -108, lat = 30)
)


state_centers


# Rename for join
state_centers <- state_centers %>%
  rename(region = state)


state_centers
# ──────────────── Join Plot Data ───────────────────────────────

state_plot_df <- tibble(
  state   = tolower(states),
  curve   = pred_tf,
  cluster = factor(cl_ind)
) %>%
  left_join(state_centers, by = c("state" = "region"))


state_plot_df

# ──────────────── US Map with Glyphs ───────────────────────────────

# ──────────────── US Map with Glyphs (Fixed) ───────────────────────────────

# Use usmap's state outlines
us_map_df <- us_map("states")



# Convert state_plot_df to sf object and transform to match us_map_df
state_plot_sf <- state_plot_df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84 (lat/lon)
  st_transform(st_crs(us_map_df)) %>%  # Transform to match us_map_df projection
  # Extract transformed coordinates
  mutate(
    x_proj = st_coordinates(.)[,1],
    y_proj = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()  # Remove geometry column to work with regular dataframe

p2_9 <- ggplot() +
  # First add the map layer using geom_sf
  geom_sf(data = us_map_df,
          fill = "white",
          color = "black",
          size = 0.2) +
  # Then add the functional data glyphs using transformed coordinates
  geom_capellini(data = state_plot_sf,
                 aes(x = x_proj, y = y_proj, tf = curve, colour = cluster),
                 width = 300000, height = 400000, line.linetype = 1) +  # Adjusted size for projected coords
  scale_color_manual(values = c("#AA00FF", "green", "#FF6D00"), name = "Cluster") +
  coord_sf() +
  theme_minimal() +
  labs(title = "US States Excess Mortality Curves with Cluster Colors",
       # or just "Longitude"
       x = "Longitude (°)",  
       ,# or just "Latitude"
       y = "Latitude (°)" )


grid.arrange(p1_9 , p2_9, ncol=2)
