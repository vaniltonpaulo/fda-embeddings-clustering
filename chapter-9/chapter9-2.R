#helpers

tf_join_clusters_tf <- function(pred_tf, cluster_vec) {
  tibble(
    id      = seq_along(pred_tf),
    cluster = factor(cluster_vec),
    curve   = pred_tf
  )
}

tf_plot_clusters_tf <- function(cluster_tbl, centers_tbl) {
  ggplot() +
    ## highlight centres
    geom_spaghetti(
      data = centers_tbl,
      aes(y = curve, group = cluster),
      colour = "black", linewidth = 1
    ) +
    geom_spaghetti(
      data = centers_tbl,
      aes(y = curve, colour = cluster, group = cluster),
      linewidth = 1
    ) +
    ## individual subjects
    geom_spaghetti(
      data = cluster_tbl,
      aes(y = curve, colour = cluster, group = id),
      alpha = .15, linewidth = .7
    ) +
    scale_color_manual(
      #values = c("#D55E00", "#009E73", "#F0E442"),
      values = c("darkred", "darkorange", "darkgreen"),
      name   = "Cluster"
    ) +
    labs(
      x = "Time from seroconversion (months)",
      y = "Log CD4 counts"
    ) +
    theme_minimal(base_size = 14)
}





# ──────────────── K-means clustering of the functional data ──────────────────────────────

#dates <- as.Date(dates)  # Ensure it's Date class
#tnum <- as.numeric(dates - min(dates))

#logic taken from the book. rember the wheel...
rownames(Wd) <- states
set.seed(1000)
km    <- kmeans(Wd, centers = 3)
cl_ind <- km$cluster
cl_ind
cl_cen <- km$centers
cl_cen




# Use their exact dates and numeric conversion with fixed reference
#dates <- as.Date(CV19$US_weekly_excess_mort_2020_dates)
#reference_date <- as.Date("2020-01-01")  # Force reference to Jan 1, 2020
#tnum <- as.numeric(dates - reference_date)


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
      end_date <- max(dates)
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

#I really dont know how to improve such plots or what to do with besides this.
#this is also take from the book.
#But at least code is working as it supposed to.

#install.packages("usmap")
library(usmap)

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


library(sf)

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
