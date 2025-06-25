
dates <- as.Date(dates)  # Ensure it's Date class


tnum <- as.numeric(dates - min(dates))



rownames(Wd) <- states
set.seed(1000)
km    <- kmeans(Wd, centers = 3)
cl_ind <- km$cluster
cl_cen <- km$centers

# Use their exact dates and numeric conversion with fixed reference
dates <- as.Date(CV19$US_weekly_excess_mort_2020_dates)
reference_date <- as.Date("2020-01-01")  # Force reference to Jan 1, 2020
tnum <- as.numeric(dates - reference_date)

# Create tfd objects
pred_tf    <- tfd(Wd, arg = tnum)
centers_tf <- tfd(cl_cen, arg = tnum)

# Create tibbles
cluster_tbl <- tf_join_clusters_tf(pred_tf, cl_ind)
centers_tbl <- tibble(
  cluster = factor(seq_len(nrow(cl_cen))),
  curve   = centers_tf
)

# Plot with corrected date handling
tf_plot_clusters_tf(cluster_tbl, centers_tbl) +
  scale_color_manual(
    values = c("#AA00FF", "#FF6D00", "#00C853"),
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







#install.packages("usmap")
library(usmap)
library(ggplot2)
library(tidyverse)

#Define a color per group
colset <- c(rgb(0.41, 0.05, 0.68), rgb(0, 1, 0), rgb(1, .55, 0))

## load state date which contains FIPS code
data("statepop")

## create a data frame to plot based on the input requirement
state_cluster <- data.frame(full = names(cl_ind), cluster = unname(cl_ind))
data_cluster <- statepop %>%
  left_join(state_cluster, by = "full") %>%
  select(fips, cluster)
data_cluster$cluster <- as.factor(data_cluster$cluster)

## make the US map
p <- plot_usmap(regions = "states", data = data_cluster, values = "cluster") +
  scale_fill_manual(name = "Cluster", values = colset) +
  labs(title = "") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold"))
print(p)