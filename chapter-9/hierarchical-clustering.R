# ────────── Hierarchical clustering of functional data──────────────────────
library(factoextra)

# 1. Compute distances and cluster as before
dist_matrix <- stats::dist(Wd)^2
hc          <- hclust(dist_matrix, method = "ward.D2")

# 2. Build a palette‐generating function from your exact hex codes
base_cols   <- c("#E69A8DFF", "#F6D55C", "#2A9D8F", "#5F4B8BFF", "#ee7600")
my_pal_fun  <- colorRampPalette(base_cols)

# 3. Actually generate exactly k = 5 colors
k_clusters  <- 5
cluster_cols <- my_pal_fun(k_clusters)

# 4. Pass that vector to the `palette=` argument
fviz_dend(hc,
          k          = k_clusters,
          rect       = TRUE,        # draw cluster rectangles
          rect_fill  = TRUE,        # fill them
          palette    = cluster_cols,# YOUR custom palette here
          lwd        = 1.5,         # line width
          cex        = 0.5,         # label size
          main       = "Hierarchical Clustering Dendrogram",
          xlab       = "Observations",
          ylab       = "Distance (Ward D2)",
          ggtheme    = theme_minimal())+scale_y_continuous(labels = function(x) {
            ifelse(x == 0, "0", paste0(scales::comma(x/1000), "K"))
          })





# ─── Packages ────────────────────────────────────────────────────────────────
library(refund)     # for COVID19 data
library(tidyfun)    # for tfd() + gglasagna()
library(dplyr)      # for tibble() + mutate()
library(ggplot2)    # for facets & scales
library(viridis)    # for plasma()

# ─── Data prep ──────────────────────────────────────────────────────────────
states <- COVID19$US_states_names
Wd     <- COVID19$States_excess_mortality_per_million
dates  <- as.Date(COVID19$US_weekly_excess_mort_2020_dates)

# Numeric time: days since 2020-01-01
tnum <- as.numeric(dates - as.Date("2020-01-01"))

# ─── Hierarchical clustering ─────────────────────────────────────────────────
rownames(Wd)  <- states
hc             <- hclust(dist(Wd)^2, method = "ward.D2")
state_order    <- rownames(Wd)[hc$order]            # the 52 states in dendrogram order
row_order_vec  <- match(states, state_order)        # for each state, its position in that order

# ─── Build tidyfun tibble ───────────────────────────────────────────────────
df_tf <- tibble(
  state     = states,
  mortality = tfd(Wd, arg = tnum),   # wrap your 52-length vectors as `tf`
  row_order = row_order_vec          # numeric: 1–52 in cluster order
)

# ─── Plot with gglasagna() ───────────────────────────────────────────────────


# Replicate the original's color strategy
breaks_orig <- c(-50, seq(-40, 130, by = 1), 200, 250, 300)
colors_orig <- viridis::plasma(length(breaks_orig))

gglasagna(
  df_tf,
  tf    = mortality,
  order = row_order,
  label = state
) +
  # Use the exact same breaks and colors as the original
  scale_fill_gradientn(
    colors = colors_orig,
    values = scales::rescale(breaks_orig),  # Map breaks to 0-1 scale
    breaks = c(-50, 0, 50, 100, 150, 200, 250, 300),
    limits = c(-50, 300),
    name = "EMR"
  ) + scale_x_continuous(
    breaks = function(x) {
      ref <- as.Date("2020-01-01")
      qs  <- seq(ref, max(dates), by = "3 months")
      as.numeric(qs - ref)
    },
    labels = function(x) {
      ref <- as.Date("2020-01-01")
      format(ref + x, "%b %Y")
    },
    name = "Weeks since Jan 1, 2020"
  ) +
  labs(
    title = "Heatmap of 2020 Weekly Excess Mortality\nStates ordered by hierarchical clustering"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y      = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



#################checks
head(data.frame(
  original_state = states,
  cluster_position = row_order_vec,
  clustered_state = state_order[row_order_vec]
))



# Check if tfd() is preserving the original values
range(Wd, na.rm = TRUE)  # Original data range
range(as.matrix(df_tf$mortality), na.rm = TRUE)  # Your tfd data range








#this needs to be improved
# IMPROVED: More systematic cluster relabeling
cut_wardd2 <- cutree(hc, k = 5)

# Get the order of clusters as they appear in the dendrogram (left to right)
dend_order <- hc$order
cluster_first_appearance <- sapply(1:5, function(k) {
  which(dend_order %in% which(cut_wardd2 == k))[1]
})
cluster_reorder <- rank(cluster_first_appearance)

# Relabel clusters based on dendrogram order
relabeled_clusters <- cluster_reorder[cut_wardd2]

# IMPROVED: Better color palette and naming
clust.col <- c("#E69A8DFF", "#F6D55C", "#2A9D8F", "#5F4B8BFF", "#ee7600")
cluster_names <- c("Early Peak", "Sustained High", "Late Peak", "Moderate", "Low Impact")

# IMPROVED: Enhanced data preparation
data("statepop")
state_cluster <- data.frame(
  full = names(cut_wardd2), 
  cluster = relabeled_clusters,
  cluster_name = cluster_names[relabeled_clusters]
)

data_cluster <- statepop %>%
  left_join(state_cluster, by = "full") %>%
  select(fips, cluster, cluster_name) %>%
  mutate(cluster = as.factor(cluster))

# IMPROVED: Enhanced map with better styling
p_improved <- plot_usmap(regions = "states", data = data_cluster, values = "cluster") +
  scale_fill_manual(
    name = "Mortality Pattern", 
    values = clust.col,
    labels = cluster_names
  ) +
  labs(
    title = "COVID-19 Excess Mortality Clusters",
    subtitle = "States grouped by temporal mortality patterns",
    caption = "Based on hierarchical clustering of weekly excess mortality data"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  guides(fill = guide_legend(nrow = 1))

print("=== IMPROVED ORIGINAL MAP ===")
print(p_improved)




# Load required libraries
library(mclust)
library(usmap)
library(dplyr)
library(ggplot2)

# Set color palette
cluster_colors <- c("#E69A8DFF", "#F6D55C", "#2A9D8F", "#5F4B8BFF")

# Center and scale data
X_scaled <- scale(Wd) %>% as.data.frame()

# Fit Gaussian Mixture Model using BIC
bic <- mclustBIC(X_scaled)
gmm_model <- Mclust(X_scaled, x = bic)
clusters <- gmm_model$classification

# Merge clustering results with state population data
state_clusters <- data.frame(full = names(cut_wardd2), cluster = clusters)

data_cluster <- statepop %>%
  left_join(state_clusters, by = "full") %>%
  mutate(cluster = factor(cluster)) %>%
  select(fips, cluster)

# Plot the US map
us_map <- plot_usmap(regions = "states", data = data_cluster, values = "cluster") +
  scale_fill_manual(name = "Cluster", values = cluster_colors) +
  labs(title = "") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Print the map with adjusted margins
par(mar = c(4, 4, 1, 1))
print(us_map)




# ────────── first three principal components──────────────────────
library(ggplot2)
library(tidyfun)
library(tibble)
library(dplyr)

# Time in weeks since Jan 1, 2020
reference_date <- as.Date("2020-01-01")
tnum <- as.numeric(dates - reference_date)

# Transpose eigenfunctions to PC x time
Phi_t <- t(results$efunctions[, 1:3])


explained <- results$evalues[1:3] / sum(results$evalues)
pc_labels <- paste0("PC", 1:3, " (", round(100 * explained, 1), "%)")

# Build tidy tibble
# pc_df <- tibble(
#   pc = paste0("PC", 1:3),
#   efun = tfd(Phi_t, arg = tnum)
# )
pc_df <- tibble(
  pc = factor(pc_labels, levels = pc_labels),
  efun = tfd(Phi_t, arg = tnum)
)

# Plot
ggplot(pc_df, aes(y = efun, color = pc)) +
  geom_spaghetti(alpha = 1) +
  scale_x_continuous(
    name = "Weeks starting January 2020",
    breaks = as.numeric(seq(reference_date, as.Date("2021-01-01"), by = "3 months") - reference_date),
    labels = format(seq(reference_date, as.Date("2021-01-01"), by = "3 months"), "%b %Y")
  ) +
  labs(
    y = "Eigenfunctions",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )
