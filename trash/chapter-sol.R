#Load packages
library(refund)
library(fields)



CV19 <- COVID19

#Date indicating weeks from the beginning of 2020
current_date <- CV19$US_weekly_excess_mort_2020_dates
#Names of states and territories considered in the analysis
new_states <- CV19$US_states_names
#Excess mortality as a function of time and state
Wd <- CV19$States_excess_mortality_per_million
#Columns are weeks, rows are states
colnames(Wd) <- 1:52
#Population of states  
pop_state_n <- CV19$US_states_population
names(pop_state_n) <- new_states



par(mfrow = c(1, 1))
cmar <- c(4, 4, 1, 1)
par(mar = cmar)
for(i in 1:length(new_states)){
  ylabel = paste("US states weekly excess deaths/million")
  xlabel = paste("Weeks starting January 2020")
  #Plot only for first state. For others add lines
  if(i == 1){
    par(bg = "white")
    #Here plot the date versus cumulative excess mortality (hence the cumsum)
    plot(current_date, Wd[i,], type = "l", lwd = 1.5, 
         col = rgb(0, 0, 0, alpha = 0.1), cex = 1, xlab = xlabel, 
         ylab = ylabel, ylim = c(-50, 400), bty = "n")
  }
  else
  {lines(current_date, Wd[i,], lwd = 1, col = rgb(0, 0, 0, alpha = 0.1))}
}

emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
col_emph <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")

emph_state_ind <- match(emphasize, new_states)

for(i in 1:length(emphasize)){
  lines(current_date, Wd[emph_state_ind[i],], lwd = 2.5, col = col_emph[i])
}





rownames(Wd) <- new_states
set.seed(1000)
kmeans_CV19_3 <- kmeans(Wd, centers = 3)
cl_ind <- kmeans_CV19_3$cluster
cl_cen <- kmeans_CV19_3$centers



# ────────── Hierarchical clustering of functional data──────────────────────
library(gplots) ##Available from CRAN
library(RColorBrewer)
library(viridis)
library(dendextend)
#Calculate the matrix of distances
dM <- dist(Wd[,]) ^ 2
#Hierarchical clustering on the square Euclidian distances
hc <- hclust(dM, method = "ward.D2")



#Set the cluster colors (five clusters)
clust.col <- c("#E69A8DFF", "#F6D55C", "#2A9D8F", "#5F4B8BFF", "#ee7600")

#Set the dendogram
hcd <- as.dendrogram(hc)
hcd <- hcd %>%
  color_branches(k = 5, col = clust.col) %>%
  set("branches_lwd", c(2, 2, 2, 2, 2)) %>%
  set("branches_lty", c(1, 1, 1, 1, 1))

cmar <- c(4, 4, 1, 1)
par(mar = cmar)
nodePar <- list(lab.cex = 0.6, pch = NA)
plot(hcd, nodePar = nodePar, axes = FALSE, ylab = "Distance (Ward.D2)", 
     ylim = c(0, 750000), cex.lab = 0.6)
axis(2, at = c(0, 250000, 500000, 750000), 
     labels = c("0", "250K", "500K", "750K"), cex.axis = 0.6, lwd = 1)



# A complementary way of plotting these results is to display the heatmap of the data together with the clustering
# of the rows of the matrix (states). The clustering re-orders the states (which were organized in 
# alphabetic order) to match with the hierarchical clustering. We could have clustered the
# columns (weeks) as well, but in this application we are interested in preserving the natural flow of time. 
# Here we used the function Heatmap in the R package ComplexHeatmap. Note that the capital letter H in Heatmap matters! Other functions we have tried seemed more finicky, but the user is encouraged to try other packages and heatmap functions



par(mfrow = c(1, 1))
par(mar = c(4, 4, 4, 4))

# #This package is on Bioconductor but not on Cran
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ComplexHeatmap")
# ## Warning: package(s) not installed when version(s) same as or greater than current; use
# ##   `force = TRUE` to re-install: 'ComplexHeatmap'
# 
library(ComplexHeatmap)
 library(circlize)

#Set a set of breaks in the colors to account for the large outliers in New Jersey. 
breaks <- c(-50, seq(-40, 130, by = 1), 200, 250, 300)

#Often heatmaps can be heavly affected by outliers. 
#This requires careful mapping of colors
hmcol <- plasma(length(breaks))
mycol <- colorRamp2(breaks = breaks, col = hmcol)

cmar <- c(4, 4, 1, 1)
par(mar = cmar)
#Plot the heatmap with the row dendogram
Heatmap(Wd, name = "EMR", col = mycol,
        row_names_gp = gpar(fontsize = 8),
        column_names_gp = gpar(fontsize = 5), cluster_columns = FALSE, 
        cluster_rows = color_branches(hc, k = 5, col = clust.col))






cut_wardd2 <- cutree(hc, k = 5)
loc_cut <- cut_wardd2
loc_cut[cut_wardd2 == 2] <- 1
loc_cut[cut_wardd2 == 5] <- 2
loc_cut[cut_wardd2 == 1] <- 3
loc_cut[cut_wardd2 == 4] <- 4
loc_cut[cut_wardd2 == 3] <- 5
cut_wardd2 <- loc_cut

## load state date which contains FIPS code
data("statepop")

state_cluster <- data.frame(full = names(cut_wardd2), cluster = unname(cut_wardd2))
data_cluster <- statepop %>%
  left_join(state_cluster, by = "full") %>%
  select(fips, cluster)
data_cluster$cluster <- as.factor(data_cluster$cluster)

## make the US map
p <- plot_usmap(regions = "states", data = data_cluster, values = "cluster") +
  scale_fill_manual(name = "Cluster", values = clust.col) +
  labs(title = "") +
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold"))

cmar <- c(4, 4, 1, 1)
par(mar = cmar)
print(p)











t <- 1:dim(Wd)[2]

#Apply functional PCA using the FACE approach
results <- fpca.face(Y = Wd, Y.pred = Wd, center = TRUE, argvals = t,
                     knots = 35, pve = 0.99, var = TRUE)

#Obtain the eigenfunctions and eigenvalues
Phi <- results$efunctions
eigenvalues <- results$evalues

#Obtain the estimated covariance and correlation matrices
cov_est <- Phi %*% diag(eigenvalues) %*% t(Phi)
cor_est <- cov2cor(cov_est)

#Obtain the scores and the predicted functions
PC_scores <- results$scores
Pred <- results$Yhat
#Name to columns and rows of the covariance
colnames(cov_est) <- 1:52
rownames(cov_est) <- 1:52

colnames(cor_est) <- 1:52
rownames(cor_est) <- 1:52



rownames(PC_scores) <- new_states
set.seed(1000)
kmeans_CV19_3 <- kmeans(PC_scores[,1:3], centers = 3)
cl_ind_sc <- kmeans_CV19_3$cluster
cl_cen_sc <- kmeans_CV19_3$centers

#This shows that the two approaches to K-means provide identical results 
table(cl_ind, cl_ind_sc)



breaks <- c(seq(-2000, -1000, length.out = 10), seq(-900, 900, length.out = 100), 
            seq(1000, 2000, length.out = 10))
hmcol <- magma(length(breaks))
mycol <- colorRamp2(breaks = breaks, col = hmcol)
Heatmap(cov_est, col = mycol, name = "Covariance",  
        row_names_gp = gpar(fontsize = 5), 
        column_names_gp = gpar(fontsize = 5), cluster_columns = FALSE, 
        cluster_rows = FALSE)