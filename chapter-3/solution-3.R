## read in the data (assumes data are in the current working directory)
df_subj <- read_rds(here::here("data","nhanes_fda_with_r.rds"))
df_subj
## filter out participants 80+ and younger than 5
df_subj <-
  df_subj %>% 
  filter(age >= 5, age < 80)

df_subj
## Do fPCA on the subject-average MIMS profiles
MIMS_mat <- unclass(df_subj$MIMS)
MIMS_mat
fpca_MIMS_subj <- fpca.face(MIMS_mat)
fpca_MIMS_subj

## Do PCA on the subject-average MIMS profiles
# subtract column (minute) means to center the "varianbles"
MIMS_mn     <- colMeans(MIMS_mat)
MIMS_mn
MIMS_mat_cn <- sweep(MIMS_mat, MARGIN=2, STATS=MIMS_mn, FUN="-")
MIMS_mat_cn
# then do SVD
#svd_MIMS_subj <- svd(MIMS_mat_cn)



## plot the results of PCA vs fpca
sind <- seq(0,1,len=1440)
sind
xinx <- (c(1,6,12,18,23)*60+1)/1440
xinx
xinx_lab <- c("01:00","06:00","12:00","18:00","23:00")
df_plt <- 
  data.frame("PC" = rep(paste0("PC",1:4), each=1440),
             "time" = rep(sind, 4),
             "fPCA" = as.vector(fpca_MIMS_subj$efunctions[,1:4]),
             "PCA" = -as.vector(svd_MIMS_subj$v[,1:4]))
df_plt

df_plt <- 
  df_plt %>% 
  pivot_longer(cols=c("fPCA","PCA"))


fpca_vs_pca_NHANES <- 
  df_plt %>% 
  mutate(name = factor(name, levels=c("fPCA","PCA"),labels=c("(A) fPCA","(B) PCA"))) %>% 
  ggplot() + 
  geom_line(aes(x=time, y=value, color=PC), lwd=1.25) + 
  facet_grid(~name) + 
  theme_minimal(base_size=18) + 
  xlab("Time of Day (s)") + 
  ylab(expression("Estimated Eigenfunctions (" ~ phi[k](s) ~ ")")) + 
  scale_x_continuous(breaks=xinx, labels=xinx_lab) + 
  theme(legend.position=c(.9,.9),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(linewidth = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(linewidth = 1, linetype = "solid", colour = "black"),
        strip.text = element_text(face="bold", size=18, hjust=0.03),
        axis.ticks.x = element_line(linewidth = 1),
        axis.ticks.y = element_line(linewidth = 1)) + 
  labs(color="Eigenfunction") + 
  guides(color=guide_legend(ncol=2))


fpca_vs_pca_NHANES












# Interpreting the functional PCs may be challenging, particularly for PCs which explain a relatively low proportion of variance. 
# One visualization technique is to plot the distribution of curves which load lowest/highest on a particular PC.
# Here, we plot the individuals in the bottom and top 10% of scores for the first four PCs. The code below calculates these quantities.


## plot individual curves loading highly/lowly on the first 4 PCs
# set the seed for reproducibility
set.seed(1983)
# number of eigenfunctions to plot
K <- 4
# number of sample curves to plot for each PC
n_plt <-  10
# create container data frame for storing/plotting the individual curves
# for high/low loadings on each of the K PCs
# this data frame will have 
#  2 * K * n_plt * 1440 rows
#  2 = high/low
#  K = # of PCs
#  n_plt = number of individual curves to plot
#  1440 = minutes in a day
df_plt_ind <- expand.grid("sind" = sind,
                          "id" = 1:n_plt,
                          "high" = c("high","low"),
                          "PC" = paste0("PC ",1:4)) %>% 
  mutate(high = relevel(high, ref="low"))
# create container data frame for storing/plotting the average curves
# for high/ow loadings on each of the first K PCs
# this data frame will have 
#  2 * K * 1440 rows
#  2 = high/low
#  K = # of PCs
#  1440 = minutes in a day
df_plt_ind_mu <- expand.grid("sind" = sind,
                             "high" = c("high","low"),
                             "PC" = paste0("PC ",1:4),
                             "value" = NA)%>% 
  mutate(high = relevel(high, ref="low"))
# NOTE: the code below assumes an ordering of rows implied by the construction
#       used by the expand.grid function

# create empty vectors to store the average and individual curves, respectively
mu_vec <- c()
ind_vec <- c()
# loop over eigenfunctions
for(k in 1:K){
  # get the 10th and 90th percentiles of the scores
  q_k <- quantile(fpca_MIMS_subj$scores[,k],c(0.1,0.9))
  # get indices associated with low/high scores
  inx_low_k <- which(fpca_MIMS_subj$scores[,k] <= q_k[1])
  inx_high_k <- which(fpca_MIMS_subj$scores[,k] > q_k[2])
  # get the average curves in each quantile: 0-.1, .9-1
  mu_low_k <- colMeans(fpca_MIMS_subj$Yhat[inx_low_k,])
  mu_high_k <- colMeans(fpca_MIMS_subj$Yhat[inx_high_k,])
  # concatenate with existing mean vector
  mu_vec <- c(mu_vec, mu_low_k, mu_high_k)
  # get individual curves for the n_plt randomly selected participants
  val_low_k <- as.vector(t(fpca_MIMS_subj$Yhat[sample(inx_low_k, size=n_plt),]))
  val_high_k <- as.vector(t(fpca_MIMS_subj$Yhat[sample(inx_high_k, size=n_plt),]))
  # concatenate with existing individual vector
  ind_vec <- c(ind_vec, val_low_k, val_high_k)
}
# add the mean and individual vectors into the data frames constructed before the loop
df_plt_ind_mu$value <- mu_vec
df_plt_ind$value <- ind_vec






plt_ind <- 
  df_plt_ind %>% 
  mutate(id = factor(id),
         id_high = paste0(id,"_",high)) %>%
  ggplot() + 
  geom_line(aes(x=sind, y=value, group=id_high, color=high),alpha=0.5) + 
  facet_wrap(~PC,ncol=2) + 
  theme_minimal(base_size=18) + 
  scale_x_continuous(breaks=xinx, labels=xinx_lab) + 
  theme(legend.position="none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        strip.text = element_text(face="bold", size=18, hjust=0.03),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y = element_line(size = 1)) + 
  xlab("Time of Day (s)") + ylab(expression("MIMS: " ~ W[i](s))) + 
  geom_line(aes(x=sind, y=value, group=high, color=high),
            data=df_plt_ind_mu, lwd=2) 
## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
## ℹ Please use the `linewidth` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
plt_ind