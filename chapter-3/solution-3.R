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
svd_MIMS_subj <- svd(MIMS_mat_cn)



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