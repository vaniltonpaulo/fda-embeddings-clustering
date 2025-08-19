#Load packages
library(readxl)
library(dplyr)
library(knitr)
library(formattable)
library(lubridate)
library(tidyr)


library(refund)
data("COVID19")
CV19 <- COVID19
names(CV19)
##  [1] "US_weekly_mort"                      "US_weekly_mort_dates"               
##  [3] "US_weekly_mort_CV19"                 "US_weekly_mort_CV19_dates"          
##  [5] "US_weekly_excess_mort_2020"          "US_weekly_excess_mort_2020_dates"   
##  [7] "US_states_names"                     "US_states_population"               
##  [9] "States_excess_mortality"             "States_excess_mortality_per_million"
## [11] "States_CV19_mortality"               "States_CV19_mortality_per_million"

Wd <- CV19$States_excess_mortality_per_million
current_date <- CV19$US_weekly_excess_mort_2020_dates
new_states <- CV19$US_states_names




#Singular value decomposition

#Obtain the cumulative excess mortality per million in every stat and territory
Wr <- Wd
for(i in 1:dim(Wd)[1]){
  Wr[i,] <- cumsum(Wd[i,])
}


#Column mean 
mW <- colMeans(Wr)

#Construct a matrix of same dimension as Wr with the mean of Wr on each row
mW_mat <- matrix(rep(mW, each = nrow(Wr)), ncol = ncol(Wr))

#Construct the de-meaned data. The columns of W have mean zero
W <- Wr - mW_mat

W[,1]




#Plot the data and the mean


for(i in 1:length(new_states)){
  ylabel <- paste("US states cumulative excess deaths/million")
  xlabel <- paste("Weeks starting January 2020")
  #Plot only for first state. For others add lines
  if(i == 1){
    par(bg = "white")
    #Here plot the date versus cumulative excess mortality (hence the cumsum)
    plot(current_date, Wr[i,], type = "l", lwd = 1.5, 
         col = rgb(0, 0, 0, alpha = 0.1), cex = 1, xlab = xlabel, 
         ylab = ylabel, ylim = c(-50, 2500), bty = "n")
  }else{
    lines(current_date, Wr[i,], lwd = 1, col = rgb(0, 0, 0, alpha = 0.1))
  }
}

lines(current_date, mW, lwd = 2.5, col = "darkred")



#Display the de-meaned data


for(i in 1:length(new_states)){
  ylabel=paste("US states centered cumulative excess deaths/million")
  xlabel=paste("Weeks starting January 2020")
  #Plot only for first state. For others add lines
  if(i==1){
    par(bg = "white")
    #Here plot the date versus cumulative excess mortality (hence the cumsum)
    plot(current_date, W[i,], type = "l", lwd = 1.5,
         col = rgb(0, 0, 0, alpha = 0.1), cex = 1, xlab = xlabel, 
         ylab = ylabel, ylim = c(-1500, 1500), bty = "n")
  }else{
    lines(current_date, W[i,], lwd = 1, col = rgb(0, 0, 0, alpha = 0.1))
  }
}

#Emphasize 5 states
emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
col_emph <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")

emph_state_ind <- match(emphasize, new_states)

for(i in 1:length(emphasize)){
  lines(current_date, W[emph_state_ind[i],], lwd = 2.5, col = col_emph[i])
}


#Calculate the SVD of 𝑊


#Calculate theSVDofW
SVD_of_W <- svd(W)
#Left singularvectorsstoredbycolumns
U <-SVD_of_W$u
#Singular values
d <-SVD_of_W$d
#Right singularvectorsstoredbycolumns
V <-SVD_of_W$v
#Calculate theeigenvalues
lambda <-SVD_of_W$d^2
#Individual proportionofvariation
propor_var <- round(100 * lambda / sum(lambda), digits = 1)
#Cumulative proportionofvariation
cumsum_var <- cumsum(propor_var)


plot(current_date, V[,1], type = "l", lwd = 2.5, 
     col = "coral", cex = 1, xlab = xlabel, ylim = c(-0.35, 0.35), 
     ylab = "Right singular vectors", bty = "n")

lines(current_date, V[,2], lwd = 2.5, col = "coral4")




#Reconstruction of the original data


#Set the approximation rank
K0 <- 2

#Reconstruct the de-meaned data using the first K0 right singular vectors
rec <- SVD_of_W$u[,1:K0] %*% diag(SVD_of_W$d[1:K0]) %*% t(V[,1:K0])

#Add the mean to the rank K0 approximation of W
WK0 <- mW_mat+rec






#Compare the real and reconstructed data

ylabel <- paste("US states cumulative excess deaths/million")

plot(current_date, Wr[emph_state_ind[1],], type = "l", lwd = 2.5, 
     col = col_emph[1], cex = 1, xlab = xlabel, 
     ylab = ylabel, ylim = c(-50, 2500), bty = "n")

lines(current_date, WK0[emph_state_ind[1],], lwd = 2.5, col = col_emph[1], lty = 2)

for(i in 2:length(emphasize)){
  lines(current_date, Wr[emph_state_ind[i],], lwd = 2.5, col = col_emph[i])
  lines(current_date, WK0[emph_state_ind[i],], lwd = 2.5, col = col_emph[i], lty = 2)
}


#Plot the scores on the first and second singular vectors


U <- SVD_of_W$u
plot(U[,1], U[,2], pch = 19, cex = 0.8, 
     xlab = "Scores on first right singular vector",
     ylab = "Scores on second right singular vector", bty = "n")

points(U[emph_state_ind, 1], U[emph_state_ind, 2], col = col_emph, pch = 19, cex = 1.5)

