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