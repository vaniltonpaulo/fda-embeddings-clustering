#Load all-cause US mortality data
library(refund)
data("COVID19")
CV19 <- COVID19
names(CV19)



#Extract weekly all-cause excess mortality between 
#2017-01-14 and 2020-12-26
counts_state <- CV19$US_weekly_mort
date_state <- CV19$US_weekly_mort_dates
#Set the labels for the plot
ylabel <- paste("Weekly all-cause deaths in the US (thousands)")
xlabel <- "Weeks starting in January 2017"

#Plot weekly all-cause US mortality 2017/2020
#Divide by 1000 to indicate numbers in thousands
par(bg = "white")
plot(date_state, counts_state / 1000, pch = 19,
     col = "blue", cex = 0.8, xlab = xlabel,
     ylab = ylabel, bty = "n")

#Define the shaded areas that are compared
#The red area is 2020 and the blue area is 2019
#These two areas are used to calculate the excess mortality
conty <- c(rep(floor(min(counts_state / 1000)), 2),
           rep(ceiling(max(counts_state / 1000)), 2))
contx_2019 <- c(as.Date("2019-01-01"), comp_time, 
                comp_time, as.Date("2019-01-01"))
contx_2020 <- c(as.Date("2020-01-01"), end_time,
                end_time, as.Date("2020-01-01"))
polygon(contx_2019, conty, col = rgb(0, 0, 1, alpha = 0.1), 
        border = NA)
polygon(contx_2020, conty, col = rgb(1, 0, 0, alpha = 0.1), 
        border = NA)



#Weekly US COVID-19 mortality data

#Extract the Covid-19 associated deaths in the US from
#01-04-2020 to 12-26-2020
cv19_deaths <- CV19$US_weekly_mort_CV19
cv19_dates <- CV19$US_weekly_mort_CV19_dates



#Obtain the US weekly excess mortality between 2020 and 2019
week_diff <- CV19$US_weekly_excess_mort_2020
current_date <- CV19$US_weekly_excess_mort_2020_dates

#Define the labels
ylabel <- paste("All-cause excess and COVID-19 deaths in the US")
xlabel <- paste("Weeks starting in January 2020")

#Plot excess weekly US all-cause excess mortality and COVID-19 mortality in 2019
par(bg = "white")
plot(current_date, week_diff, pch = 19,
     col = "blue", cex = 1, xlab = xlabel, 
     ylab = ylabel, bty = "n")
points(current_date, cv19_deaths, pch = 19, 
       col = "red", cex = 1)
legend(x = as.Date("2020-01-01"), y = 25000, 
       c("COVID-19", "All-cause excess"), 
       col = c("red", "blue"), 
       pch = 19, cex = 0.75, bty = "n")



#Plot of all-cause cumulative excess data



#Names of states + Puerto Rico + DC
new_states <- CV19$US_states_names
state_population <- CV19$US_states_population
states_excess <- CV19$States_excess_mortality

#Loop over all states and plot the number of weekly all-cause mortality deaths per million
for(i in 1:length(new_states)){
  #Excess mortality 
  week_diff <- states_excess[i,]
  
  #Normalize to rate per 1000000 people
  week_diff <- 1000000 * week_diff / state_population[i]
  
  ylabel <- paste("US states cumulative excess deaths/million")
  xlabel <- paste("Weeks starting January 2020")
  #Plot only for first state. For others add lines
  if(i == 1){
    par(bg = "white")
    #Here plot the date versus cumulative excess mortality (hence the cumsum)
    plot(current_date, cumsum(week_diff), type = "l", lwd = 1.5,
         col = rgb(0, 0, 0, alpha = 0.1), cex = 1, xlab = xlabel,
         ylab = ylabel, ylim = c(-50, 2500), bty = "n")
  }else{
    lines(current_date,cumsum(week_diff),lwd=1,col=rgb(0, 0, 0, alpha = 0.1))
  }
}

#Overplotting New Jersey, Louisiana, California, Maryland, and Texas
#We add this to emphasize some states, as overploting can become an issue even with only 50 states
emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
col_emph <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")
for(i in 1:length(emphasize)){
  state_n <- emphasize[i]
  index_state <- which(new_states == state_n)
  
  #Extract variables to be plotted
  counts_state <- states_excess[index_state,]
  week_diff <- 1000000*counts_state / state_population[index_state]
  lines(current_date, cumsum(week_diff), lwd = 2.5, col = col_emph[i])
}


#Plot of COVID-19 cumulative data




states_CV19_mort <- CV19$States_CV19_mortality
#Loop over all states and plot the number of weekly cumulative all-cause mortality deaths per million
for(i in 1:length(new_states)){
  cv19_deaths <- states_CV19_mort[i,]
  cv19_deaths <- 1000000 * cv19_deaths / state_population[i]
  cv19_deaths <- cumsum(replace_na(cv19_deaths, 0))
  
  ylabel <- paste("US states cumulative COVID-19 deaths/million")
  xlabel <- paste("Weeks starting January 2020")
  if(i == 1){
    par(bg = "white")
    plot(current_date, cv19_deaths, type = "l", lwd = 1.5,
         col = rgb(0, 0, 0, alpha = 0.1), cex = 1, xlab = xlabel, 
         ylab = ylabel, ylim = c(-50, 2500), bty = "n")
  }else{
    lines(current_date, cv19_deaths, lwd = 1, col = rgb(0, 0, 0, alpha = 0.1))
  }
}

#Overplotting Maryland, California, Florida, New York
emphasize <- c("New Jersey", "Louisiana", "California", "Maryland", "Texas")
col_emph <- c("darkseagreen3", "red", "plum3", "deepskyblue4", "salmon")
for(i in 1:length(emphasize)){
  state_n <- emphasize[i]
  index_state <- which(new_states == state_n)
  
  #Extract variables to be plotted
  cv19_deaths <- states_CV19_mort[index_state,]
  cv19_deaths <- 1000000 * cv19_deaths / state_population[index_state]
  cv19_deaths <- cumsum(replace_na(cv19_deaths, 0))
  lines(current_date, cv19_deaths, lwd = 2.5, col = col_emph[i])
}