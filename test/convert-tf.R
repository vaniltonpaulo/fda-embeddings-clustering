#-------------------------------------------------------------------------------
#Converting to & from `tf`
#-------------------------------------------------------------------------------
library(fda)
#library(SemiPar)  this package is not available in cran
install.packages("ALA", repos = "http://R-Forge.R-project.org")

data(dti_df, package = "tidyfun")
glimpse(dti_df)
dim(dti_df)
dim(refund::DTI$cca)
#In this vignette, we illustrate how to convert data from common structures to tf objects.

#Throughout, functional data vectors are stored as columns in a data frame to facilitate subsequent wrangling and analysis.

# In the next examples, we’ll use tfd() to turn a matrix into a special kind of object called a tf vector,
#which holds functional data (like curves).
# To do this, the matrix should be set up so that each row is one subject’s curve (for example, one person’s brain scan over time).

# While we could just use the tf vector by itself, we’ll usually attach it as a column in a data frame—
#because that fits better with how tidyfun is designed to work (like tidyverse: data in tables, one row per observation).


#EXAMPLE

# This code creates a new tibble (which is just a nicer version of a data frame) called dti_df.
# #It pulls in data from the built-in dataset DTI (which comes from the refund package), and organizes just a few columns like this:
# id: the subject ID
# visit: the visit number (probably which time they were scanned)
# sex: the person's sex
#case: whether the person has MS (multiple sclerosis) or is a control (healthy)
#"If case is TRUE, label it 'MS'; otherwise, label it 'control'. Then turn those labels into a factor

#So in short: this builds a clean table with subject info and whether or not they have MS.
dti_df <- tibble(
  id = refund::DTI$ID,
  visit = refund::DTI$visit,
  sex = refund::DTI$sex,
  case = factor(ifelse(refund::DTI$case, "MS", "control")
)
)


# This adds a new column to dti_df called cca.
# Here’s what’s happening:
#   refund::DTI$cca is a matrix of functional data—each row is one person, and each column is a measurement point along a curve.
# tfd(...) turns that matrix into a tf object (a tidyfun-friendly version of functional data).
# arg = seq(0, 1, length.out = 93) means the curves are defined on a grid from 0 to 1, with 93 equally spaced points.

# So basically:
#   “Take the brain scan curves from the cca data, turn them into tidy functional objects, and add them to the data frame.”

#For this stuff is always easier to check
dim(refund::DTI$cca)
dim(refund::DTI$rcst)


dti_df$cca <- tfd(refund::DTI$cca, arg = seq(0, 1, length.out = 93))
dti_df$rcst <- tfd(refund::DTI$rcst, arg = seq(0, 1, length.out = 55))






dti_df



dti_df |>
  ggplot() +
  geom_spaghetti(aes(y = cca, col = case, alpha = 0.2 + 0.4 * (case == "control"))) +
  facet_wrap(~sex) +
  scale_alpha(guide = "none", range = c(0.2, 0.4))




#EXAMPLE
#the Canadian weather data

data(CanadianWeather, package = "fda")
typeof(CanadianWeather)
summary(CanadianWeather)
str(CanadianWeather)




# You're building a data frame of Canadian weather stations, and for each one:
# You include its name, region, and location
# You add a smooth curve of its daily temperature
# You add a smooth curve of its heavy precipitation (over 10mm)
# This gives you a tidy table where each row is a weather station, and the curves (temp, precipl10) 
#are stored as tidy functional data—ready for plotting or analysis.

# temp
# Takes the average daily temperature (for each day of the year)
# [, , 1] pulls the temperature array from the data (it's a 3D array: station × day × variable)
# t() transposes it, so rows = stations and columns = days
# tfd(arg = 1:365) turns this matrix into a functional object over 365 days


# precipl10
# Takes the precipitation on days with >10mm, from the same dataset
# [, , 3] selects that variable
# Again transposes and converts it to a functional object with tfd(arg = 1:365)
# Then smooths the curves using tf_smooth(), to make them less jagged




canada <- tibble(
  place = fda::CanadianWeather$place,
  region = fda::CanadianWeather$region,
  lat = fda::CanadianWeather$coordinates[, 1],
  lon = -fda::CanadianWeather$coordinates[, 2] #(note: longitude is made negative so it's in proper western hemisphere coordinates)
) |>
  mutate(
    temp = t(fda::CanadianWeather$dailyAv[, , 1]) |>
      tfd(arg = 1:365),
    precipl10 = t(fda::CanadianWeather$dailyAv[, , 3]) |>
      tfd(arg = 1:365) |>
      tf_smooth()
  )
## using f = 0.15 as smoother span for lowess


canada



temp_panel <- canada |>
  ggplot(aes(y = temp, color = region)) +
  geom_spaghetti()

precip_panel <- canada |>
  ggplot(aes(y = precipl10, color = region)) +
  geom_spaghetti()

gridExtra::grid.arrange(temp_panel, precip_panel, nrow = 1)


#-------------------------------------------------------------------------------
#Conversion to tf from a data frame
#… in “long” format
#-------------------------------------------------------------------------------


# Sometimes, your functional data is in a “long” format, which means:
#   
#   Each row shows one value of a curve for one person at one point (like day 23, or time 1.5)
# 
# The data has columns like:
#   
#   A person ID (id)
# 
# The input (arg) — like time or day
# 
# The output (value) — what the curve equals at that point
# 
# (Maybe) some extra info like age or gender — repeated for each row of that person
# 
# This kind of layout is useful for some things, but often we want it tidy with just one row per person, and their whole curve stored as a single object.
# 
# 👉 That’s what tf_nest() does:
#   
#   It takes the long-format data and nests each person’s curve into one tidy row.
# 
# So in short:
#   Long = lots of rows per person → tf_nest() = one row per person with a full curve.




#EXAMPLE ---->the package is non existent
data("pig.weights", package = "SemiPar")

pig.weights <- as_tibble(pig.weights)

pig.weights


#We create pig_df by nesting weight within subject. The result is a data frame containing a single row for each pig, 
#and columns for id.num and the weight function.


#EXAMPLE  ---->ERRORS


ALA::fev1 |> glimpse()

ALA::fev1 |>
  group_by(id) |>
  mutate(n_obs = n()) |>
  filter(n_obs > 1) |>
  tf_nest(logFEV1, height, .arg = age) |>
  glimpse()


#-------------------------------------------------------------------------------
#Conversion to tf from a data frame
#… in “wide” format
#-------------------------------------------------------------------------------
# Sometimes, functional data comes in a “wide” format, which means:
#   
#   Each row is one person
# 
# Each column is a value of the curve at a certain point (like Day 1, Day 2, ..., Day 365)
# 
# So the curve is spread across many columns
# 
# This is the opposite of “long” format.
# 
# 👉 To turn this into tidy functional data, we can use tf_gather(), which:
#   
#   Takes all those separate columns and gathers them into a single function for each person.
# 
# So in short:
#   Wide = one row per person, one column per time point → tf_gather() turns it into a functional column.

dti_df <- refund::DTI |>
  janitor::clean_names() |>
  select(-starts_with("rcst")) |>
  glimpse()


dti_df |>
  tf_gather(starts_with("cca")) |>
  glimpse()
