#-------------------------------------------------------------------------------
#Main Page
#-------------------------------------------------------------------------------
#packages needed


# install.packages("pak")
#pak::pak("tidyfun/tidyfun")
library(tidyverse)
library("tidyfun")
library(refund)
library(viridisLite)
#library(viridis). Was depricated





#Crucially, vectors of class tf can be included in data frames containing other variables
#enabling data manipulation using tidyverse tools.


# This approach is connected to the conceptual framework in functional data analysis, 
# which assumes that complete functions are the unit of observation; with tidyfun, 
# full curves sit alongside numeric, factor, and other observations on the same subject.





#EDA

data(dti_df, package = "tidyfun")
glimpse(dti_df)

dti_df |>
  group_by(case, sex) |>
  summarize(mean_cca = mean(cca, na.rm = TRUE)) |>
  ggplot(aes(y = mean_cca, color = case)) +
  geom_spaghetti(size = 2) +
  facet_grid(~sex)


#What does it do?
#raw data (class tfd for tidy functional data) or in 
#basis representation (class tfb for tidy functional basis data), defined in the underlying {tf} package.

#tidyfun relies on tf package


#The tf objects are basically glorified lists, so they work well as columns in data frames. 
#That makes it a lot easier to keep your other data and functional measurements together in 
#one object for preprocessing, exploratory analysis and description.



#Methods for converting existing data to tf 

#tidyfun includes functions tfd and tfb for converting matrices, data frames, etc. to tf vectors. 
#It also provides tf_gather & tf_nest in order to reshape tables with functional data, by going from wide
#to narrow or from long to short; functions like as.matrix, tf_spread & tf_unnest can reverse these data conversions.




#-------------------------------------------------------------------------------
#tf Vectors and Operations
#-------------------------------------------------------------------------------

#tf is a new data type for (vectors of) functional data.Basically, a list of numeric vectors

#Example
data("dti_df")
#glimpse(dti_df)
#View(dti_df)

#Extracts the cca object from dti_df. This likely contains functional observations over some domain, e.g., [0,1].
cca <- dti_df$cca

# Takes the first 5 rows (observations).
# 
# Evaluates them at 93 equally spaced points in [0, 1].
# 
# interpolate = TRUE ensures smooth values at those points.
cca_five <- cca[1:5, seq(0, 1, length.out = 93), interpolate = TRUE]
rownames(cca_five) <- LETTERS[1:5]

#the dataset was converted to  tfd just before the ending of the skript
cca_five <- tfd(cca_five, signif = 2)
cca_five #raw data (class tfd for tidy functional data)
#typeof(cca_five)  Its in fact a list

pal_5 <- c("blue", "red", "green", "orange", "purple")
plot(cca_five, xlim = c(-0.15, 1), col = pal_5)
text(x = -0.1, y = cca_five[, 0.07], labels = names(cca_five), col = pal_5)



#tf subclass: tfd

#tfd objects contain “raw” functional data

cca_five |>
  tf_evaluations() |>
  str()

cca_five |>
  tf_arg() |>
  str()

cca_five |> tf_domain()

#each tfd-vector contains an evaluator function that defines how to inter-/extrapolate evaluations between args

tf_evaluator(cca_five) |> str()
## function (x, arg, evaluations)
tf_evaluator(cca_five) <- tf_approx_spline


#Another example
cd4_vec <- tfd(refund::cd4)

cd4_vec[1:2]


cd4_vec[1:2] |>
  tf_arg() |>
  str()

cd4_vec[1:20] |> plot(pch = "x", col = viridis(20))



#tf subclass: tfb

#represented as a list of coefficients and a common basis_matrix of basis function evaluations on a vector of arg-values.

#(internal) flavors:
#tfb_spline: uses mgcv-spline bases
#tfb_fpc: uses functional principal components


cca_five_b <- cca_five |> tfb()
cca_five_b[1:2]
cca_five[1:2] |> tfb(bs = "tp", k = 55)


cca_five[1:2] |>
  tfb(bs = "ps", m = c(2, 1), family = mgcv::betar(link = "cloglog"))


layout(t(1:2))
cca_five |> plot()
cca_five_b |> plot(col = "red")
cca_five |>
  tfb(k = 35, penalized = FALSE) |>
  lines(col = "blue")

cca_five |>
  tfb(sp = 0.001) |>
  lines(col = "orange")



#There are errors here
layout(t(1:3))
clrs <- scales::alpha(sample(viridis(15)), 0.5)
plot(raw, main = "raw", col = clrs)
plot(tfb(raw, k = 55), main = "separate", col = clrs)

plot(tfb(raw, k = 55, global = TRUE), main = "global", col = clrs)




#tfb FPC-based

cca_five_fpc <- cca_five |> tfb_fpc(pve = 0.999)
cca_five_fpc


cca_five_fpc_lowrank <- cca_five |> tfb_fpc(pve = 0.6)
cca_five_fpc_lowrank



layout(t(1:2))
cca_five |> plot()
cca_five_fpc |> plot(col = "red", ylab = "tfb_fpc(cca_five)")
cca_five_fpc_lowrank |> lines(col = "blue", lty = 2)


#tfb_fpc is currently only implemented for data on identical (but possibly non-equidistant) grids.
#The {refunder} rfr_fpca-functions provide FPCA methods appropriate for highly irregular and sparse data and regularized/smoothed FPCA.