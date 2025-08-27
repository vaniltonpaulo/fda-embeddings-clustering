library(face)
library(refund)

#Load the data
data(cd4)
n <- nrow(cd4) 
T <- ncol(cd4)

#Construct a vectorized form of the data
id <- rep(1:n, each = T)
t <- rep(-18:42, times = n) 
y <- as.vector(t(cd4))

#Indicator for NA observations. This takes advantage of the sparse nature of the data
sel <- which(is.na(y))
#Organize data as outcome, time, subject ID 
data <- data.frame(y = log(y[-sel]), argvals = t[-sel],
                   subj <- id[-sel])
data <- data[data$y > 4.5,]
#Provide the structure of the transformed data
head(data)
colnames(data)
##          y argvals subj....id..sel.
## 1 6.306275      -9                1
## 2 6.794587      -3                1
## 3 6.487684       3                1
## 4 6.622736      -3                2
## 5 6.129050       3                2
## 6 5.198497       9                2

#Fit the sparse smoother face.sparse
#This call extracts the scores, as well
fit_face <- face.sparse(data, argvals.new = (-20:40), 
                        calculate.scores = TRUE, 
                        newdata = data, pve = 0.95)

fit_face$eigenfunctions
fit_face$rand_eff$scores

#Obtain the scores. Clustering could be done on the scores
#This time we will conduct clustering on the predicted functions
scores <- fit_face$scores$scores
scores

data.h <- data

tnew <- fit_face$argvals.new
tnew


#Construct the predicted functions for each study participant. This is obtained from the fpca.sparse function fit.


#Extract the id vector and the vector of unique ids
id <- data.h$subj
id
uid <- unique(id)
uid

#Set the grid where to predict
seq <- -20:40
k <- length(seq)
#Set the matrix that contains the predictions
Pred_mat <- matrix(rep(NA, n*k), ncol = k)

#Predict every curve
for(i in 1:n){ #Begin loop over study participants
  #Select the i-th study participant  
  sel <- which(id == uid[i])
  dati <- data.h[sel,]
  
  #Set the frmework for data prediction
  #With this framework it predicts at the grid and where observations were taken
  #This is why vectors are longer than the length of the grid
  dati_pred <- data.frame(y = rep(NA, nrow(dati) + k ),
                          argvals = c(rep(NA, nrow(dati)), seq), 
                          subj = rep(dati$subj[1], nrow(dati) + k )
  )
  
  #This is where the data is populated
  dati_pred[1:nrow(dati),] <- dati
  yhat2 <- predict(fit_face, dati_pred)
  
  #Extract just the predictions on the grid
  Ord <- nrow(dati) + 1:k
  temp_pred <- yhat2$y.pred[Ord]
  Pred_mat[i,] <- temp_pred
}



#Conduct clustering of predicted functions from fpca sparse



set.seed(202200228)
cl_kmeans_CD4 <- kmeans(Pred_mat, centers = 3)
cl_kmeans_CD4
cl_ind_CD4 <- cl_kmeans_CD4$cluster
cl_cen_CD4 <- cl_kmeans_CD4$centers



#Plot the results of predicted CD4 curves together with the estimated clustering as well as centers of the clusters.

par(mar = c(4, 4, 1, 1))
colset <- c("#E69A8DFF", "#F6D55C", "#2A9D8F", "#5F4B8BFF")
plot(NULL, xlim = c(-20, 40), ylim = c(5, 8), xlab = "Time from seroconversion (months)", 
     ylab = "Log CD4 counts", bty = "n")

for(i in 1:n){
  lines(seq, Pred_mat[i,], col = colset[cl_ind_CD4[i]])
}

lines(seq, cl_cen_CD4[1,], col = "darkred", type = "l", lwd = 3)
lines(seq, cl_cen_CD4[2,], col = "darkorange", lwd = 3)
lines(seq, cl_cen_CD4[3,], col = "darkgreen", lwd = 3)