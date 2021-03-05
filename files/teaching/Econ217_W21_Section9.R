# ----------------------------- #
# Econ217 (J. Li, W2021)        #
# Section 9 (Mar. 3-5, 2021)    # NOTE: all the path directories are based on my own computer, 
# TA: David Sungho Park         #       so you should change them according to yours.
# ----------------------------- #

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to where R script is saved

###################################################################################
# Conditional average treatment effects
###################################################################################
rm(list = ls())
library(matrixStats)
library(grf)

set.seed(1)

star.data<-read.csv('star.csv')
attach(star.data)

# Setting up variables 
y<-star.data$y # outcome variable
w<-star.data$w # treatment variable
X<-as.matrix(cbind(fem,wh,fl,urb,age,exp,lad,deg)) # covariates: aligning the columns so that the first 3 columns (variables) define the subgroups of interest

# Training a causal forest
tau.forest <- causal_forest(X, y, w, num.trees = 4000)

# Estimating conditional treatment effect on different subgroups
## 8 different subgroups for {fem,wh,fl}, which are all 0/1 variables
subgroups<-expand.grid(c(0,1),c(0,1),c(0,1)) 
num.subgroups<-nrow(subgroups) # 8 subgroups
num.Xvars<-ncol(X) # 8 covariates in total
X.eval<-matrix(0,nrow=num.subgroups,ncol=num.Xvars) # empty 8x8 matrix with 0's as placeholders
for(j in 1:num.subgroups){
  X.eval[j,1:3]<-as.matrix(subgroups[j,]) 
  X.eval[j,-c(1:3)]<-colMeans(X)[-c(1:3)] # other variables at means
}

## Estimates of predicted conditional treatment effects for each subgroup 
tau.hat <- predict(tau.forest, X.eval, estimate.variance = TRUE)

## Standard errors
SE <- sqrt(tau.hat$variance.estimates)

## 95% confidence intervals
CI<-cbind(tau.hat$predictions - 1.96 * SE, 
          tau.hat$predictions + 1.96 * SE)

# Printing the results
results.summ <-cbind(subgroups,tau.hat$predictions,SE,CI)
colnames(results.summ)<-c("fem","wh","fl","estimate","SE","CI_lower","CI_upper")
print(results.summ)

###################################################################################
# Support vector machines
###################################################################################
rm(list = ls())
library(ISLR)
library(e1071)

set.seed(1)

data(Caravan) 

y<-Caravan$Purchase # outcome variable 
X<-Caravan[,-86] # covariates

# Setting train and test sets
test<-1:500
train.y<-y[-test]
train.X<-X[-test,]
test.y<-y[test]
test.X<-X[test,]
train.data<-data.frame(x=train.X, y=train.y)
test.data<-data.frame(x=test.X, y=test.y)

# Train SVM
costs<-seq(0.01, 2, length.out = 200)
test.accuracy<-costs
for(i in 1:length(costs)){
  print(i)
  
  # Fit SVM with polynomial kernel.
  svmfit<-svm(y~., data=train.data, kernel="radial",gamma=2, cost = costs[i])
  
  # Predict labels for test data
  test.ypred<-predict(svmfit,test.data)
  # Test sample accuracy
  test.accuracy[i]<-mean(test.ypred==test.data$y)
}

test.bestcost<-costs[which.max(test.accuracy)]
print(test.bestcost)




