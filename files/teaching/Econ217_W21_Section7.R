# ----------------------------- #
# Econ217 (J. Li, W2021)        #
# Section 7 (Feb. 17-19, 2021)  # NOTE: all the path directories are based on my own computer, 
# TA: David Sungho Park         #       so you should change them according to yours.
# ----------------------------- #

# clear environment
rm(list = ls())

# set working directory
setwd('/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/')

# loading (installed) libraries
packages<-c("quantreg","matrixStats","doParallel","glmnet","AER","class","ISLR")
invisible(lapply(packages,library,character.only=TRUE))


###################################################################################
# Bootstrapping confidence intervals and standard errors (for quantile regressions)
###################################################################################
data<-read.csv('Data Sets/bwght.csv') # importing dataset
attach(data) # so that we can refer to the variables without the $ sign
registerDoParallel(cores=2) # setting 2 cores for executing code in parallel

B<-50 # total number of iterations in bootstrap
N<-nrow(data) # number of observations for the imported data
taus<-seq(0.1,0.95, by=.05) # specifying which quantiles
ntaus<-length(taus)

bootCI<-matrix(0,nrow=ntaus,ncol=2) # creating empty matrix for storing bootstrapped C.I.'s
bootSE<-matrix(0,nrow=ntaus,ncol=1) # empty vector for storing bootstrapped s.e.'s

for(i in 1:ntaus){
  t<-taus[i]
  # Bootstrapping empirical distribution using a parallel for loop
  bootDistribution<-foreach(b=1:B,.combine = rbind) %dopar% {
    set.seed(b)
    indices<-sample(N,N,replace = TRUE) 
    fit<-rq(bwght[indices]~faminc[indices]+motheduc[indices]+cigs[indices],tau=t)
    fit$coefficients[2] # here, only interested in 1st indep var. (which is 2nd after the intercept term)
  }
  
  alpha<-0.05 # alpha = significance level = 1 - confidence level
  
  # Saving bootstrapped estimates
  bootCI[i,]<-colQuantiles(bootDistribution,probs=c(alpha/2,(1-alpha/2))) 
  bootSE[i,]<-colSds(bootDistribution)
}

###################################################################################
# Test vs. training error (over k's in KNN)
###################################################################################
attach(Caravan) # data on purchases of Caravan insurance (from ISLR package)
data(Caravan)

# Standardizing X variables to have mean 0 sd 1
standardized.X<-scale(Caravan[,-86]) # note: col 86 is <Purchase> (Y variable)

# Splitting data into test & training sets
testindices<-1:1000 
test.X<-standardized.X[testindices,] 
test.Y<-Purchase[testindices] 
train.X<-standardized.X[-testindices,] 
train.Y<-Purchase[-testindices] 

set.seed(1) # for reproducibility 
# Run 5-nearest neighbors and predict outcome on training data
train.knn.pred<-knn(train.X,train.X,train.Y,k=5)
# Compute training error
train.error<-mean(train.Y!=train.knn.pred)
# Run 5-nearest neighbors and predict outcome on test data
test.knn.pred<-knn(train.X,test.X,train.Y,k=5)
# Compute test error 
test.error<-mean(test.Y!=test.knn.pred)

# Now, doing this for multiple k's in a for loop (example: k=1 through 10)
ks<-1:10
train.error.vector<-ks # empty vector for storing training errors
test.error.vector<-ks # empty vector for storing test errors

set.seed(1) 
for(k in ks){
  train.knn.pred<-knn(train.X,train.X,train.Y,k=k) 
  train.error.vector[k]<-mean(train.Y!=train.knn.pred) 
  test.knn.pred<-knn(train.X,test.X,train.Y,k=k) 
  test.error.vector[k]<-mean(test.Y!=test.knn.pred) 
}

# Plotting 
pdf(file="tex_raw/section7_knn_errors.pdf")
plot(ks,test.error.vector,type="n",xlab="K",ylab="Prediction Error") # creating an empty plot frame
lines(ks,test.error.vector,col="red")
lines(ks,train.error.vector,col="blue")
legend("topright",legend = c("Test Error", "Training Error"),col=c("red", "blue"),lty=1)
dev.off()

# Finding the index number of the lowest value 
which.min(test.error.vector)
which.min(train.error.vector)


###################################################################################
# Post Double-Selection Lasso 
###################################################################################
starData<-read.csv('Data Sets/star.csv')

# Setting up variables in matrices (i.e. correct format for <cv.glmnet>)
colnames(starData)
y<-as.matrix(starData)[,"y"] # outcome (response) variable
d<-as.matrix(starData)[,"w"] # treatment indicator 
X<-as.matrix(starData)[,-c(1,2)] # covariates (features)

set.seed(1)
k = 5 
# Step 1: Lasso of y on X to select the controls that best predict y.
y.fit<-cv.glmnet(X,y,family="gaussian", type.measure = "mse", nfolds = k)
y.betas<-as.numeric(coef(y.fit,s = "lambda.min"))
y.nonzeroindices<-which(y.betas[-1]!=0) # note: excluding 1st index, where the intercept term is

# Step 2: Lasso of d on X to select the controls that best predict y.
d.fit<-cv.glmnet(X,d,family="gaussian", type.measure = "mse", nfolds = k)
d.betas<-as.numeric(coef(d.fit,s = "lambda.min"))
d.nonzeroindices<-which(d.betas[-1]!=0) # note: excluding 1st index, where the intercept term is

# Step 3: OLS of y on d and the unions of controls selected in steps 1 and 2.
nonzeroindices<-union(y.nonzeroindices,d.nonzeroindices)
X.selected<-X[,nonzeroindices]
OLSfit.postLasso<-lm(y ~ d + X.selected)
OLSfit.preLasso<-lm(y ~ d + X)

###################################################################################
# Post-Lasso 2SLS
###################################################################################
starData<-read.csv('Data Sets/star.csv')

# Setting up variables in matrices (i.e. correct format for <cv.glmnet>)
colnames(starData)
y<-as.matrix(starData)[,1] # outcome (response) variable
d<-as.matrix(starData)[,2] # endogenous regressor 
X<-as.matrix(starData)[,3:5] # exogenous regressors
Z<-as.matrix(starData)[,6:11] # instrument candidates

# OLS residuals of each instrument on exogenous regressors
Z.residuals<-matrix(0,nrow=nrow(Z),ncol=ncol(Z)) # empty matrix
for(i in 1:ncol(Z)){
  fit<-lm.fit(X,Z[,i]) 
  Z.residuals[,i]<-fit$residuals
}

# OLS residuals of endogenous regressor on exogenous regressors
fit<-lm.fit(X,d) 
d.residuals<-fit$residuals

# Lasso of d.residuals on z.residuals to select which instruments to include
set.seed(1)
k=5
fit<-cv.glmnet(Z.residuals,d.residuals,family="gaussian", type.measure = "mse", nfolds = k)
betas<-as.numeric(coef(fit,s = "lambda.min"))
nonzeroindices<-which(betas[-1]!=0) # note: excluding 1st index, where the intercept term is

# Post-Lasso 2SLS
Z.selected<-Z[,nonzeroindices]
postLasso.2sls<-ivreg(y~d+X | Z.selected+X)
summary(postLasso.2sls)

preLasso.2sls<-ivreg(y~d+X | Z+X)
summary(preLasso.2sls)
