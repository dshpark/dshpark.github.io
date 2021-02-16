# clear environment
rm(list = ls())

# set working directory
setwd('/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/')

# loading (installed) libraries
library(devtools)
install_github("markwestcott34/stargazer-booktabs")
library(quantreg)
library(MASS)
library(jtools)
library(stargazer)

# Importing dataset 
bwght<-read.csv('Data Sets/bwght.csv')

# Quantile regressions
qfit_p10 <- rq(bwght~faminc+motheduc+cigs,data=bwght,tau=0.1) # 10th percentile (0.1 quantile)
qfit_p50 <- rq(bwght~faminc+motheduc+cigs,data=bwght,tau=0.5) # 50th percentile (0.5 quantile)
  
  # Viewing results
  summary(qfit_p10)
  summ(qfit_p10) # from jtools library
  
  # Exporting to LaTeX
  stargazer(qfit_p10, out="tex_raw/section6_qfit_sample.tex", 
            dep.var.labels=c("Birthweight"),
            covariate.labels=c("Family Income", "Mother's education","Number of cigarettes smoked during pregnant"),
            style="aer",float=FALSE,omit.table.layout = "n",
            omit.stat=c("LL","ser","f"), no.space=TRUE)
  
# Plotting quantile regression lines
pdf(file="tex_raw/section6_qplot_sample.pdf") # starting a PDF device to save the plot
plot(bwght$faminc,bwght$bwght,type="n",xlab="Family Income", ylab="Birthweight")
points(bwght$faminc,bwght$bwght,col="blue",cex=0.8) # plotting the data points
ols_fit <- lm(bwght~faminc,data=bwght)
abline(ols_fit,lty=1,col="red") # OLS line
taus <- c(.25,.5,.75) 
for(i in 1:length(taus)) {
  rq_fit <- rq(bwght~faminc,data=bwght,tau=taus[i])
  abline(rq_fit,lty=i+1,col="gray")
}
legend("topright", legend=c("OLS", "25th quantile", "50th quantile", "75th quantile"), col=c("red","gray","gray","gray"),
       lty=1:4, cex=0.8) # adding a legend onto the plot
dev.off() # closing the PDF device 

# Plotting the effects by quantile (with confidence intervals)
pdf(file="tex_raw/section6_qplot_sample2.pdf")
taus <- c(seq(.1,.95,by=.05)) 
qfit <- summary(rq(bwght~faminc,data=bwght,tau=taus))
plot(qfit, parm=c(2), level=0.95, main="")
title(xlab = "Quantile",ylab="Estimated effect")
dev.off()
