#########
# Sobol sensitivity analysis
##########
library(sensitivity)
### First create two samples, X1 and X1, globally for
### all fuels categories
# then generate consume outputs for all parameter combinations
# so first you create the sobol object
n <- 1000
X1 <- data.frame(matrix(runif(8 * n), nrow = n))
X2 <- data.frame(matrix(runif(8 * n), nrow = n))
sobol1<-sobol(model=NULL,X1=X1,X2=X2,nboot=100)
sobol1Eff<-sobolEff(model=NULL,X1=X1,X2=X2,nboot=100)
dim(sobol1$X) # so this has an extended sample based on the sobol method, nsamp (X1) + nsamp*nvar
# then generate the y-variables
y1<-sobol1Eff$X[,1]*19+sobol1Eff$X[,3]*10+rnorm(9000,0,2)
tell(sobol1Eff,y1) # updates the sobol object with the output
plot(sobol1Eff) # plots the importance indices, with confidence intervals
