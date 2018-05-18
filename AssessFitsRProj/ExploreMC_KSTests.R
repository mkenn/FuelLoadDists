##########
# bootstrap distribution fits
# requires execution of FitHurdleUnivariateDistributions.R
##########
# idea: create an "envelope" akin to spatial stats envelope,
# using bootstrap
# see if ecdf is outside of envelope for each distribution fit
# test type I error rate for this approach with a simulation study
# can also try MC approach to KS test. I still fear this suffers from
# from high power
# test case

#bootdist() to perform bootstrap of fitdist
# bootdist(fitObj, niter), looks like you can parallize it with parallel="snow" in windows ("multicore" otherwise), 
# and specifying the number of available CPUs; should probably also do this on server, if attempted.

bootstrapHurdleFits<-bootstrapFits.fn(data.file=data.file,evts=evt.vals,evt.col=evt.col,start.col=13,min.plot=30,
                           write.file=FALSE,file.name="DistFitSummaryEVT",
                           include0=FALSE,add.val=0.1,verbose=FALSE,rankObj=distributionRankingHurdle,
                           n.iter=5000) #rankObj is a distribution ranking object
  
load("BootstrapHurdleFitsList.RData")
ecdf1<-ecdf(cur.loads.vals)


loads.plot<-seq(min(cur.loads.vals),max(cur.loads.vals),0.1)
plot(ecdf1,col="purple")
for(i in 1:1000)
  lines(loads.plot,pnorm(loads.plot,ests2$estim[i,1],ests2$estim[i,2]),col="grey")
par(new=TRUE)
plot(ecdf1,col="purple")

plot(ecdf1,col="purple")
for(i in 1:1000)
  lines(loads.plot,pgamma(loads.plot,ests$estim[i,1],ests$estim[i,2]),col="grey")
par(new=TRUE)
plot(ecdf1,col="purple")

gamma.fit<-fitdist(cur.loads.vals,distr = "gamma")

ks.test(cur.loads.vals,"pgamma",ests$estim[i,1],ests$estim[i,2])
ks.test(cur.loads.vals,"pnorm",ests$estim[i,1],ests$estim[i,2])

gamma.test<-ks.test(cur.loads.vals,"pgamma",gamma.fit$estimate[1],gamma.fit$estimate[2])

gamma.mc<-KSMC.fn(distr = "gamma",param1 =gamma.fit$estimate[1],param2=gamma.fit$estimate[2],
                  niter = 1000,nsamp = length(cur.loads.vals),obs.stat = gamma.test$statistic)


weibull.fit<-fitdist(cur.loads.vals,distr = "weibull")
weibull.test<-ks.test(cur.loads.vals,"pweibull",weibull.fit$estimate[1],weibull.fit$estimate[2])

weibull.mc<-KSMC.fn(distr = "weibull",param1 =weibull.fit$estimate[1],param2=weibull.fit$estimate[2],
                  niter = 5000,nsamp = length(cur.loads.vals),obs.stat = weibull.test$statistic)


hist(gamma.mc)
abline(v=gamma.test$statistic)

norm.fit<-fitdist(cur.loads.vals,distr = "norm")
norm.test<-ks.test(cur.loads.vals,"pnorm",norm.fit$estimate[1],norm.fit$estimate[2])

norm.mc<-KSMC.fn(distr = "norm",param1 =norm.fit$estimate[1],param2=norm.fit$estimate[2],niter = 1000,nsamp = length(cur.loads.vals) )


hist(norm.mc)
abline(v=norm.test$statistic)

####
# So, keep a dataframe with the p-values for each candidate distribution
# on both with and without zeroes
# as a list for each evt
# Requires the distfit object
# need to add n to distfit object
####
# move over to server, this is going to take a while!
ksPValsWZero<-list()
ksPValsWOZero<-list()

n.iter<-5000
#for(i in 1:length(evt.vals))
  for(i in 32:36)
  {
  ksPValsWZero[[i]]<-ksPValsWOZero[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normPVal=NA,lnormPVal=NA,
                                                    gammaPVal=NA,weibullPVal=NA)
  for(j in 1:nrow(ksPValsWOZero[[i]])) # still need observed values for the observed KS stat, or return that in
    #distribution fitting object
  {
    if(!is.na(distributionFittingHurdle$WithZeroFit[[i]]$norm.ks[j]))
    {
      tmp.fn<-KSMC.fn(distr = "norm",param1=distributionFittingHurdle$WithZeroFit[[i]]$norm.mu[j],
                      param2=distributionFittingHurdle$WithZeroFit[[i]]$norm.sigma[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$norm.ks[j])
  
      ksPValsWZero[[i]]$normPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "lnorm",param1=distributionFittingHurdle$WithZeroFit[[i]]$lnorm.mu[j],
                      param2=distributionFittingHurdle$WithZeroFit[[i]]$lnorm.sigma[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$lnorm.ks[j])
      
      ksPValsWZero[[i]]$lnormPVal[j]<-tmp.fn[[1]]

      tmp.fn<-KSMC.fn(distr = "gamma",param1=distributionFittingHurdle$WithZeroFit[[i]]$gamma.shape[j],
                      param2=distributionFittingHurdle$WithZeroFit[[i]]$gamma.rate[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$gamma.ks[j])
      
      ksPValsWZero[[i]]$gammaPVal[j]<-tmp.fn[[1]]

      tmp.fn<-KSMC.fn(distr = "weibull",param1=distributionFittingHurdle$WithZeroFit[[i]]$weibull.shape[j],
                      param2=distributionFittingHurdle$WithZeroFit[[i]]$weibull.scale[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$weibull.ks[j])
      
      ksPValsWZero[[i]]$weibullPVal[j]<-tmp.fn[[1]]
      
    
  }
    if(!is.na(distributionFittingHurdle$HurdleFit[[i]]$norm.ks[j]))
    {
      tmp.fn<-KSMC.fn(distr = "norm",param1=distributionFittingHurdle$HurdleFit[[i]]$norm.mu[j],
                      param2=distributionFittingHurdle$HurdleFit[[i]]$norm.sigma[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$HurdleFit[[i]]$norm.ks[j])
      
      ksPValsWOZero[[i]]$normPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "lnorm",param1=distributionFittingHurdle$HurdleFit[[i]]$lnorm.mu[j],
                      param2=distributionFittingHurdle$HurdleFit[[i]]$lnorm.sigma[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$HurdleFit[[i]]$lnorm.ks[j])
      
      ksPValsWOZero[[i]]$lnormPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "gamma",param1=distributionFittingHurdle$HurdleFit[[i]]$gamma.shape[j],
                      param2=distributionFittingHurdle$HurdleFit[[i]]$gamma.rate[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$HurdleFit[[i]]$gamma.ks[j])
      
      ksPValsWOZero[[i]]$gammaPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "weibull",param1=distributionFittingHurdle$HurdleFit[[i]]$weibull.shape[j],
                      param2=distributionFittingHurdle$HurdleFit[[i]]$weibull.scale[j],niter = n.iter,
                      nsamp = distributionFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionFittingHurdle$HurdleFit[[i]]$weibull.ks[j])
      
      ksPValsWOZero[[i]]$weibullPVal[j]<-tmp.fn[[1]]
      
      
    }
  }
}
save.image("KS_MCFnTestResults.RData")

# note: i=29, j=11 couldn't get MC to work. Check! For now, skip; Still doesn't work, hangs up on gamma
# i =35, j= 11  # tried again i=35, j=11 doesn't work error on gamma dist can't estimate parameters
# 36, 18
# 69, 1--check, trying again. Seemed to be a random bs msg; Ok, ran through second time.
# Looks like it might have been an issue with naming the columns in WO zero. 
# Finish those missing EVTs (i) manually
