#######
# final script to perform bootstrap on estimated distribution parameters
# and to use Monte Carlo estimation of KS p-values
# this assumes only log-normal and gamma distributions were estimated
#######

load("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # load in the workspaces saved in FinalHurdleFitsScript.R
source("../Functions/BootStrapFitDist_FN.R") # source the bootstrap function
source("../Functions/KS_MC_FN.R") # source the Monte Carlo KS Function 

######
# first the bootstrap
# Including outliers
bootstrapHurdleFits<-bootstrapFits.fn(data.file=data.file,evts=evt.vals,evt.col=EVTCol,start.col=13,min.plot=30,
                                      write.file=FALSE,file.name="DistFitSummaryEVT",
                                      include0=FALSE,add.val=0.1,verbose=FALSE,rankObj=distributionCustomRankingHurdle,
                                      n.iter=5000) #rankObj is a distribution ranking object
# With outliers removed
bootstrapHurdleNOutFits<-bootstrapFits.fn(data.file=data.file,evts=evt.vals,evt.col=EVTCol,start.col=13,min.plot=30,
                                      write.file=FALSE,file.name="DistFitSummaryEVT",
                                      include0=FALSE,add.val=0.1,verbose=FALSE,rankObj=distributionCustomRankingHurdleNOut,
                                      n.iter=5000,removeOut=TRUE) #rankObj is a distribution ranking object

#Save the results, to be summarized into master tables
save(bootstrapHurdleFits,bootstrapHurdleNOutFits,file="BootstrapHurdleFitsListInclNOut.RData")


##Now KS p-values
ksPValsNOut<-list() # with outliers removed
ksPValsWOZero<-list() # including outliers

n.iter<-5000 # number of Monte Carlo replications
cur.col<-start.col:ncol(data.file) # define the columns in the database corresponding to fuel types.
for(i in 1:length(evt.vals))
{
# setup the dataframe to record the results
  ksPValsNOut[[i]]<-ksPValsWOZero[[i]]<-data.frame(fueltype=names(data.file)[cur.col],lnormPVal=NA,gammaPVal=NA)
  for(j in 1:nrow(ksPValsNOut[[i]]))   
  {
    # first without outliers
    if(!is.na(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$lnorm.ks[j])) # so the distribution was estimated without outliers
    {
# perform Monte Carlo reps for log-normal
      tmp.fn<-KSMC.fn(distr = "lnorm",param1=distributionCustomFittingHurdleNOut$HurdleFit[[i]]$lnorm.p1[j],
                      param2=distributionCustomFittingHurdleNOut$HurdleFit[[i]]$lnorm.p2[j],niter = n.iter,
                      nsamp = distributionCustomFittingHurdleNOut$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionCustomFittingHurdleNOut$HurdleFit[[i]]$lnorm.ks[j])
# record the p-value      
      ksPValsNOut[[i]]$lnormPVal[j]<-tmp.fn[[1]]
      
# perform Monte Carlo reps for gamma
      tmp.fn<-KSMC.fn(distr = "gamma",param1=distributionCustomFittingHurdleNOut$HurdleFit[[i]]$gamma.p1[j],
                      param2=distributionCustomFittingHurdleNOut$HurdleFit[[i]]$gamma.p2[j],niter = n.iter,
                      nsamp = distributionCustomFittingHurdleNOut$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionCustomFittingHurdleNOut$HurdleFit[[i]]$gamma.ks[j])
# record the p-value      
      ksPValsNOut[[i]]$gammaPVal[j]<-tmp.fn[[1]]
      
    }
# Now with outliers, all else as above
    if(!is.na(distributionCustomFittingHurdle$HurdleFit[[i]]$lnorm.ks[j])) # so the distribution was estimated with outliers
    {
      tmp.fn<-KSMC.fn(distr = "lnorm",param1=distributionCustomFittingHurdle$HurdleFit[[i]]$lnorm.p1[j],
                      param2=distributionCustomFittingHurdle$HurdleFit[[i]]$lnorm.p2[j],niter = n.iter,
                      nsamp = distributionCustomFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionCustomFittingHurdle$HurdleFit[[i]]$lnorm.ks[j])
      
      ksPValsWOZero[[i]]$lnormPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "gamma",param1=distributionCustomFittingHurdle$HurdleFit[[i]]$gamma.p1[j],
                      param2=distributionCustomFittingHurdle$HurdleFit[[i]]$gamma.p2[j],niter = n.iter,
                      nsamp = distributionCustomFittingHurdle$HurdleFit[[i]]$n.obs[j],
                      obs.stat =distributionCustomFittingHurdle$HurdleFit[[i]]$gamma.ks[j])
      
      ksPValsWOZero[[i]]$gammaPVal[j]<-tmp.fn[[1]]

      
    }
  }
}
# save the results, to be summarized in a master table
save.image("KS_MCFnTestResultsInclNOut.RData")
