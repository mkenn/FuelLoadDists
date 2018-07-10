###########
# script to perform bootstrap and KS MC p-values
# for custom hurdle fits
# with and without outliers
# all distributions (norm,lnorm,gamma,weib)
###########

# requires the distribution fitting workspace
load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
# this should have the required functions
# this returns the bootstrap values for the
# "best-fitting" distribution, wrt minimum liklihood
library(fitdistrplus)
bootstrapHurdleFitsNout<-bootstrapFits.fn(data.file=data.file,evts=evt.vals,evt.col=EVTCol,start.col=start.col,
                                      min.plot=min.plot,write.file=FALSE,file.name="DistFitSummaryEVT",
                                      include0=FALSE,add.val=0.1,verbose=FALSE,
                                      rankObj=distributionCustomRankingHurdleNOut,
                                      n.iter=5000,removeOut=TRUE) #rankObj is a distribution ranking object
bootstrapHurdleFits<-bootstrapFits.fn(data.file=data.file,evts=evt.vals,evt.col=EVTCol,start.col=start.col,
                                          min.plot=min.plot,write.file=FALSE,file.name="DistFitSummaryEVT",
                                          include0=FALSE,add.val=0.1,verbose=FALSE,
                                          rankObj=distributionCustomRankingHurdle,
                                          n.iter=5000,removeOut=FALSE) #rankObj is a distribution ranking object

save(bootstrapHurdleFitsNout,bootstrapHurdleFits,"bootstrapHurdleNOutFitsListInclNOut.RData")


ksPValsWZero<-list()
ksPValsWOZero<-list()
ksPValsWOZeroNOut<-list()
n.iter<-5000

cur.cols<-start.col:ncol(data.file)

cur.fit<-distributionCustomFittingHurdleNOut$HurdleFit
for(i in 34:length(distributionCustomFittingHurdleNOut$HurdleFit))
{
  ksPValsWOZeroNOut[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],lnormPVal=NA,gammaPVal=NA)
  for(j in 1:nrow(ksPValsWOZeroNOut[[i]])) 
  {
    ################
    # if(!is.na(distributionFittingHurdle$WithZeroFit[[i]]$norm.ks[j]))
    # {
    #   tmp.fn<-KSMC.fn(distr = "norm",param1=distributionFittingHurdle$WithZeroFit[[i]]$norm.mu[j],
    #                   param2=distributionFittingHurdle$WithZeroFit[[i]]$norm.sigma[j],niter = n.iter,
    #                   nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
    #                   obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$norm.ks[j])
    #   
    #   ksPValsWZero[[i]]$normPVal[j]<-tmp.fn[[1]]
    #   
    #   tmp.fn<-KSMC.fn(distr = "lnorm",param1=distributionFittingHurdle$WithZeroFit[[i]]$lnorm.mu[j],
    #                   param2=distributionFittingHurdle$WithZeroFit[[i]]$lnorm.sigma[j],niter = n.iter,
    #                   nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
    #                   obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$lnorm.ks[j])
    #   
    #   ksPValsWZero[[i]]$lnormPVal[j]<-tmp.fn[[1]]
    #   
    #   tmp.fn<-KSMC.fn(distr = "gamma",param1=distributionFittingHurdle$WithZeroFit[[i]]$gamma.shape[j],
    #                   param2=distributionFittingHurdle$WithZeroFit[[i]]$gamma.rate[j],niter = n.iter,
    #                   nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
    #                   obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$gamma.ks[j])
    #   
    #   ksPValsWZero[[i]]$gammaPVal[j]<-tmp.fn[[1]]
    #   
    #   tmp.fn<-KSMC.fn(distr = "weibull",param1=distributionFittingHurdle$WithZeroFit[[i]]$weibull.shape[j],
    #                   param2=distributionFittingHurdle$WithZeroFit[[i]]$weibull.scale[j],niter = n.iter,
    #                   nsamp = distributionFittingHurdle$WithZeroFit[[i]]$n.obs[j],
    #                   obs.stat =distributionFittingHurdle$WithZeroFit[[i]]$weibull.ks[j])
    #   
    #   ksPValsWZero[[i]]$weibullPVal[j]<-tmp.fn[[1]]
    #   
    #   
    # }
    ###########
    if(!is.na(cur.fit[[i]]$lnorm.ks[j]))
    {
      tmp.fn<-KSMC.fn(distr = "lnorm",param1=cur.fit[[i]]$lnorm.p1[j],
                      param2=cur.fit[[i]]$lnorm.p2[j],niter = n.iter,
                      nsamp = cur.fit[[i]]$n.obs[j],
                      obs.stat =cur.fit[[i]]$lnorm.ks[j])
      
      ksPValsWOZeroNOut[[i]]$lnormPVal[j]<-tmp.fn[[1]]
      
      tmp.fn<-KSMC.fn(distr = "gamma",param1=cur.fit[[i]]$gamma.p1[j],
                      param2=cur.fit[[i]]$gamma.p2[j],niter = n.iter,
                      nsamp = cur.fit[[i]]$n.obs[j],
                      obs.stat =cur.fit[[i]]$gamma.ks[j])
      
      ksPValsWOZeroNOut[[i]]$gammaPVal[j]<-tmp.fn[[1]]
      
    }
  }
}
save.image("KS_MCFnTestResults.RData")

############
# now do equivalence tests
#############
library(equivalence)
equivalence.epValNOut.list<-equivalence.epValWOZero.list<-list() # contains results with and without 0
cur.cols<-start.col:ncol(data.file)
for(i in 1:length(evt.vals)) # for each evt group
{
  # summary dataframe
  equivalence.epValNOut.list[[i]]<-equivalence.epValWOZero.list[[i]]<-
    data.frame(fueltype=names(data.file)[cur.cols],lnormResult=NA,gammaResult=NA)  
  equivalence.epValNOut.list[[i]][!is.na(distributionCustomFittingHurdleNOut$HurdleFit[[i]][,2]),2:3]<-1#flag fits with 1's, see if gets replaced.
  equivalence.epValWOZero.list[[i]][!is.na(distributionCustomFittingHurdle$HurdleFit[[i]][,2]),2:3]<-1#flag fits with 1's, see if gets replaced.
}  

ep.val<-0.001 # initialize with a tiny epsilon value
# then use the equivalence function to calculate the equivalence test
equivalenceNOut<-equivalence.fn(data.file,distFit.obj=distributionCustomFittingHurdleNOut$HurdleFit,evts=evt.vals,
                                evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                ep.val=ep.val, include0=FALSE,add.val=0.1,removeOut=TRUE)

equivalenceWOZero<-equivalence.fn(data.file,distFit.obj=distributionCustomFittingHurdle$HurdleFit,evts=evt.vals,
                                  evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                  min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                  ep.val=ep.val, include0=FALSE,add.val=0)
# if the result is rejected, and this is the first time rejected is observed, note the epsilon value
for(k in 1:length(equivalenceNOut))
{
  for(j in 1:nrow(equivalenceNOut[[k]]))
  {
    if(!is.na(equivalenceNOut[[k]][j,3])) # is there a fit?
    {
      tmp.id<-which(equivalenceNOut[[k]][j,]=="rejected") # id the rejected hypotheses
      if(length(tmp.id)>0) # if there are any rejected
      {
        for(m in tmp.id)
        {
          if(equivalence.epValNOut.list[[k]][j,m]==1) # if it's still at its initial value
            equivalence.epValNOut.list[[k]][j,m]<-ep.val #replace with this smaller value
        }
      }
    }
    if(!is.na(equivalenceWOZero[[k]][j,3])) # as above, for continuous part of hurdle distribution
    {
      tmp.id<-which(equivalenceWOZero[[k]][j,]=="rejected")
      if(length(tmp.id)>0)
      {
        for(m in tmp.id)
        {
          if(equivalence.epValWOZero.list[[k]][j,m]==1)# if it's still at its initial value
            equivalence.epValWOZero.list[[k]][j,m]<-ep.val#replace with the smaller value
        }
      }
    }
  }
}


# Loop through ep.vals to 0.4, find for each with and without zeroes the lowest ep.val
# at which the equivalence test is rejected
# There is a power issue here, where the hurdle model will have a much
# lower sample size for the equivalence test (thereby lower power)
# this loop works as above, replacing the epsilon value with those in this vector
ep.vals<-seq(0.002,0.40,0.001)
for(i in 1:length(ep.vals))
{
  tmp.ep.val<-ep.vals[i]
  tmp.equivalenceNOut<-equivalence.fn(data.file,distFit.obj=distributionCustomFittingHurdleNOut$HurdleFit,evts=evt.vals,
                                      evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                      min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                      ep.val=tmp.ep.val, include0=FALSE,add.val=0.1,removeOut=TRUE)
  tmp.equivalenceWOZero<-equivalence.fn(data.file,distFit.obj=distributionCustomFittingHurdle$HurdleFit,evts=evt.vals,
                                        evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                        min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                        ep.val=tmp.ep.val, include0=FALSE,add.val=0)
  
  for(k in 1:length(tmp.equivalenceNOut))
  {
    for(j in 1:nrow(tmp.equivalenceNOut[[k]]))
    {
      if(!is.na(tmp.equivalenceNOut[[k]][j,3]))
      {
        tmp.id<-which(tmp.equivalenceNOut[[k]][j,]=="rejected")
        if(length(tmp.id)>0)
        {
          for(m in tmp.id)
          {
            if(equivalence.epValNOut.list[[k]][j,m]==1) # if it's still at its initial value
              equivalence.epValNOut.list[[k]][j,m]<-tmp.ep.val # replace with the smaller value
          }
        }
      }
      if(!is.na(tmp.equivalenceWOZero[[k]][j,3]))
      {
        tmp.id<-which(tmp.equivalenceWOZero[[k]][j,]=="rejected")
        if(length(tmp.id)>0)
        {
          for(m in tmp.id)
          {
            if(equivalence.epValWOZero.list[[k]][j,m]==1)# if it's still at its initial value
              equivalence.epValWOZero.list[[k]][j,m]<-tmp.ep.val# replace with the smaller value
          }
        }
      }
    }
  }
}

# and this provides a summary across all evts, for each fuel type
fuel.ep.list<-list()

for(j in 1:30)
{
  fuel.ep.list[[j]]<-data.frame(evt=evt.vals,epNOut=NA,epWOZero=NA,prop0=NA)
  for(k in 1:length(equivalence.epValNOut.list))
  {
    if(!is.na(equivalence.epValNOut.list[[k]][j,2]))
      fuel.ep.list[[j]]$epNOut[k]<-min(equivalence.epValNOut.list[[k]][j,2:3])
    if(!is.na(equivalence.epValWOZero.list[[k]][j,2]))
      fuel.ep.list[[j]]$epWOZero[k]<-min(equivalence.epValWOZero.list[[k]][j,2:3])
    fuel.ep.list[[j]]$prop0[k]<-distributionCustomFittingHurdle$HurdleFit[[k]]$prop0[j]
  }
}

save(equivalence.epValNOut.list, equivalence.epValWOZero.list,fuel.ep.list,file="EquivalenceResultsInclNOut.RData")

