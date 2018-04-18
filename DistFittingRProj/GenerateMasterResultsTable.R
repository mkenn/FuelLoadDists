#############
# script to create master table of results 
# One table per evt
#############
# table structure: rows=fuel types, columns = c(distribution name, n.obs, prop0, param1, bs param1, cv param1, param2, bs param2, cv param2, equivalence val, ks p-val)
# requires all distribution fits and evaluation
#############
# # load the necessary results workspaces
# load("Workspaces/KS_MCFnTestResults.RData") # KS tests
# load("Workspaces/EquivalenceResults.RData") # equivalence tests
# #load("Workspaces/HurdleFits.RData") # estimated distributions
# load("Workspaces/HurdleCustomFits.RData") # estimated distributions
# load("Workspaces/BootstrapHurdleFitsList.RData") # bootstrap results

load("Workspaces/KS_MCFnTestResultsInclNOut.RData") # KS tests
load("Workspaces/EquivalenceResultsInclNOut.RData") # equivalence tests
load("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
load("Workspaces/BootstrapHurdleFitsListInclNOut.RData") # bootstrap results


distribution.masterTable<-list()
for(i in 1:length(evt.vals))
{
  distribution.masterTable[[i]]<-data.frame(fuelType=colnames(data.file)[start.col:ncol(data.file)],
                                            distr=NA,n.obs=NA,prop0=NA,
                                            param1.est=NA,param1.sd=NA,param1.cv=NA,
                                            param2.est=NA,param2.sd=NA,param2.cv=NA,
                                            KS.pVal=NA,equivalence.val=NA)
  fuel.cols<-start.col:ncol(data.file)
  for(j in 1:length(fuel.cols))
  {
    distribution.masterTable[[i]]$n.obs[j]<-round(distributionCustomFittingHurdle$HurdleFit[[i]]$n.obs[j],digits=3)
    distribution.masterTable[[i]]$prop0[j]<-round(distributionCustomFittingHurdle$HurdleFit[[i]]$prop0[j],digits=3)
    distribution.masterTable[[i]]$distr[j]<-distributionCustomRankingHurdle[[i]]$dist1.fit[j]
    distribution.masterTable[[i]]$param1.est[j]<-round(bootstrapHurdleFits[[i]]$param1.est[j],digits=3)
    distribution.masterTable[[i]]$param1.sd[j]<-round(bootstrapHurdleFits[[i]]$param1.bootSD[j],digits=3)
    distribution.masterTable[[i]]$param1.cv[j]<-round(bootstrapHurdleFits[[i]]$param1.bootCV[j],digits=3)
    distribution.masterTable[[i]]$param2.est[j]<-round(bootstrapHurdleFits[[i]]$param2.est[j],digits=3)
    distribution.masterTable[[i]]$param2.sd[j]<-round(bootstrapHurdleFits[[i]]$param2.bootSD[j],digits=3)
    distribution.masterTable[[i]]$param2.cv[j]<-round(bootstrapHurdleFits[[i]]$param2.bootCV[j],digits=3)
    if(!is.na(distribution.masterTable[[i]]$distr[j]))
      distribution.masterTable[[i]]$equivalence.val[j]<-switch(distribution.masterTable[[i]]$distr[j],
                                                     normLL=round(equivalence.epValWOZero.list[[i]]$normResult[j],digits=3),
                                                     lnormLL=round(equivalence.epValWOZero.list[[i]]$lnormResult[j],digits=3),
                                                     gammaLL=round(equivalence.epValWOZero.list[[i]]$gammaResult[j],digits=3),
                                                     weibullLL=round(equivalence.epValWOZero.list[[i]]$weibullResult[j],digits=3))
    # if(i<length(ksPValsWOZero)) # placeholder, until we complete KS p-values
    # {
      if(!is.na(distribution.masterTable[[i]]$distr[j]))
        distribution.masterTable[[i]]$KS.pVal[j]<-switch(distribution.masterTable[[i]]$distr[j],
                                                         normLL=round(ksPValsWOZero[[i]]$normPVal[j],digits=3),
                                                         lnormLL=round(ksPValsWOZero[[i]]$lnormPVal[j],digits=3),
                                                         gammaLL=round(ksPValsWOZero[[i]]$gammaPVal[j],digits=3),
                                                         weibullLL=round(ksPValsWOZero[[i]]$weibullPVal[j],digits=3))
    }
#  }
}
save(distribution.masterTable,file="Workspaces/MasterTable2.RData")

for(i in 1:length(evt.vals))
{
  write.csv(distribution.masterTable[[i]],file=paste("MasterTables/MasterTableEVT_",evt.vals[i],".csv",sep=""),row.names = FALSE)
}

#save(distribution.masterTable,file="Workspaces/MasterTable.RData")

# so far only have 23 KS pvalues. Can do some comparisons of measures of fit, for example
# KS p-values v. eq Vals, KS p-vals v. n.obs, eq Vals v. n.obs

masterSummary.list<-list()
for(j in 1:30)
{
  masterSummary.list[[j]]<-data.frame(n.obs=rep(NA,length(evt.vals)),KS.pval=NA,ep.val=NA,param1.cv=NA,param2.cv=NA)
  for(k in 1:length(evt.vals))
  {
    masterSummary.list[[j]]$n.obs[k]<-distribution.masterTable[[k]]$n.obs[j]
    masterSummary.list[[j]]$KS.pval[k]<-distribution.masterTable[[k]]$KS.pVal[j]
    masterSummary.list[[j]]$ep.val[k]<-distribution.masterTable[[k]]$equivalence.val[j]
    masterSummary.list[[j]]$param1.cv[k]<-distribution.masterTable[[k]]$param1.cv[j]
    masterSummary.list[[j]]$param2.cv[k]<-distribution.masterTable[[k]]$param2.cv[j]
  }
}

pdf(file="MasterSummaryCompEpN.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:30)
  {
    plot(masterSummary.list[[j]]$n.obs,masterSummary.list[[j]]$ep.val,
         xlim=c(0,1300),ylim=c(0,0.1),pch=16,main=distribution.masterTable[[1]]$fuelType[j],
         xlab="n non-zero entries",ylab="epsilon value (distribution error)")
  tmp.val<-
  if(length(masterSummary.list[[j]]$ep.val[!is.na(masterSummary.list[[j]]$ep.val)])>3)
  {
    tmp.lm<-lm(masterSummary.list[[j]]$ep.val~masterSummary.list[[j]]$n.obs)
    abline(tmp.lm$coefficients,lwd=2)
    }
  }
dev.off()

pdf(file="MasterSummaryCompKSPValN.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:30)
{
  plot(masterSummary.list[[j]]$n.obs,masterSummary.list[[j]]$KS.pval,
       xlim=c(0,1300),ylim=c(0,1),pch=16,main=distribution.masterTable[[1]]$fuelType[j],
       xlab="n non-zero entries",ylab="KS p-value")
  tmp.val<-
    if(length(masterSummary.list[[j]]$KS.pval[!is.na(masterSummary.list[[j]]$KS.pval)])>3)
    {
      tmp.lm<-lm(masterSummary.list[[j]]$KS.pval~masterSummary.list[[j]]$n.obs)
      abline(tmp.lm$coefficients,lwd=2)
    }
  abline(h=0.10,lty=2)
}
dev.off()

pdf(file="MasterSummaryCompEpKS.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:30)
{  
  plot(masterSummary.list[[j]]$KS.pval,masterSummary.list[[j]]$ep.val,
       xlim=c(0,1),ylim=c(0,0.1),pch=16,main=distribution.masterTable[[1]]$fuelType[j],
       xlab="KS P value",ylab="epsilon value (distribution error)")
  if(length(masterSummary.list[[j]]$ep.val[!is.na(masterSummary.list[[j]]$ep.val)])>3)
  {
    tmp.lm<-lm(masterSummary.list[[j]]$ep.val~masterSummary.list[[j]]$KS.pval)
    abline(tmp.lm$coefficients,lwd=2)
  }
  
}
dev.off()

