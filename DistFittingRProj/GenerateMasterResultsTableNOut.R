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
# load("Workspaces/bootstrapHurdleFitsNoutList.RData") # bootstrap results

load("Workspaces/Current/KS_MCFnTestResults.RData") # KS tests
load("Workspaces/Current/EquivalenceResultsInclNOut.RData") # equivalence tests
load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
load("Workspaces/Current/bootstrapHurdleNoutFitsListInclNOut.RData") # bootstrap results


distribution.masterTableNOut<-list()
lower.ep.thresh<-0.05 # epsilon threshold for excellent (n>=100) or good fit (30<=n<100)
upper.ep.thresh<-0.15 # epsilon threshold for good fit
for(i in 1:length(evt.vals))
{
  distribution.masterTableNOut[[i]]<-data.frame(fuelType=colnames(data.file)[start.col:ncol(data.file)],
                                            distr=NA,n.obs=NA,prop0=NA,
                                            param1.est=NA,param1.sd=NA,param1.cv=NA,
                                            param2.est=NA,param2.sd=NA,param2.cv=NA,
                                            KS.pVal=NA,equivalence.val=NA,distr.fit.class=NA)
  fuel.cols<-start.col:ncol(data.file)
  for(j in 1:length(fuel.cols))
  {
    distribution.masterTableNOut[[i]]$n.obs[j]<-round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$n.obs[j],digits=3)
    distribution.masterTableNOut[[i]]$prop0[j]<-round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$prop0[j],digits=3)
    distribution.masterTableNOut[[i]]$distr[j]<-distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j]
    distribution.masterTableNOut[[i]]$param1.est[j]<-round(bootstrapHurdleFitsNout[[i]]$param1.est[j],digits=3)
    distribution.masterTableNOut[[i]]$param1.sd[j]<-round(bootstrapHurdleFitsNout[[i]]$param1.bootSD[j],digits=3)
    distribution.masterTableNOut[[i]]$param1.cv[j]<-round(bootstrapHurdleFitsNout[[i]]$param1.bootCV[j],digits=3)
    distribution.masterTableNOut[[i]]$param2.est[j]<-round(bootstrapHurdleFitsNout[[i]]$param2.est[j],digits=3)
    distribution.masterTableNOut[[i]]$param2.sd[j]<-round(bootstrapHurdleFitsNout[[i]]$param2.bootSD[j],digits=3)
    distribution.masterTableNOut[[i]]$param2.cv[j]<-round(bootstrapHurdleFitsNout[[i]]$param2.bootCV[j],digits=3)
    if(!is.na(distribution.masterTableNOut[[i]]$distr[j]))
    {
      distribution.masterTableNOut[[i]]$equivalence.val[j]<-switch(distribution.masterTableNOut[[i]]$distr[j],
                                                     normLL=round(equivalence.epValNOut.list[[i]]$normResult[j],digits=3),
                                                     lnormLL=round(equivalence.epValNOut.list[[i]]$lnormResult[j],digits=3),
                                                     gammaLL=round(equivalence.epValNOut.list[[i]]$gammaResult[j],digits=3),
                                                     weibullLL=round(equivalence.epValNOut.list[[i]]$weibullResult[j],digits=3))
      
        distribution.masterTableNOut[[i]]$KS.pVal[j]<-switch(distribution.masterTableNOut[[i]]$distr[j],
                                                         normLL=round(ksPValsWOZeroNOut[[i]]$normPVal[j],digits=3),
                                                         lnormLL=round(ksPValsWOZeroNOut[[i]]$lnormPVal[j],digits=3),
                                                         gammaLL=round(ksPValsWOZeroNOut[[i]]$gammaPVal[j],digits=3),
                                                         weibullLL=round(ksPValsWOZeroNOut[[i]]$weibullPVal[j],digits=3))
        ###************ NEED TO CHECK CLASSIFICATIONS--SPOT CHECK SOME DISTR GRAPHS****###
        if(distribution.masterTableNOut[[i]]$n.obs[j]*(1-distribution.masterTableNOut[[i]]$prop0[j])>=100)
        {
          if(distribution.masterTableNOut[[i]]$KS.pVal[j]>0.05)
          {
            if(distribution.masterTableNOut[[i]]$equivalence.val[j]<=lower.ep.thresh)
              distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Excellent"
            else
            {
              if(distribution.masterTableNOut[[i]]$KS.pVal[j]>0.05&distribution.masterTableNOut[[i]]$equivalence.val[j]<=upper.ep.thresh)
                distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Good"
              else
              distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Poor"
            }
          }
          else
          {
            if(distribution.masterTableNOut[[i]]$equivalence.val[j]<=lower.ep.thresh)
              distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Good"
            else
              distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Poor"
            
          }
        }
        else # the the number is [30,100)
        {
            if(distribution.masterTableNOut[[i]]$KS.pVal[j]>0.05)
            {
              if(distribution.masterTableNOut[[i]]$equivalence.val[j]<=upper.ep.thresh)
                distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Good"
            }
            else
            {
              if(distribution.masterTableNOut[[i]]$equivalence.val[j]<=lower.ep.thresh)
                distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Good"
              else
                distribution.masterTableNOut[[i]]$distr.fit.class[j]<-"Poor"
            }
        }
    }
  }
}
save(distribution.masterTableNOut,file="Workspaces/MasterTableNOut.RData")

for(i in 1:length(evt.vals))
{
  write.csv(distribution.masterTableNOut[[i]],file=paste("MasterTables/Current/MasterTableNOutEVT_",evt.vals[i],".csv",sep=""),row.names = FALSE)
}

#save(distribution.masterTableNOut,file="Workspaces/MasterTable.RData")

# so far only have 23 KS pvalues. Can do some comparisons of measures of fit, for example
# KS p-values v. eq Vals, KS p-vals v. n.obs, eq Vals v. n.obs

masterSummaryNOut.list<-list()
for(j in 1:length(fuel.cols))
{
  masterSummaryNOut.list[[j]]<-data.frame(n.obs=rep(NA,length(evt.vals)),KS.pval=NA,ep.val=NA,param1.cv=NA,param2.cv=NA)
  for(k in 1:length(evt.vals))
  {
    masterSummaryNOut.list[[j]]$n.obs[k]<-distribution.masterTableNOut[[k]]$n.obs[j]
    masterSummaryNOut.list[[j]]$KS.pval[k]<-distribution.masterTableNOut[[k]]$KS.pVal[j]
    masterSummaryNOut.list[[j]]$ep.val[k]<-distribution.masterTableNOut[[k]]$equivalence.val[j]
    masterSummaryNOut.list[[j]]$param1.cv[k]<-distribution.masterTableNOut[[k]]$param1.cv[j]
    masterSummaryNOut.list[[j]]$param2.cv[k]<-distribution.masterTableNOut[[k]]$param2.cv[j]
  }
}

pdf(file="MasterSummaryCompEpNOut.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:length(fuel.cols))
  {
    plot(masterSummaryNOut.list[[j]]$n.obs,masterSummaryNOut.list[[j]]$ep.val,
         xlim=c(0,1300),ylim=c(0,0.1),pch=16,main=distribution.masterTableNOut[[1]]$fuelType[j],
         xlab="n non-zero entries",ylab="epsilon value (distribution error)")
  tmp.val<-
  if(length(masterSummaryNOut.list[[j]]$ep.val[!is.na(masterSummaryNOut.list[[j]]$ep.val)])>3)
  {
    tmp.lm<-lm(masterSummaryNOut.list[[j]]$ep.val~masterSummaryNOut.list[[j]]$n.obs)
    abline(tmp.lm$coefficients,lwd=2)
    }
  }
dev.off()

pdf(file="MasterSummaryCompKSPValNOut.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:length(fuel.cols))
{
  plot(masterSummaryNOut.list[[j]]$n.obs,masterSummaryNOut.list[[j]]$KS.pval,
       xlim=c(0,1300),ylim=c(0,1),pch=16,main=distribution.masterTableNOut[[1]]$fuelType[j],
       xlab="n non-zero entries",ylab="KS p-value")
#  tmp.val<-
    if(length(masterSummaryNOut.list[[j]]$KS.pval[!is.na(masterSummaryNOut.list[[j]]$KS.pval)])>3)
    {
      tmp.lm<-lm(masterSummaryNOut.list[[j]]$KS.pval~masterSummaryNOut.list[[j]]$n.obs)
      abline(tmp.lm$coefficients,lwd=2)
    }
  abline(h=0.10,lty=2)
}
dev.off()

pdf(file="MasterSummaryCompEpKSNOut.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1)
for(j in 1:length(fuel.cols))
{  
  plot(masterSummaryNOut.list[[j]]$KS.pval,masterSummaryNOut.list[[j]]$ep.val,
       xlim=c(0,1),ylim=c(0,0.1),pch=16,main=distribution.masterTableNOut[[1]]$fuelType[j],
       xlab="KS P value",ylab="epsilon value (distribution error)")
  if(length(masterSummaryNOut.list[[j]]$ep.val[!is.na(masterSummaryNOut.list[[j]]$ep.val)])>3)
  {
    tmp.lm<-lm(masterSummaryNOut.list[[j]]$ep.val~masterSummaryNOut.list[[j]]$KS.pval)
    abline(tmp.lm$coefficients,lwd=2)
  }
  
}
dev.off()


#########
# now report summary statistics for each EVT group and fuel loading type
#########

sum.stats<-data.frame(matrix(NA,ncol=length(cur.cols)*2+1,nrow=length(evt.vals)))
sum.stats.mean<-sum.stats.sd<-sum.stats.prop0<-sum.stats.n<-data.frame(matrix(NA,ncol=length(cur.cols)+1,nrow=length(evt.vals)))
sum.stats[,1]<-evt.vals
sum.stats.mean[,1]<-evt.vals
sum.stats.sd[,1]<-evt.vals
sum.stats.prop[,1]<-evt.vals
sum.stats.n[,1]<-evt.vals

cols1<-seq(3,85,2)


for(i in 1:length(evt.vals))
{
  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
  for(j in cur.cols)
  {
    tmp.loads<-tmp.df[,j]
    tmp2.loads<-tmp.loads[!is.na(tmp.loads)]
    if(length(tmp2.loads)>0)
    {
      n0<-length(tmp2.loads[tmp2.loads==0])
      nAll<-length(tmp2.loads)
      sum.stats[i,2]<-round(n0/nAll,digits=3)
      sum.stats.prop0[i,j-9]<-round(n0/nAll,digits=3)
      tmp3.loads<-tmp2.loads[tmp2.loads>0]
      sum.stats.n[i,j-9]<-length(tmp3.loads)
      
      if(length(tmp3.loads)>=30)
      {
        sum.stats[i,cols1[j-10]]<-round(mean(tmp3.loads),digits=2)
        sum.stats[i,cols1[j-10]+1]<-round(sd(tmp3.loads),digits=2)
        sum.stats.mean[i,j-9]<-round(mean(tmp3.loads),digits=2)
        sum.stats.sd[i,j-9]<-round(sd(tmp3.loads),digits=2)
      }
    }
  }
}

names(sum.stats.mean)<-c("EVT",names(data.file)[cur.cols])
names(sum.stats.sd)<-c("EVT",names(data.file)[cur.cols])
names(sum.stats.prop0)<-c("EVT",names(data.file)[cur.cols])
names(sum.stats.n)<-c("EVT",names(data.file)[cur.cols])

#sum.stats.sd/sum.stats.mean

evt.ew<-c(655,682,666) # 
evt.cf<-c(683,758,631) #peatland, black spruce woodland, pp
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp

fuels.plot<-c("tree_loading_Mgha","cwd_loading_Mgha","duff_loading_Mgha","litter_loading_Mgha")

tabForPub.df<-data.frame(EVT=rep(c(evt.ew,evt.cf2),each=length(fuels.plot)),fuelType=rep(fuels.plot,6),prop0=NA,nGT0=NA,mean=NA,sd=NA,CV=NA)

tmp.id<-0
for(i in c(evt.ew,evt.cf2))
{
  tmp.line<-which(evt.vals==i)
  for(j in fuels.plot)
  {
    tmp.id<-tmp.id+1
    tabForPub.df$prop0[tmp.id]<-sum.stats.prop0[tmp.line,j]
    tabForPub.df$nGT0[tmp.id]<-sum.stats.n[tmp.line,j]
    tabForPub.df$mean[tmp.id]<-sum.stats.mean[tmp.line,j]
    tabForPub.df$sd[tmp.id]<-sum.stats.sd[tmp.line,j]
    tabForPub.df$CV[tmp.id]<-round(sum.stats.sd[tmp.line,j]/sum.stats.mean[tmp.line,j],digits=2)
    
  }
}
  
write.csv(tabForPub.df,file="SummaryLoadingTableForPub.csv",row.names = FALSE)  

tab2ForPub.df<-data.frame(EVT=rep(c(evt.ew,evt.cf2),each=length(fuels.plot)),fuelType=rep(fuels.plot,6),prop0=NA,nGT0=NA,mean=NA,sd=NA,CV=NA)

tmp.id<-0
for(i in c(evt.ew,evt.cf2))
{
  tmp.line<-which(evt.vals==i)
  for(j in fuels.plot)
  {
    tmp.id<-tmp.id+1
    tabForPub.df$prop0[tmp.id]<-sum.stats.prop0[tmp.line,j]
    tabForPub.df$nGT0[tmp.id]<-sum.stats.n[tmp.line,j]
    tabForPub.df$mean[tmp.id]<-sum.stats.mean[tmp.line,j]
    tabForPub.df$sd[tmp.id]<-sum.stats.sd[tmp.line,j]
    tabForPub.df$CV[tmp.id]<-round(sum.stats.sd[tmp.line,j]/sum.stats.mean[tmp.line,j],digits=2)
    
  }
}
