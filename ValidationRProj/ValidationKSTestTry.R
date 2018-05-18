#####
# run bootstrap on database-estimated distributions
# compared to empirical distributions
#####
# step 1, compute the observed KS statistic
# step 2, calculate the MC p-value using function
# this will have to compare the non-zero values
# we can also do a hypothesis test on the proportion of zeroes
#######
# first load the fitdist workspace
# also assumes have completed ValidationEDATry1.R
load("../DistFittingRProj/Workspaces/HurdleCustomFitsNoZeroWNoOut.RData")
# the problem is this has a particular order to evt.vals

sage.ks.pvals<-matrix(NA,ncol=length(sage.fuelTypes),nrow=length(sage.evts))
srs.ks.pvals<-matrix(NA,ncol=length(srs.fuelTypes),nrow=length(srs.evts))
# definitely start with srs.ks.pvals, because of the issues with sage

srs.evts.match<-match(srs.evts,evt.vals)
distFits.ids<-srs.data.cols-12

n.iter<-5000
for(i in 1:length(srs.evts.match))
{
  if(!is.na(srs.evts.match[i]))
  {
    for(j in 1:length(srs.fuelTypes)) # still need observed values for the observed KS stat, or return that in
      #distribution fitting object
    {
      if(!is.na(distributionCustomRankingHurdleNOut[[srs.evts.match[i]]]$dist1.fit[srs.data.cols[j]-start.col+1]))
      {
        tmp.obs<-srs.df[srs.df[,srs.evgroupCol]==srs.evts[i],srs.fuelTypes[j]]
        tmp.obs<-tmp.obs[!is.na(tmp.obs)]
        tmp.obs<-tmp.obs[tmp.obs>0]
        if(length(tmp.obs)>min.plot)
        {
          cur.distr<-switch(distributionCustomRankingHurdleNOut[[srs.evts.match[i]]]$dist1.fit[srs.data.cols[j]-start.col+1],
                         lnormLL="lnorm",
                         gammaLL="gamma")
          tmp.ks<-switch(cur.distr,
                         lnorm=ks.test(tmp.obs,"plnorm",
                                       distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$lnorm.p1[srs.data.cols[j]-start.col+1],
                                       distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$lnorm.p2[srs.data.cols[j]-start.col+1]),
                         gamma=ks.test(tmp.obs,"pgamma",distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$gamma.p1[srs.data.cols[j]-start.col+1],
                                       distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$gamma.p2[srs.data.cols[j]-start.col+1]))
          tmp.obs.stat<-tmp.ks$statistic
          tmp.fn<-switch(cur.distr,
                          lnorm=KSMC.fn(distr = cur.distr,param1=distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$lnorm.p1[srs.data.cols[j]-start.col+1],
                          param2=distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$lnorm.p2[srs.data.cols[j]-start.col+1],niter = n.iter,
                          nsamp = length(tmp.obs),obs.stat =tmp.obs.stat),
                         gamma=KSMC.fn(distr = cur.distr,param1=distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$gamma.p1[srs.data.cols[j]-start.col+1],
                                       param2=distributionCustomFittingHurdleNOut$HurdleFit[[srs.evts.match[i]]]$gamma.p2[srs.data.cols[j]-start.col+1],niter = n.iter,
                                       nsamp = length(tmp.obs),obs.stat =tmp.obs.stat))
          srs.ks.pvals[i,j]<-tmp.fn[[1]]
        }
      }
    }
  }
}
save.image("KS_MCFnTestResultsSRSValidation.RData")


