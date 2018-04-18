##########
# function to perform equivalence test using library equivalence
# tests H0: the distributions are DISsimilar. Rejecting this H0 means that
# we can accept the theoretical distribution for the dataset.
# We will use the empirical parameters for the theoretical
# distribution 
# this is complementary to the best fitting distribution, which 
# should be completed first. Requires the estimated distribution parameters
# Or, call internally from this function?
#########
equivalence.fn<-function(data.file,distFit.obj,evts,evt.col,start.col,cur.cols,
                         min.plot=30,write.file=FALSE,file.name="EquivalenceEVT",
                         ep.val=.25, include0=FALSE,add.val=0.01,usePTTE=FALSE)
{
  equivalence.list<-list()
  for(i in 1:length(evts))
  {
    # summary dataframe
    equivalence.list[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normResult=NA,lnormResult=NA,
                                      gammaResult=NA,weibullResult=NA) # 0 indicates not rejected, 1 rejected. 
    #Looking for distributions with 1's, where the null that they AREN'T equivalent is rejected
    for(j in 1:length(cur.cols))
    {
      if(!is.na(distFit.obj[[i]][j,2])) # so we have fit statistics
      {
        tmp.loads<-data.file[data.file[,evt.col]==evts[i],cur.cols[j]]
        if(include0)
          cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0]+add.val
        
        else
            cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
        norm.cdf<-pnorm(cur.loads.vals,mean=distFit.obj[[i]]$norm.mu[j], # not sure why this wasn't giving out of bounds errors
                sd=distFit.obj[[i]]$norm.sigma[j])
        lnorm.cdf<-plnorm(cur.loads.vals,meanlog = distFit.obj[[i]]$lnorm.mu[j],
                  sdlog = distFit.obj[[i]]$lnorm.sigma[j])
        gamma.cdf<-pgamma(cur.loads.vals,shape = distFit.obj[[i]]$gamma.shape[j],
                 rate = distFit.obj[[i]]$gamma.rate[j])
        weibull.cdf<-pweibull(cur.loads.vals,shape = distFit.obj[[i]]$weibull.shape[j],
                      scale = distFit.obj[[i]]$weibull.scale[j])
        cur.ecdf<-ecdf(cur.loads.vals)
        # observed-expected for the paired t-test for equivalance--something not working
        # Looks like poorer fits have higher SD, resulting in a smaller t and thereby reject.
        if(usePTTE)
        {
          equivalence.list[[i]]$normResult[j]<-ptte.data(norm.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$lnormResult[j]<-ptte.data(lnorm.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$gammaResult[j]<-ptte.data(gamma.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$weibullResult[j]<-ptte.data(weibull.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
        }
        else
        {
          equivalence.list[[i]]$normResult[j]<-tost(norm.cdf-cur.ecdf(cur.loads.vals),
                                                         epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$lnormResult[j]<-tost(lnorm.cdf-cur.ecdf(cur.loads.vals),
                                                     epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$gammaResult[j]<-tost(gamma.cdf-cur.ecdf(cur.loads.vals),
                                                     epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$weibullResult[j]<-tost(weibull.cdf-cur.ecdf(cur.loads.vals),
                                                       epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
        }
          
      }
    }
  }
  equivalence.list
}