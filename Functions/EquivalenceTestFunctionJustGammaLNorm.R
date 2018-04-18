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
                         ep.val=.25, include0=FALSE,add.val=0.01,usePTTE=FALSE,removeOut=FALSE,out.mult=4)
{
  equivalence.list<-list()
  for(i in 1:length(evts))
  {
    # summary dataframe
    equivalence.list[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],lnormResult=NA,gammaResult=NA) # 0 indicates not rejected, 1 rejected. 
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
        if(removeOut)
          cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]
        lnorm.cdf<-plnorm(cur.loads.vals,meanlog = distFit.obj[[i]]$lnorm.p1[j],
                  sdlog = distFit.obj[[i]]$lnorm.p2[j])
        gamma.cdf<-pgamma(cur.loads.vals,shape = distFit.obj[[i]]$gamma.p1[j],
                 rate = distFit.obj[[i]]$gamma.p2[j])
        cur.ecdf<-ecdf(cur.loads.vals)
        # observed-expected for the paired t-test for equivalance--something not working
        # Looks like poorer fits have higher SD, resulting in a smaller t and thereby reject.
        if(usePTTE)
        {
          equivalence.list[[i]]$lnormResult[j]<-ptte.data(lnorm.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$gammaResult[j]<-ptte.data(gamma.cdf-cur.ecdf(cur.loads.vals),
                                                         Epsilon = ep.val)$Dissimilarity # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
        }
        else
        {
           equivalence.list[[i]]$lnormResult[j]<-tost(lnorm.cdf-cur.ecdf(cur.loads.vals),
                                                     epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
          equivalence.list[[i]]$gammaResult[j]<-tost(gamma.cdf-cur.ecdf(cur.loads.vals),
                                                     epsilon = ep.val)$result # fail to reject dissimilarity means we can't say that the theoretical and empirical aren't different. Rejecting dissimilarity means we can say they're the same
        }
          
      }
    }
  }
  equivalence.list
}