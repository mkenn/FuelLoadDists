# Distribution Fitting Rank Function
# This function ranks the likelihood of the distribution fitting function
# The function reads the results of the Dist_Fitting_FN likelihoods.
# The function then ranks the LL and returns which distribution fit best.
# the file.inputname must be known in order to read the distribution fitting summary


distfit.rank.fn<-function(evts,file.inputname="DistFitSummaryEVT",write.file=FALSE,
                          file.outputname="DistFitRankEVT",DistFitSum,start.col,
                          dist.cols=c("lnormLL","gammaLL","normLL","weibullLL"))
{
  # results data frame
  DistFitRank<-list()
  
  for(i in 1:length(evts)) # for each EVT group for which distributions were estimated
  {
# create a dataframe that orders the distributions by descending likelihood,
# with the maximum likelihood given rank 1, and recording whether the likelihoods are
# the same when rounded to the nearest integer (tie = 1)
    DistFitRank[[i]]<-data.frame(fueltype=names(data.file)[start.col:ncol(data.file)],dist.LL=NA,tie=0,dist1.fit=NA,dist2.fit=NA,dist3.fit=NA,dist4.fit=NA)
    
    dist.type<-as.data.frame(DistFitSum[[i]])
    
    for(j in 1:nrow(dist.type))
    {
      tmp.ll<-dist.type[j,dist.cols]
      
      tmp.ll<-tmp.ll[!is.na(tmp.ll)] # determine if the distribution was estimated 
      if(length(tmp.ll)>0)
      {
        max.ll<-max(tmp.ll) # identify the maximum likelihood value
        DistFitRank[[i]][j,2]<-max.ll
        
        if(!is.na(max.ll)) # make sure it is a value
        {
          distfit.id<-which(dist.type[j,dist.cols]==max.ll) # identify the distribution corresponding to the maximum likelihood
          tmp.sort<-sort.int(unlist(dist.type[j,dist.cols]),index.return = TRUE,decreasing=TRUE) # sort the distributions by likelihood values
          DistFitRank[[i]][j,4:7]<-dist.cols[tmp.sort$ix] # and return the sorted distributions

          if(length(distfit.id)>1) # more than 1 distribution achieved the maximum likelihood value
          {
            DistFitRank[[i]]$tie[j]<-1 # so label it a tie
          }
          
          if(write.file) # use if you want to write the results to a file
          {
            write.csv(DistFitRank[[i]],file=paste(file.outputname,evts[i],".csv",sep = ""),row.names=FALSE) 
          }
        }
      }
    }
  }
  return(DistFitRank)
}


