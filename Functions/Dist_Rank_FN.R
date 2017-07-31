# Distribution Fitting Rank Function
# This function ranks the likelihood of the distribution fitting function
# The function reads the results of the Dist_Fitting_FN likelihoods.
# The function then ranks the LL and returns which distribution fit best.
# the file.inputname must be known in order to read the distribution fitting summary


distfit.rank.fn<-function(evts,file.inputname="DistFitSummaryEVT",write.file=FALSE,
                          file.outputname="DistFitRankEVT",DistFitSum)
{
  # loading LL results
#  DistFitSum<-list()
  
  # results data frame
  DistFitRank<-list()
  
  dist.names<-c("normal","logNormal","gamma","weibull")

  for(i in 1:length(evts))
  {
    # reading distribution fitting summary csv
#    DistFitSum[[i]]<-read.csv(paste(file.inputname,evts[i],".csv",sep=""))
    
    DistFitRank[[i]]<-data.frame(fueltype=names(data.file)[3:ncol(data.file)],dist.LL=NA,tie=0,dist1.fit=NA,dist2.fit=NA,dist3.fit=NA,dist4.fit=NA)
    
    dist.type<-as.data.frame(DistFitSum[[i]])
    
    for(j in 1:nrow(dist.type))
    {
      tmp.ll<-dist.type[j,seq(2,11,3)]
      tmp.ll<-tmp.ll[!is.na(tmp.ll)]
      if(length(tmp.ll)>0)
      {
        max.ll<-max(tmp.ll)
        DistFitRank[[i]][j,2]<-max.ll
        
        if(!is.na(max.ll))
        {
          distfit.id<-which(dist.type[j,seq(2,11,3)]==max.ll)
          tmp.sort<-sort.int(unlist(dist.type[j,seq(2,11,3)]),index.return = TRUE,decreasing=TRUE)
          DistFitRank[[i]][j,4:7]<-dist.names[tmp.sort$ix]
          
          if(length(distfit.id)>1)
          {
            DistFitRank[[i]]$tie[j]<-1
          }
          
          if(write.file)
          {
            write.csv(DistFitRank[[i]],file=paste(file.outputname,evts[i],".csv",sep = ""),row.names=FALSE) 
          }
        }
      }
    }
  }
  return(DistFitRank)
}


