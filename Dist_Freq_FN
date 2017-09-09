#Used dist_rank_fn
#Lines 8-39, excluding the comments
{  dist.names<-c("normal","logNormal","gamma","weibull")
  
  for(i in 1:length(evts))
  {
  
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
        }
      }
    }
  }
}
#Error: object 'evts' not found      

#Lines from Functions(2)
#Line 27
EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd", write.file = TRUE)
#Line 42
evt.vals<-EVTTallies[[1]][EVTTallies[[1]][,2]>100,1]
#Line 46
distributionFitting <- dist.fit.fn(data.file, evts = evt.vals, start.col = 12, write.file = FALSE, min.plot = 100)
