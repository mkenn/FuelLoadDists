#Used dist_rank_fn
#Lines 8-39, excluding the comments

### MCK: First run functions(2) through distfit.rank.fn
#Line 27
EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd", write.file = TRUE)
#Line 42
evt.vals<-EVTTallies[[1]][EVTTallies[[1]][,2]>100,1]
#Line 46
distributionFitting <- dist.fit.fn(data.file, evts = evt.vals, start.col = 12, write.file = FALSE, min.plot = 100)
# Line 49
distributionRanking<-distfit.rank.fn(evts = evt.vals,DistFitSum = distributionFitting,start.col=12)
### Then use this to conduct the tally

  dist.names<-c("normal","logNormal","gamma","weibull")
  
  rank1.f.df<-data.frame(fueltype=names(data.file)[start.col:ncol(data.file)],
                         norm.f=0,lnorm.f=0,gamma.f=0,weibull.f=0)
 
  
  for(i in 1:length(evt.vals))
  {
  
    cur.fits<-as.data.frame(distributionRanking[[i]])
    
    for(j in 1:nrow(cur.fits))
    {
      if(!is.na(cur.fits$dist.LL[j]))
      {
        cur.r1<-match(cur.fits[j,4],dist.names) # same as: which(dist.names==cur.fits[j,4])
        rank1.f.df[j,cur.r1+1]<-rank1.f.df[j,cur.r1+1]+1
      }
    }
  }
  
# Next, determine the proportion of times for each loading
# that each distribution is chosen
#Error: object 'evts' not found      

#Lines from Functions(2)
