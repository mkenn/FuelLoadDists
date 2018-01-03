###########
# function to summarize how often distributions
# are chosen across EVTs
# for each fuel loading category in the database
# requires the result of Dist_Rank_FN call
###########

distfit.summary.fn<-function(distRankObj,data.df,start.col)
{
  distSummary<-data.frame(loadingNames=names(data.df)[start.col:ncol(data.df)],
                          normal=0,logNormal=0,gamma=0,weibull=0)
  for(i in 1:length(distRankObj))
  {
    cur.rank<-distRankObj[[i]]
    for(j in 1:nrow(cur.rank))
    {
      
      if(!is.na(cur.rank[j,4]))
      {
        cur.col<-which(!is.na(match(names(distSummary),cur.rank[j,4])))
        distSummary[j,cur.col]<-distSummary[j,cur.col]+1
        
        if(cur.rank[j,3]==1)
        {
          cur.col<-which(!is.na(match(names(distSummary),cur.rank[j,5])))
          distSummary[j,cur.col]<-distSummary[j,cur.col]+1
          
        }
      }
    }
  }
  distSummary
}