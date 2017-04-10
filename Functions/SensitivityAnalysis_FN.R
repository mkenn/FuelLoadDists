  # Sensitivity Analysis
# completing the sensitivity analysis of the data for specific evts for each loading type
# this analysis will help determine the sensitivity of the data to change
# evts 642,660, 666 were selected as default as a comparison tool to another data set
# the lower and upper quantiles of 10% and 90% were arbitrarily selected
# the min of 30 was arbitrarily selected
# SAMO = sensitivity analysis of model output


samo.fn<-function(data,evts=c(642,660,666),start.col=3,cur.cols=c(start.col:ncol(data)),evt.col="LFEVTGroupCd_FINAL",min.n=30,q.lower=0.1,q.upper=0.9,write.file=FALSE,file.name="SensitivityAnalysis_")
{
  samo.list<-list()
  
  for(i in 1:length(evts))
  {
    samo.list[[i]]<-data.frame(fueltypes=names(data)[cur.cols],lower.q=NA,upper.q=NA,min=NA,max=NA)
    
    for(j in cur.cols)
    {
      tmp.loads<-data[data[,evt.col]==evts[i],j]
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)]
      
      if(length(cur.loads.vals)>min.n)
      {
        samo.list[[i]][j-(start.col-1),2]<-quantile(cur.loads.vals,q.lower)
        samo.list[[i]][j-(start.col-1),3]<-quantile(cur.loads.vals,q.upper)
        samo.list[[i]][j-(start.col-1),4]<-min(cur.loads.vals)
        samo.list[[i]][j-(start.col-1),5]<-max(cur.loads.vals)
      }
      
      if(write.file)
      {
        write.csv(samo.list[[i]],file=paste(file.name,evts[i],"minQ",q.lower,"maxQ",q.upper,".csv",sep=""),row.names=FALSE)
      }
    }
  }
  return(samo.list)
}
