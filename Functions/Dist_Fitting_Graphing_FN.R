# Distribution Fitting Graphing Function
# This function focuses on graphing the data for each loading type at specific evts
# This function will automatically plot the histogram, boxplot, qqnorm and boxplot where values>0 into a pdf
# The distributions being examined are normal, lognormal, gamma, weibull
# The minimum plot value was arbitrarily chosen


dist.fit.graph.fn<-function(data.file,evts,evt.col,start.col,cur.cols=c(start.col:ncol(data)),min.plot=30,file.name="DistFitGraphEVT")
{
  for(k in 1:length(evts))
  {
    tmp.loads<-data.file[data.file[,evt.col]==evts[k],]
    
    tally.var<-data.frame(fueltype=names(data.file)[cur.cols],tally=NA)
    
    # writing graphs to a pdf
    pdf(file=paste(file.name,evts[k],".pdf",sep=""))
    
    # plotting for each loading based on EVT
    # plot histogram, boxplot, qqplot for each loading
    for(i in 3:32)
    {
      tally.var[i-2,2]<-length(tmp.loads[!is.na(tmp.loads[,i]),3])
      if(tally.var$tally[i-2]>min.plot)
      {
        cur.loads.vals<-tmp.loads[,as.character(tally.var$fueltype)[i-2]]
        par(mfrow=c(2,2))
        boxplot(cur.loads.vals,ylab=tally.var$fueltype[i-2])
        hist(cur.loads.vals,xlab=tally.var$fueltype[i-2],main=tally.var$fueltype[i-2])
        qqnorm(cur.loads.vals,main=tally.var$fueltype[i-2])
        qqline(cur.loads.vals)
        boxplot(cur.loads.vals[cur.loads.vals>0],main=paste(tally.var$fueltype[i-2],">0"))
      }
    }
    # closing pdf writing
    dev.off()
    
  }
}

