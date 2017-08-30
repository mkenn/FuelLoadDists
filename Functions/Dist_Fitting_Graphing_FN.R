# Distribution Fitting Graphing Function
# This function focuses on graphing the data for each loading type at specific evts
# This function will automatically plot the histogram, boxplot, qqnorm and boxplot where values>0 into a pdf
# The distributions being examined are normal, lognormal, gamma, weibull
# The minimum plot value was arbitrarily chosen

#DM: Indexing for new database completed, trouble with PDF. The file is created but there are no graphs within the PDF file

dist.fit.graph.fn<-function(data.file,evts,evt.col,start.col,cur.cols=c(start.col:ncol(data.file)),min.plot=30,file.name="DistFitGraphEVT")
{
  cur.cols=c(start.col:ncol(data.file))
  for(k in 1:length(evts))
  {
    # so here we isolate the evt
    tmp.loads<-data.file[data.file[,evt.col]==evts[k],]
    
#    tally.var<-data.frame(fueltype=names(data.file)[cur.cols],tally=NA)
# this is not necessary, we're not looking to return a tally    
    # writing graphs to a pdf
    pdf(file=paste(file.name,evts[k],".pdf",sep=""))
    
    # plotting for each loading based on EVT
    # plot histogram, boxplot, qqplot for each loading
    for(i in cur.cols) # so here we concentrate on each loading
    {
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads[,i])&tmp.loads[,i]>0,i]
      #tally.var[i-(start.col-1),2]<-length(tmp.loads[!is.na(tmp.loads[,i]),3])
      # so we want to tally the current fuel loading
#      if(tally.var$tally[i-(start.col-1)]>min.plot)
        if(length(cur.loads.vals)>min.plot)
        {
#        cur.loads.vals<-tmp.loads[,as.character(tally.var$fueltype)[start.col-1]]
        par(mfrow=c(2,2))
#        boxplot(cur.loads.vals,ylab=tally.var$fueltype[start.col-1])
        # boxplot(cur.loads.vals,ylab=names(data.file)[i])
        # hist(cur.loads.vals,xlab=tally.var$fueltype[start.col-1],main=tally.var$fueltype[start.col-1])
        # qqnorm(cur.loads.vals,main=tally.var$fueltype[start.col-1])
        # qqline(cur.loads.vals)
        # boxplot(cur.loads.vals[cur.loads.vals>0],main=paste(tally.var$fueltype[start.col-1],">0"))
        boxplot(cur.loads.vals,ylab=names(data.file)[i])
        hist(cur.loads.vals,xlab=names(data.file)[i],main=paste("EVT",evts[k]))
        qqnorm(cur.loads.vals,main=names(data.file)[i])
        qqline(cur.loads.vals)
        }
    }
    # closing pdf writing
    dev.off()
  }
  
}

