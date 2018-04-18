############
# a function to fit a hurdle "model" to fuel
# loading distributions.
# This will be a simple proportion of zeroes
# With candidate non-zero distributions compared
#########

dist.custom.hurdle.fit.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,
                             add.val=0.1,verbose=FALSE,distrs=c("lnorm","gamma","norm","weibull"),include0=FALSE,
                             removeOut=FALSE,out.mult=4)
{
  distfitW0.df<-list() # include zeroes in estimated distribution
  distfitHurdle.df<-list() # estimate zeroes separately
  cur.cols=c(start.col:ncol(data.file))

  for(i in 1:length(evts))
  {
    # summary dataframe
    if(verbose)
      print(paste("starting evt",evts[i]))
    distfitW0.df[[i]]<-data.frame(matrix(NA,ncol=2+4*length(distrs),nrow=length(cur.cols)))
    distfitHurdle.df[[i]]<-data.frame(matrix(NA,ncol=3+4*length(distrs),nrow=length(cur.cols)))
    
    names(distfitW0.df[[i]])[1]<-names(distfitHurdle.df[[i]])[1]<-"fueltype"
    for(k in 1:length(distrs))
    {
      names(distfitW0.df[[i]])[k*4-2]<-names(distfitHurdle.df[[i]])[k*4-2]<-paste(distrs[k],"LL",sep="")
      names(distfitW0.df[[i]])[k*4-1]<-names(distfitHurdle.df[[i]])[k*4-1]<-paste(distrs[k],".ks",sep="")
      names(distfitW0.df[[i]])[k*4]<-names(distfitHurdle.df[[i]])[k*4]<-paste(distrs[k],".p1",sep="")
      names(distfitW0.df[[i]])[k*4+1]<-names(distfitHurdle.df[[i]])[k*4+1]<-paste(distrs[k],".p2",sep="")
    }
    names(distfitW0.df[[i]])[length(distrs)*4+2]<-names(distfitHurdle.df[[i]])[length(distrs)*4+2]<-"n.obs"
    names(distfitHurdle.df[[i]])[length(distrs)*4+3]<-"prop0"
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # include all values including zeroes
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0] # only fit for non-zero values
      
      if(removeOut) # exclude outliers from distribution estimation
        cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]
      
      if(length(cur.loads.vals)>min.plot) # only estimate if the continuous portion has > min.plot observations
      {
        # Tally the 0's
        distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<-length(cur.loads.vals0[cur.loads.vals0==0])/length(cur.loads.vals0)
        distfitHurdle.df[[i]]$n.obs[j-(start.col-1)]<-length(cur.loads.vals) # tally the sample size for the non-zero part
        distfitW0.df[[i]]$n.obs[j-(start.col-1)]<-length(cur.loads.vals0)
        # Estimate the continuous distributions, both hurdle and including zero
        # loop through the distrs
        for(k in 1:length(distrs))
        {
          if(verbose)
            print(paste(distrs[k],"Hurdle"))
          cur.fit<-fitdist(cur.loads.vals,distr=distrs[k])
          cur.ll<-cur.fit$loglik
          cur.test<-gofstat(cur.fit)
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".ks",sep="")]<-cur.test$ks
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],"LL",sep="")]<-round(cur.ll,digits=0)
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".p1",sep="")]<-cur.fit$estimate[1]
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".p2",sep="")]<-cur.fit$estimate[2]

          if(include0)
          {
            cur0.fit<-fitdist(cur.loads.vals0+add.val,distr=distrs[k])
            cur0.ll<-cur0.fit$loglik
            cur0.test<-gofstat(cur0.fit)
            distfitW0.df[[i]][j-(start.col-1),paste(distrs[k],".ks",sep="")]<-cur0.test$ks
            distfitW0.df[[i]][j-(start.col-1),paste(distrs[k],"LL",sep="")]<-round(cur0.ll,digits=0)
            distfitW0.df[[i]][j-(start.col-1),paste(distrs[k],".p1",sep="")]<-cur0.fit$estimate[1]
            distfitW0.df[[i]][j-(start.col-1),paste(distrs[k],".p2",sep="")]<-cur0.fit$estimate[2]
          }
        }
      }
    }
  }
  return(list(WithZeroFit=distfitW0.df,HurdleFit=distfitHurdle.df))
}