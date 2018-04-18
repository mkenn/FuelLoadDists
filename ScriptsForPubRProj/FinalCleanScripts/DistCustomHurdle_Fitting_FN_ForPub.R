############
# DistCustomHurdle_Fitting_FN.R
# a function to estimate a hurdle-like distribution to fuel
# loading.
# Overview: Estimate the proportion of zeroes for a given 
# fuel type and EVT Group
# For non-zero data, estimate each of the indicated theoretical distributions
# The distributions require a R-function such as qnorm, etc, and 2 parameter
# Includes option to remove outliers from distribution estimates, with
# a user-specified multiplier for the IQR.
# If this option is chosen, then any observations > Q_3+out.mult*IQR are omitted
# there is also an option to include zero values and not estimate the
# hurdle distribution, although this is not used in the study
#########

dist.custom.hurdle.fit.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,
                             add.val=0.1,verbose=FALSE,distrs=c("lnorm","gamma","norm","weibull"),include0=FALSE,
                             removeOut=FALSE,out.mult=4)
{
  distfitHurdle.df<-list() # results where zeroes are estimated separately
  cur.cols=c(start.col:ncol(data.file)) # identify which columns in the data file correspond to the fuel types

  for(i in 1:length(evts)) # loop through the EVT groups for which distributions will be estimated
  {
# summary dataframe
# this summary data frame will include likelihood values, parameter estimates, and KS statistic values for each estimated distributions
    if(verbose)
      print(paste("starting evt",evts[i]))
    distfitHurdle.df[[i]]<-data.frame(matrix(NA,ncol=3+4*length(distrs),nrow=length(cur.cols)))
    
    for(k in 1:length(distrs)) # create columns for estimated parameters and statistics for each distribution
    {
     names(distfitHurdle.df[[i]])[k*4-2]<-paste(distrs[k],"LL",sep="")
      names(distfitHurdle.df[[i]])[k*4-1]<-paste(distrs[k],".ks",sep="")
      names(distfitHurdle.df[[i]])[k*4]<-paste(distrs[k],".p1",sep="")
      names(distfitHurdle.df[[i]])[k*4+1]<-paste(distrs[k],".p2",sep="")
    }
    names(distfitHurdle.df[[i]])[length(distrs)*4+2]<-"n.obs"
    names(distfitHurdle.df[[i]])[length(distrs)*4+3]<-"prop0"
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      tmp.loads<-tmp.loads[!is.na(tmp.loads)]
#      cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # include all values including zeroes
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0] # only fit for non-zero values
      
      if(removeOut) # exclude outliers from distribution estimation
        cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]
      
      if(length(cur.loads.vals)>min.plot) # only estimate if the continuous portion has > min.plot observations
      {
        # Tally the 0's
        distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<-length(tmp.loads[tmp.loads==0])/length(tmp.loads)
        distfitHurdle.df[[i]]$n.obs[j-(start.col-1)]<-length(cur.loads.vals) # tally the sample size for the non-zero part
        # Estimate the continuous distributions
        # loop through the distrs
        for(k in 1:length(distrs))
        {
          if(verbose)
            print(paste(distrs[k],"Hurdle"))
          cur.fit<-fitdist(cur.loads.vals,distr=distrs[k]) # estimate the distribution
          cur.ll<-cur.fit$loglik # extract the likelihood values
          cur.test<-gofstat(cur.fit) # calculate goodness of fit statistics
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".ks",sep="")]<-cur.test$ks # record the Kolmogorov-Smirnov statistic
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],"LL",sep="")]<-round(cur.ll,digits=0) # record the log-likelihood
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".p1",sep="")]<-cur.fit$estimate[1] # record the estimate for the first parameter
          distfitHurdle.df[[i]][j-(start.col-1),paste(distrs[k],".p2",sep="")]<-cur.fit$estimate[2] # record the estimate for the second parameter

        }
      }
    }
  }
  return(HurdleFit=distfitHurdle.df) # return the list, whose length is the same as the length of evts passed to the function
}