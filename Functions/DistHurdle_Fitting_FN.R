############
# a function to fit a hurdle "model" to fuel
# loading distributions.
# This will be a simple proportion of zeroes
# With candidate non-zero distributions compared
#########

dist.hurdle.fit.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,add.val=0.1,verbose=FALSE)
{
  distfitW0.df<-list() # include zeroes in estimated distribution
  distfitHurdle.df<-list() # estimate zeroes separately
  cur.cols=c(start.col:ncol(data.file))

  for(i in 1:length(evts))
  {
    # summary dataframe
    if(verbose)
      print(paste("starting evt",evts[i]))
    distfitW0.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.ks=NA,norm.mu=NA,norm.sigma=NA,
                                  lnormLL=NA,lnorm.ks=NA,lnorm.mu=NA,lnorm.sigma=NA,
                                  gammaLL=NA,gamma.ks=NA,gamma.shape=NA,gamma.rate=NA,
                                  weibullLL=NA,weibull.ks=NA,weibull.shape=NA,weibull.scale=NA,n.obs=NA)
    distfitHurdle.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.ks=NA,norm.mu=NA,norm.sigma=NA,
                                      lnormLL=NA,lnorm.ks=NA,lnorm.mu=NA,lnorm.sigma=NA,
                                      gammaLL=NA,gamma.ks=NA,gamma.shape=NA,gamma.rate=NA,
                                      weibullLL=NA,weibull.ks=NA,weibull.shape=NA,weibull.scale=NA,prop0=NA,n.obs=NA)
    
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # include all values including zeroes
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0] # only fit for non-zero values
      
      if(length(cur.loads.vals)>min.plot) # only estimate if the continuous portion has > min.plot observations
      {
        # Tally the 0's
        distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<-length(cur.loads.vals0[cur.loads.vals0==0])/length(cur.loads.vals0)
        distfitHurdle.df[[i]]$n.obs[j-(start.col-1)]<-length(cur.loads.vals) # tally the sample size for the non-zero part
        # Estimate the hurdle continuous distributions
        ############# normal after hurdle##############
        if(verbose)
          print("NormalHurdle")
        normal.fit<-fitdist(cur.loads.vals,distr="norm")
        normal.ll<-normal.fit$loglik
        test.normal<-gofstat(normal.fit)
        distfitHurdle.df[[i]]$norm.ks[j-(start.col-1)]<-test.normal$ks
        distfitHurdle.df[[i]]$normLL[j-(start.col-1)]<-round(normal.ll,digits=0)
        distfitHurdle.df[[i]]$norm.mu[j-(start.col-1)]<-normal.fit$estimate[1]
        distfitHurdle.df[[i]]$norm.sigma[j-(start.col-1)]<-normal.fit$estimate[2]
        
        ############# lognormal ##############
        if(verbose)
          print("LNormalHurdle")
        lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")
        lnorm.ll<-lnorm.fit$loglik
        test.lnorm<-gofstat(lnorm.fit)
        distfitHurdle.df[[i]]$lnorm.ks[j-(start.col-1)]<-test.lnorm$ks
        distfitHurdle.df[[i]]$lnormLL[j-(start.col-1)]<-round(lnorm.ll,digits=0)
        distfitHurdle.df[[i]]$lnorm.mu[j-(start.col-1)]<-lnorm.fit$estimate[1]
        distfitHurdle.df[[i]]$lnorm.sigma[j-(start.col-1)]<-lnorm.fit$estimate[2]
        
        ############# gamma ##############
        if(verbose)
          print("GammaHurdle")
        gamma.fit<-fitdist(cur.loads.vals,distr="gamma")
        gamma.ll<-gamma.fit$loglik
        test.gamma<-gofstat(gamma.fit)
        distfitHurdle.df[[i]]$gamma.ks[j-(start.col-1)]<-test.gamma$ks
        distfitHurdle.df[[i]]$gammaLL[j-(start.col-1)]<-round(gamma.ll,digits=0)
        distfitHurdle.df[[i]]$gamma.shape[j-(start.col-1)]<-gamma.fit$estimate[1]
        distfitHurdle.df[[i]]$gamma.rate[j-(start.col-1)]<-gamma.fit$estimate[2]
        
        ############# weibull ##############
        if(verbose)
          print("WeibullHurdle")
        weibull.fit<-fitdist(cur.loads.vals,distr="weibull")
        weibull.ll<-weibull.fit$loglik
        test.weibull<-gofstat(weibull.fit)
        distfitHurdle.df[[i]]$weibull.ks[j-(start.col-1)]<-test.weibull$ks
        distfitHurdle.df[[i]]$weibullLL[j-(start.col-1)]<-round(weibull.ll,digits = 0)
        distfitHurdle.df[[i]]$weibull.shape[j-(start.col-1)]<-weibull.fit$estimate[1]
        distfitHurdle.df[[i]]$weibull.scale[j-(start.col-1)]<-weibull.fit$estimate[2]
        
        ###
        # Including 0's--note for consistency add.val is added to all loads vectors
        ###
        distfitW0.df[[i]]$n.obs[j-(start.col-1)]<-length(cur.loads.vals0)
        ############# normal ##############
        normal.fit<-fitdist(cur.loads.vals0+add.val,distr="norm")
        normal.ll<-normal.fit$loglik
        if(distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<0.5) # only bother with gof stats if prop0<0.5
          test.normal<-gofstat(normal.fit)
        distfitW0.df[[i]]$norm.ks[j-(start.col-1)]<-test.normal$ks
        distfitW0.df[[i]]$normLL[j-(start.col-1)]<-round(normal.ll,digits=0)
        distfitW0.df[[i]]$norm.mu[j-(start.col-1)]<-normal.fit$estimate[1]
        distfitW0.df[[i]]$norm.sigma[j-(start.col-1)]<-normal.fit$estimate[2]
        
        ############# lognormal ##############
        lnorm.fit<-fitdist(cur.loads.vals0+add.val,distr="lnorm") # lognormal does not accommodate non-zero entries
        lnorm.ll<-lnorm.fit$loglik
        if(distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<0.5)
          test.lnorm<-gofstat(lnorm.fit)
        distfitW0.df[[i]]$lnorm.ks[j-(start.col-1)]<-test.lnorm$ks
        distfitW0.df[[i]]$lnormLL[j-(start.col-1)]<-round(lnorm.ll,digits=0)
        distfitW0.df[[i]]$lnorm.mu[j-(start.col-1)]<-lnorm.fit$estimate[1]
        distfitW0.df[[i]]$lnorm.sigma[j-(start.col-1)]<-lnorm.fit$estimate[2]
          
        ############# gamma ##############
        gamma.fit<-fitdist(cur.loads.vals0+add.val,distr="gamma")
        gamma.ll<-gamma.fit$loglik
        if(distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<0.5)
          test.gamma<-gofstat(gamma.fit)
        distfitW0.df[[i]]$gamma.ks[j-(start.col-1)]<-test.gamma$ks
        distfitW0.df[[i]]$gammaLL[j-(start.col-1)]<-round(gamma.ll,digits=0)
        distfitW0.df[[i]]$gamma.shape[j-(start.col-1)]<-gamma.fit$estimate[1]
        distfitW0.df[[i]]$gamma.rate[j-(start.col-1)]<-gamma.fit$estimate[2]
          
        ############# weibull ##############
        weibull.fit<-fitdist(cur.loads.vals0+add.val,distr="weibull")
        weibull.ll<-weibull.fit$loglik
        if(distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<0.5)
          test.weibull<-gofstat(weibull.fit)
        distfitW0.df[[i]]$weibull.ks[j-(start.col-1)]<-test.weibull$ks
        distfitW0.df[[i]]$weibullLL[j-(start.col-1)]<-round(weibull.ll,digits = 0)
        distfitW0.df[[i]]$weibull.shape[j-(start.col-1)]<-weibull.fit$estimate[1]
        distfitW0.df[[i]]$weibull.scale[j-(start.col-1)]<-weibull.fit$estimate[2]
       }
    }
  }
  return(list(WithZeroFit=distfitW0.df,HurdleFit=distfitHurdle.df))
}