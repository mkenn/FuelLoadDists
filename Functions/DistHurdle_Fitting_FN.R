############
# a function to fit a hurdle "model" to fuel
# loading distributions.
# This will be a simple proportion of zeroes
# With candidate non-zero distributions compared
#########

dist.hurdle.fit.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,add.val=0.1)
{
  distfitW0.df<-list()
  distfitHurdle.df<-list()
  cur.cols=c(start.col:ncol(data.file))

  for(i in 1:length(evts))
  {
    # summary dataframe
    print(paste("starting evt",evts[i]))
    distfitW0.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.mu=NA,norm.sigma=NA,lgnormLL=NA,
                                lnorm.mu=NA,lnorm.sigma=NA,gammaLL=NA,gamma.shape=NA,gamma.rate=NA,weibullLL=NA,weibull.shape=NA,weibull.scale=NA)
    distfitHurdle.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.mu=NA,norm.sigma=NA,lgnormLL=NA,
                                    lnorm.mu=NA,lnorm.sigma=NA,gammaLL=NA,gamma.shape=NA,gamma.rate=NA,weibullLL=NA,
                                    weibull.shape=NA,weibull.scale=NA,prop0=NA)
    
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # include all values including zeroes
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0] # only fit for non-zero values
      
      if(length(cur.loads.vals)>min.plot) # only estimate if the continuous portion has > min.plot observations
      {
        # Tally the 0's
        distfitHurdle.df[[i]]$prop0[j-(start.col-1)]<-length(cur.loads.vals0[cur.loads.vals0==0])/length(cur.loads.vals0)
        # Estimate the hurdle continuous distributions
        ############# normal after hurdle##############
        normal.fit<-fitdist(cur.loads.vals,distr="norm")
        normal.ll<-normal.fit$loglik
        #  test.normal<-gofstat(normal.fit)
        distfitW0.df[[i]][j-(start.col-1),2]<-round(normal.ll,digits=0)
        distfitW0.df[[i]][j-(start.col-1),3]<-round(normal.fit$estimate[1],digits=2)
        distfitW0.df[[i]][j-(start.col-1),4]<-round(normal.fit$estimate[2],digits=2)
        
        ############# lognormal ##############
        lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")
        lnorm.ll<-lnorm.fit$loglik
        #  test.lnorm<-gofstat(lnorm.fit)
        distfitW0.df[[i]][j-(start.col-1),5]<-round(lnorm.ll,digits=0)
        distfitW0.df[[i]][j-(start.col-1),6]<-round(lnorm.fit$estimate[1],digits=2)
        distfitW0.df[[i]][j-(start.col-1),7]<-round(lnorm.fit$estimate[2],digits=2)
        
        ############# gamma ##############
        gamma.fit<-fitdist(cur.loads.vals,distr="gamma")
        gamma.ll<-gamma.fit$loglik
        #  test.gamma<-gofstat(gamma.fit)
        distfitW0.df[[i]][j-(start.col-1),8]<-round(gamma.ll,digits=0)
        distfitW0.df[[i]][j-(start.col-1),9]<-round(gamma.fit$estimate[1],digits=2)
        distfitW0.df[[i]][j-(start.col-1),10]<-round(gamma.fit$estimate[2],digits=2)
        
        ############# weibull ##############
        weibull.fit<-fitdist(cur.loads.vals,distr="weibull")
        weibull.ll<-weibull.fit$loglik
        #   test.weibull<-gofstat(weibull.fit)
        distfitW0.df[[i]][j-(start.col-1),11]<-round(weibull.ll,digits = 0)
        distfitW0.df[[i]][j-(start.col-1),12]<-round(weibull.fit$estimate[1],digits=2)
        distfitW0.df[[i]][j-(start.col-1),13]<-round(weibull.fit$estimate[2],digits=2)
        
        ###
        # Including 0's--note for consistency add.val is added to all loads vectors
        ###
        ############# normal ##############
        normal.fit<-fitdist(cur.loads.vals0+add.val,distr="norm")
        normal.ll<-normal.fit$loglik
        #  test.normal<-gofstat(normal.fit)
        distfitHurdle.df[[i]][j-(start.col-1),2]<-round(normal.ll,digits=0)
        distfitHurdle.df[[i]][j-(start.col-1),3]<-round(normal.fit$estimate[1],digits=2)
        distfitHurdle.df[[i]][j-(start.col-1),4]<-round(normal.fit$estimate[2],digits=2)
        
        ############# lognormal ##############
        lnorm.fit<-fitdist(cur.loads.vals0+add.val,distr="lnorm") # lognormal does not accommodate non-zero entries
        lnorm.ll<-lnorm.fit$loglik
        #  test.lnorm<-gofstat(lnorm.fit)
        distfitHurdle.df[[i]][j-(start.col-1),5]<-round(lnorm.ll,digits=0)
        distfitHurdle.df[[i]][j-(start.col-1),6]<-round(lnorm.fit$estimate[1],digits=2)
        distfitHurdle.df[[i]][j-(start.col-1),7]<-round(lnorm.fit$estimate[2],digits=2)
          
        ############# gamma ##############
        gamma.fit<-fitdist(cur.loads.vals0+add.val,distr="gamma")
        gamma.ll<-gamma.fit$loglik
        #  test.gamma<-gofstat(gamma.fit)
        distfitHurdle.df[[i]][j-(start.col-1),8]<-round(gamma.ll,digits=0)
        distfitHurdle.df[[i]][j-(start.col-1),9]<-round(gamma.fit$estimate[1],digits=2)
        distfitHurdle.df[[i]][j-(start.col-1),10]<-round(gamma.fit$estimate[2],digits=2)
          
        ############# weibull ##############
        weibull.fit<-fitdist(cur.loads.vals0+add.val,distr="weibull")
        weibull.ll<-weibull.fit$loglik
        #   test.weibull<-gofstat(weibull.fit)
        distfitHurdle.df[[i]][j-(start.col-1),11]<-round(weibull.ll,digits = 0)
        distfitHurdle.df[[i]][j-(start.col-1),12]<-round(weibull.fit$estimate[1],digits=2)
        distfitHurdle.df[[i]][j-(start.col-1),13]<-round(weibull.fit$estimate[2],digits=2)
       }
    }
  }
  return(list(distfitW0.df,distfitHurdle.df))
}