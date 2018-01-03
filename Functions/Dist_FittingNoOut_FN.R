# Distribution Fitting Function
# This function will determine the loglikelihood of the distribution for each fuel loading type
# The function looks at four distributions: normal, lognormal, gamma, weibull
# The results are then entered into a dataframe that can compare likelihoods
# the minimum plot was arbitrarily selected

dist.fitNoOut.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,write.file=FALSE,file.name="DistFitSummaryEVT",out=3)
{
  distfit.df<-list()
  cur.cols=c(start.col:ncol(data.file))
  for(i in 1:length(evts))
  {
    # summary dataframe
    print(paste("starting evt",evts[i]))
    distfit.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.mu=NA,norm.sigma=NA,lgnormLL=NA,lnorm.mu=NA,lnorm.sigma=NA,gammaLL=NA,gamma.shape=NA,gamma.rate=NA,weibullLL=NA,weibull.shape=NA,weibull.scale=NA)
    
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
      cur.loads.ln<-log(cur.loads.vals)
      cur.iq<-IQR(cur.loads.ln)
      cur.quant<-quantile(cur.loads.ln,probs = c(0.25,0.75))
      cur.loads.vals.noOut<-cur.loads.vals[(cur.loads.ln>(cur.quant[2]+out*cur.iq))&(cur.loads.ln<(cur.quant[1]-out*cur.iq))]
      
      if(length(cur.loads.vals.noOut)>min.plot)
      {
        ############# normal ##############
        normal.fit<-fitdist(cur.loads.vals.noOut,distr="norm")
        normal.ll<-normal.fit$loglik
        #  test.normal<-gofstat(normal.fit)
        distfit.df[[i]][j-(start.col-1),2]<-round(normal.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),3]<-round(normal.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),4]<-round(normal.fit$estimate[2],digits=2)
 
        ############# lognormal ##############
        lnorm.fit<-fitdist(cur.loads.vals.noOut,distr="lnorm")
        lnorm.ll<-lnorm.fit$loglik
        #  test.lnorm<-gofstat(lnorm.fit)
        distfit.df[[i]][j-(start.col-1),5]<-round(lnorm.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),6]<-round(lnorm.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),7]<-round(lnorm.fit$estimate[2],digits=2)
       
        ############# gamma ##############
        gamma.fit<-fitdist(cur.loads.vals.noOut,distr="gamma")
        gamma.ll<-gamma.fit$loglik
        #  test.gamma<-gofstat(gamma.fit)
        distfit.df[[i]][j-(start.col-1),8]<-round(gamma.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),9]<-round(gamma.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),10]<-round(gamma.fit$estimate[2],digits=2)

        ############# weibull ##############
        weibull.fit<-fitdist(cur.loads.vals.noOut,distr="weibull")
        weibull.ll<-weibull.fit$loglik
        distfit.df[[i]][j-(start.col-1),11]<-round(weibull.ll,digits = 0)
        distfit.df[[i]][j-(start.col-1),12]<-round(weibull.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),13]<-round(weibull.fit$estimate[2],digits=2)
        
        if(write.file)
        {
          write.csv(distfit.df[[i]],file=paste(file.name,evts[i],".csv",sep = ""),row.names=FALSE)
        }
      }
    }
  }
  return(distfit.df)
}

