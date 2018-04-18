##########
# function to generate MC null
# distribution values for KS test
##########
KSMC.fn<-function(distr,param1,param2,niter,nsamp,obs.stat)
{
  library(fitdistrplus)
  KS.vals<-rep(NA,niter) # to record the values in the null distribution
  for(i in 1:niter) # repeat niter times
  {
    tmp.sim<-switch(distr, # simulate a random sample of size nsamp from the indicated distribution. This should be the same size as that for which the distribution was estimated
                    norm=rnorm(nsamp,param1,param2),
                    lnorm=rlnorm(nsamp,param1,param2),
                    gamma=rgamma(nsamp,param1,param2),
                    weibull=rweibull(nsamp,param1,param2))
    tmp.fit<-switch(distr, # estimate the distribution from the random sample
                    norm=fitdist(tmp.sim,distr="norm"),
                    lnorm=fitdist(tmp.sim,distr="lnorm"),
                    gamma=fitdist(tmp.sim,distr="gamma"),
                    weibull=fitdist(tmp.sim,distr="weibull"))
    tmp.ks<-switch(distr, # calculate the value of the KS test statistic.
                   norm=ks.test(tmp.sim,"pnorm",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   lnorm=ks.test(tmp.sim,"plnorm",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   gamma=ks.test(tmp.sim,"pgamma",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   weibull=ks.test(tmp.sim,"pweibull",tmp.fit$estimate[1],tmp.fit$estimate[2]))
    KS.vals[i]<-tmp.ks$statistic
  }
  p.val<-1-length(KS.vals[obs.stat>KS.vals])/(niter+1) # since the KS test is only in the right-tail
  # we are only concerned if the observed statistic is too large

  return(list(p.val,KS.vals)) # return both the estimated p-value and the null distribution
}