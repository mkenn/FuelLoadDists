##########
# function to generate MC null
# distribution values for KS test
##########
KSMC.fn<-function(distr,param1,param2,niter,nsamp,obs.stat)
{
  library(fitdistrplus)
  KS.vals<-rep(NA,niter)
  for(i in 1:niter)
  {
    tmp.sim<-switch(distr,
                    norm=rnorm(nsamp,param1,param2),
                    lnorm=rlnorm(nsamp,param1,param2),
                    gamma=rgamma(nsamp,param1,param2),
                    weibull=rweibull(nsamp,param1,param2))
    tmp.fit<-switch(distr,
                    norm=fitdist(tmp.sim,distr="norm"),
                    lnorm=fitdist(tmp.sim,distr="lnorm"),
                    gamma=fitdist(tmp.sim,distr="gamma"),
                    weibull=fitdist(tmp.sim,distr="weibull"))
    tmp.ks<-switch(distr,
                   norm=ks.test(tmp.sim,"pnorm",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   lnorm=ks.test(tmp.sim,"plnorm",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   gamma=ks.test(tmp.sim,"pgamma",tmp.fit$estimate[1],tmp.fit$estimate[2]),
                   weibull=ks.test(tmp.sim,"pweibull",tmp.fit$estimate[1],tmp.fit$estimate[2]))
    KS.vals[i]<-tmp.ks$statistic
  }
  p.val<-1-length(KS.vals[obs.stat>KS.vals])/(niter+1) # since the KS test is only in the right-tail
  # we are only concerned if the observed statistic is too large
  # if(obs.stat>median(KS.vals))
  #   p.val<-2*length(KS.vals[KS.vals>=obs.stat])/(niter+1)
  # else
  #   p.val<-2*length(KS.vals[KS.vals<=obs.stat])/(niter+1)
  
  return(list(p.val,KS.vals))
}