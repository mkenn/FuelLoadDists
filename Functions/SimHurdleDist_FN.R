#######
# Function to randomly sample from hurdle
# distribution, with a continuous non-zero distribution
#######

simHurdle.fn<-function(distr,prop0,nsamp,nrep,param1,param2)
{
  return.samp<-data.frame(matrix(NA,nrow=nsamp,ncol=nrep))
  for(i in 1:nrep)
  {
    tmp.n.zero<-rbinom(1,nsamp,prop0)
    tmp.n.gtZero<-nsamp-tmp.n.zero
    if(tmp.n.gtZero>0)
    tmp.hurdle<-switch(distr,
                       norm=rnorm(tmp.n.gtZero,param1,param2),
                       logNormal=rlnorm(tmp.n.gtZero,param1,param2),
                       gamma=rgamma(tmp.n.gtZero,param1,param2),
                       weibull=rweibull(tmp.n.gtZero,param1,param2))
    return.samp[1:tmp.n.zero,i]<-0
    return.samp[(tmp.n.zero+1):nsamp,i]<-tmp.hurdle
    
  }
  return.samp
}