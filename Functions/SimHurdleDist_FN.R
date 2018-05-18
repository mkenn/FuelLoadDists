#######
# Function to randomly sample from hurdle
# distribution, with a continuous non-zero distribution
#######

simHurdle.fn<-function(distr,prop0,nsamp,nrep,param1,param2,cur.gt0,upper.quantile=NA)
{
  return.samp<-data.frame(matrix(NA,nrow=nsamp,ncol=nrep))
  for(i in 1:nrep)
  {
    tmp.n.zero<-rbinom(1,nsamp,prop0)
    tmp.n.gtZero<-nsamp-tmp.n.zero
    if(tmp.n.gtZero>0)
    {
      if(!is.na(param2))
      {
        if(!is.na(upper.quantile)) # if we're truncating the random draws
        {
          upper.samp<-ceiling(tmp.n.gtZero/0.95)
          first.hurdle<-switch(distr,
                             norm=rnorm(upper.samp,param1,param2),
                             lnorm=rlnorm(upper.samp,param1,param2),
                             gamma=rgamma(upper.samp,param1,param2),
                             weibull=rweibull(upper.samp,param1,param2))
          tmp.hurdle<-first.hurdle[first.hurdle<quantile(first.hurdle,probs = upper.quantile)]
          tmp.hurdle<-tmp.hurdle[1:tmp.n.gtZero]
          
        }
        else
         tmp.hurdle<-switch(distr,
                         norm=rnorm(tmp.n.gtZero,param1,param2),
                         lnorm=rlnorm(tmp.n.gtZero,param1,param2),
                         gamma=rgamma(tmp.n.gtZero,param1,param2),
                         weibull=rweibull(tmp.n.gtZero,param1,param2))
       
      }
      else
      {
        tmp.hurdle<-sample(cur.gt0,tmp.n.gtZero,replace = TRUE) # just sample from the non-zero portion of the distribution
      }
    }
    return.samp[1:tmp.n.zero,i]<-0
    return.samp[(tmp.n.zero+1):nsamp,i]<-tmp.hurdle
    
  }
  return.samp
}