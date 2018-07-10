########
# a script to estimate hurdle models to 
# fuel loading distributions, particularly for those distributions 
# with zero-inflated data
# MCK
########

#data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
data.file<-read.csv("../Data/CurrentData/loadingsByEVTGroup_20180607.csv") #substitute filepath and name for local system
start.col=11 # the first fuel-loading column
EVTCol = "LFEVTGroupCd_FINAL"
# first read the functions into the current session
file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))

sapply(file.sources,FUN="source")

# tally number of observations by EVT, to find the subset for which we will perform
# distribution fitting

EVTTallies<-EVT.tally.fn(data.file, evt.col = EVTCol, write.file = TRUE,min.tally = 30)

# These are the EVTs that meet the minimum threshold
# of at least 30 total entries.
evt.vals<-EVTTallies$evt.min_tally
# Estimate the 4 candidate distributions for each evt/loading combination
# that meets the minimum data requirement
library(fitdistrplus) # required for distribution fitting

min.plot<-30

distributionCustomFittingHurdle <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                min.plot = min.plot, evt.col = EVTCol,add.val=0.1)


distributionFittingHurdle <- dist.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                        min.plot = min.plot, evt.col = EVTCol,add.val=0.1)
# Note: this object contains distfitW0.df and distfitHurdle.df. 
# distfitW0.df has all values including 0 in a single fitted distribution
# 
# One idea for assessment is to see if the bootstrap CI for each distribution contains the 
# empirical CDF. Not sure if the bootstrap CI would be wider with poorer fit.
# summarize the best-fitting (with max likelihood)
# among the four candidate distributions. Note, this is equivalent to AIC
# criterion because they all have the same number of parameters
distributionRankingWZero<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingHurdle$WithZeroFit,start.col=13)
distributionCustomRankingHurdle<-distfit.rank.fn(evts = evt.vals,
                                           DistFitSum = distributionCustomFittingHurdle$HurdleFit,start.col=13)

distributionRankingHurdle<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingHurdle$HurdleFit,start.col=13)




##Now, how to compare distribution fits between the two. Numerically a possibility.
##Can also just compare equivalence results between with 0's and without 0's
##Assuming prop0 an unbiased, efficient estimate of the proportion of zeroes

# first create a master list to contain the minimum ep value
# Note: getting strange results with ptte.data (the paired test)
# using tost instead (two-one-sided test for equivalence). The
# Epsilon value is the actual magnitude of error tolerance,
# the maximum allowable difference bewteen the observed cdf
# and the theoretical cdf.
# test on increments of 0.0001
library(equivalence)
equivalence.epValWZero.list<-equivalence.epValWOZero.list<-list()
cur.cols<-start.col:ncol(data.file)
for(i in 1:length(evt.vals))
{
  # summary dataframe
  equivalence.epValWZero.list[[i]]<-equivalence.epValWOZero.list[[i]]<-
    data.frame(fueltype=names(data.file)[cur.cols],normResult=NA,lnormResult=NA,
                                    gammaResult=NA,weibullResult=NA)  
  equivalence.epValWZero.list[[i]][!is.na(distributionFittingHurdle$WithZeroFit[[i]][,2]),2:5]<-1#flag fits with 1's, see if gets replaced.
  equivalence.epValWOZero.list[[i]][!is.na(distributionFittingHurdle$HurdleFit[[i]][,2]),2:5]<-1#flag fits with 1's, see if gets replaced.
}  

ep.val<-0.01
equivalenceWZero<-equivalence.fn(data.file,distFit.obj=distributionFittingHurdle$WithZeroFit,evts=evt.vals,
                                 evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                 min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                 ep.val=ep.val, include0=TRUE,add.val=0.1)

equivalenceWOZero<-equivalence.fn(data.file,distFit.obj=distributionFittingHurdle$HurdleFit,evts=evt.vals,
                                 evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                 min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                 ep.val=ep.val, include0=FALSE,add.val=0)

for(k in 1:length(equivalenceWZero))
{
  for(j in 1:nrow(equivalenceWZero[[k]]))
  {
    if(!is.na(equivalenceWZero[[k]][j,3]))
    {
      tmp.id<-which(equivalenceWZero[[k]][j,]=="rejected")
      if(length(tmp.id)>0)
      {
        for(m in tmp.id)
        {
          if(equivalence.epValWZero.list[[k]][j,m]==1) # if it's still at its initial value
            equivalence.epValWZero.list[[k]][j,m]<-ep.val #replace with this smaller value
        }
      }
    }
    if(!is.na(equivalenceWOZero[[k]][j,3]))
    {
      tmp.id<-which(equivalenceWOZero[[k]][j,]=="rejected")
      if(length(tmp.id)>0)
      {
        for(m in tmp.id)
        {
          if(equivalence.epValWOZero.list[[k]][j,m]==1)# if it's still at its initial value
            equivalence.epValWOZero.list[[k]][j,m]<-ep.val#replace with the smaller value
        }
      }
    }
  }
}


# Loop through ep.vals to 0.04, find for each with and without zeroes the lowest ep.val
# at which the equivalence test is rejected
# There is a power issue here, where the hurdle model will have a much
# lower sample size for the equivalence test (thereby lower power)
ep.vals<-seq(0.02,0.10,0.01)
for(i in 1:length(ep.vals))
{
  tmp.ep.val<-ep.vals[i]
  tmp.equivalenceWZero<-equivalence.fn(data.file,distFit.obj=distributionFittingHurdle$WithZeroFit,evts=evt.vals,
                                       evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                       min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                       ep.val=tmp.ep.val, include0=TRUE,add.val=0.1)
  tmp.equivalenceWOZero<-equivalence.fn(data.file,distFit.obj=distributionFittingHurdle$HurdleFit,evts=evt.vals,
                                       evt.col=EVTCol,start.col=start.col,cur.cols = start.col:ncol(data.file),
                                       min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                       ep.val=tmp.ep.val, include0=FALSE,add.val=0)
  
  for(k in 1:length(tmp.equivalenceWZero))
  {
    for(j in 1:nrow(tmp.equivalenceWZero[[k]]))
    {
      if(!is.na(tmp.equivalenceWZero[[k]][j,3]))
      {
        tmp.id<-which(tmp.equivalenceWZero[[k]][j,]=="rejected")
        if(length(tmp.id)>0)
        {
          for(m in tmp.id)
          {
            if(equivalence.epValWZero.list[[k]][j,m]==1) # if it's still at its initial value
              equivalence.epValWZero.list[[k]][j,m]<-tmp.ep.val # replace with the smaller value
          }
        }
      }
      if(!is.na(tmp.equivalenceWOZero[[k]][j,3]))
      {
        tmp.id<-which(tmp.equivalenceWOZero[[k]][j,]=="rejected")
        if(length(tmp.id)>0)
        {
          for(m in tmp.id)
          {
            if(equivalence.epValWOZero.list[[k]][j,m]==1)# if it's still at its initial value
              equivalence.epValWOZero.list[[k]][j,m]<-tmp.ep.val# replace with the smaller value
          }
        }
      }
    }
  }
}

### We seem to be getting some contradictory results for ptte.data, where the "best fit" by likelihood is not necessarily the
### the distribution with the lowest epsilon in the equivalence test. 
### Should also check the sample sizes to see if the non-zeroes are small samples.

## Looks like this might be a power issue. The better fitting distributions have a smaller mean (= not dissimilar)
## but also a smaller standard deviation, such that mean/stdErr is LARGE = dissimilar.
## Tempted to standardize the standard deviations (maybe a bootstrap?) for
## comparisons. 
## This seems to be solved with the simpler tost approach, which is basically finding a CI width around zero that
## that contains both sides of a CI around the mean difference between empirical and theoretical cdf


# first evaluate with an EVT with many observations, then an EVT with close to minimum observations

cur.evt<-697
cur.id<-70

tmp.df<-data.file[data.file[,EVTCol]==cur.evt,]
tmp.eqWOZero<-equivalence.epValWOZero.list[[cur.id]]
tmp.eqWZero<-equivalence.epValWZero.list[[cur.id]]
tmp.fitWOZero<-distributionFittingHurdle[[2]][[cur.id]]
tmp.fitWZero<-distributionFittingHurdle[[1]][[cur.id]]
tmp.rankWOZero<-distributionRankingHurdle[[cur.id]]
tmp.rankWZero<-distributionRankingWZero[[cur.id]]

cur.col<-start.col+29-1 # the row number in the above objects, + the starting column for the loadings

distFitGraph.fn(data.file=data.file,evts=cur.evt,cur.cols = cur.col,evt.col = EVTCol,distr="norm")
distFitGraph.fn(data.file=data.file,evts=cur.evt,cur.cols = cur.col,evt.col = EVTCol,distr="logNormal")
distFitGraph.fn(data.file=data.file,evts=cur.evt,cur.cols = cur.col,evt.col = EVTCol,distr="gamma")
distFitGraph.fn(data.file=data.file,evts=cur.evt,cur.cols = cur.col,evt.col = EVTCol,distr="weibull")

# The likelihood rankings make sense, which means something is going wrong in the equivalence test.

# here is an example equivalance test
j<-29 # current column for fuel load variable
i<-70
tmp.loads<-data.file[data.file[,EVTCol]==cur.evt,cur.cols[j]]
cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
distFit.obj<-tmp.fitWOZero
norm.cdf<-pnorm(cur.loads.vals,mean=tmp.fitWOZero$norm.mu[j], # not sure why this wasn't giving out of bounds errors
                sd=tmp.fitWOZero$norm.sigma[j])
lnorm.cdf<-plnorm(cur.loads.vals,meanlog = tmp.fitWOZero$lnorm.mu[j],
                  sdlog = tmp.fitWOZero$lnorm.sigma[j])
gamma.cdf<-pgamma(cur.loads.vals,shape = tmp.fitWOZero$gamma.shape[j],
                  rate = tmp.fitWOZero$gamma.rate[j])
weibull.cdf<-pweibull(cur.loads.vals,shape = tmp.fitWOZero$weibull.shape[j],
                      scale = tmp.fitWOZero$weibull.scale[j])
cur.ecdf<-ecdf(cur.loads.vals)

tmp.diff<-norm.cdf-cur.ecdf(cur.loads.vals)

ptte.data(tmp.diff)

t.test(tmp.diff)


tmp.loads<-data.file[data.file[,EVTCol]==cur.evt,cur.cols[j]]
cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
distFit.obj<-tmp.fitWOZero
norm.cdf<-pnorm(cur.loads.vals,mean=tmp.fitWOZero$norm.mu[j], # not sure why this wasn't giving out of bounds errors
                sd=tmp.fitWOZero$norm.sigma[j])
lnorm.cdf<-plnorm(cur.loads.vals,meanlog = tmp.fitWOZero$lnorm.mu[j],
                  sdlog = tmp.fitWOZero$lnorm.sigma[j])
gamma.cdf<-pgamma(cur.loads.vals,shape = tmp.fitWOZero$gamma.shape[j],
                  rate = tmp.fitWOZero$gamma.rate[j])
weibull.cdf<-pweibull(cur.loads.vals,shape = tmp.fitWOZero$weibull.shape[j],
                      scale = tmp.fitWOZero$weibull.scale[j])
cur.ecdf<-ecdf(cur.loads.vals)

tmp.diff<-norm.cdf-cur.ecdf(cur.loads.vals)

ptte.data(tmp.diff)

t.test(tmp.diff)



cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0]+0.1
distFit.obj<-tmp.fitWZero
norm.cdf<-pnorm(cur.loads.vals,mean=tmp.fitWOZero$norm.mu[j], # not sure why this wasn't giving out of bounds errors
                sd=tmp.fitWOZero$norm.sigma[j])
lnorm.cdf<-plnorm(cur.loads.vals,meanlog = tmp.fitWOZero$lnorm.mu[j],
                  sdlog = tmp.fitWOZero$lnorm.sigma[j])
gamma.cdf<-pgamma(cur.loads.vals,shape = tmp.fitWOZero$gamma.shape[j],
                  rate = tmp.fitWOZero$gamma.rate[j])
weibull.cdf<-pweibull(cur.loads.vals,shape = tmp.fitWOZero$weibull.shape[j],
                      scale = tmp.fitWOZero$weibull.scale[j])
cur.ecdf<-ecdf(cur.loads.vals)

tmp.diff<-norm.cdf-cur.ecdf(cur.loads.vals)
tmp.diff<-gamma.cdf-cur.ecdf(cur.loads.vals)

ptte.data(tmp.diff,Epsilon=0.7)

t.test(tmp.diff)


#######
# So now we compare with and without 0 uncertainty, relative to the proportion of zeroes in the fuel loading category
#######
# I think organize by fuel loading as a list
# for each fuel loading, give a data frame with 1 row for each EVT,
# The minimum epsilon (Which would just be the max of the lower/upper bound 95% CI on the paired differences),
# for both with and without zeroes
# and the proportion of zeroes in that category for that EVT
# We are wondering if with zero does better when prop0 is low
# to get an idea for a lower threshold on prop0 for the hurdle model
fuel.ep.list<-list()

for(j in 1:30)
{
  fuel.ep.list[[j]]<-data.frame(evt=evt.vals,epWZero=NA,epWOZero=NA,prop0=NA)
  for(k in 1:length(equivalence.epValWZero.list))
  {
    if(!is.na(equivalence.epValWZero.list[[k]][j,2]))
      fuel.ep.list[[j]]$epWZero[k]<-min(equivalence.epValWZero.list[[k]][j,2:5])
    if(!is.na(equivalence.epValWOZero.list[[k]][j,2]))
      fuel.ep.list[[j]]$epWOZero[k]<-min(equivalence.epValWOZero.list[[k]][j,2:5])
    fuel.ep.list[[j]]$prop0[k]<-distributionFittingHurdle$HurdleFit[[k]]$prop0[j]
  }
}

## The goal here is to find a threshold for proportion of zeroes at which
# there is evidence we see separation between including the zeroes and fitting
# the hurdle model. 
# Subjectively, 0.1 seems to work as a differentiator, so if there is > 10% zero entries
# use the hurdle model.
# With one exception of herb to consider, where it does't look like the hurdle model does better
# regardless of prop0.

pdf(file="EpValProp0GraphsConstrictedYLimFixed.pdf")
par(mfrow=c(2,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0))
for(j in 1:30)
{
  if(length(!is.na(fuel.ep.list[[j]][,2]))>0)
  { 
    plot(fuel.ep.list[[j]]$prop0,fuel.ep.list[[j]]$epWZero,pch=16,col="purple",xlab="Proportion of zero entries",
         ylab="Distribution error",main=distributionFittingHurdle[[2]][[1]]$fueltype[j],xlim=c(0,1),ylim=c(0,0.1))
    points(fuel.ep.list[[j]]$prop0,fuel.ep.list[[j]]$epWOZero)
#    abline(v=c(0.1,0.15))
    abline(v=0.1)
    legend(x="right",legend=c("Fit With Zero","Hurdle Fit"),pch=c(16,1),col=c("purple","black"))
  }
}
dev.off()

