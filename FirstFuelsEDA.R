############
# eda for fitting multivariate fuels distributions
############
library(mvnmle)
evt1.df<-read.csv("../EVTDataFiles/EVT642Query.csv")
evt.loads.df<-evt1.df[,3:32]
sort1<-mysort(evt1.df[,3:15])  # internal function, not very useful
est1<-mlest(evt1.df[,3:32])
boxplot(evt1.df[,3:32])

#####
# tally the number of observations for each column in this evt group
n.obs<-rep(NA,30)
for(i in 1:30)
{
  n.obs[i]<-length(evt1.df[!is.na(evt1.df[,i+2]),i+2])
}

n2.obs<-rep(NA,30)
for(i in 1:30)
{
  n2.obs[i]<-length(which(!is.na(evt1.df[,i+2])))
}

######
# exclude columns with zero observations from analysis
evt.loadsAll.df<-evt.loads.df[,which(n.obs>800)]
est1<-mlest(evt.loadsAll.df)


#############
# now try all study points, with and without EVT
############
allpts.df<-read.csv("../EVTDataFiles/StdyPtAllLoadings.csv")
allpts.loads.df<-allpts.df[,2:31]
nAll.obs<-rep(NA,30)
for(i in 1:30)
{
  nAll.obs[i]<-length(which(!is.na(allpts.loads.df[,i])))
}
names(nAll.obs)<-names(allpts.loads.df)
nAll.obs/dim(allpts.loads.df)[1]
