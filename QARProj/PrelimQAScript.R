#############
# flag outliers and zero variability EVTs
#############
library(fitdistrplus)
###########
# 1. Read in the data
# this is the old data base
#data.file<-read.csv("../FuelLoadDists/Data/loadingsByEVTGroup_20170328.csv") #substitute filepath and name for local system
# this is the current database
file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))
sapply(file.sources,FUN="source")
data.file<-read.csv("../Data/loadingsByEVTGroup_20170928.csv") #substitute filepath and name for local system

start.col<-13

#########
# look for zero variability
EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd", write.file = TRUE)
# first see which EVTs have multiple entries
evts1<-EVTTallies[[1]][EVTTallies[[1]][,2]>=10,1]
new.corr<-corrpairs.fn(data.file = data.file,start.col = start.col,evts = evts1,evt.col = "LFEVTGroupCd")
# Can leverage the correlation function to identify those with zero variability
# for now have it print those evts and just copy it from the screen

# Now look for outliers
# Maybe based on histograms, look for a literal separation
# EVT615 has an example in snag loading
tmp.loads<-data.file[data.file$LFEVTGroupCd==615,17]
tmp.loads<-tmp.loads[!is.na(tmp.loads)]
tmp.loads<-tmp.loads[tmp.loads>0]
tmp.hist<-hist(tmp.loads)
length(tmp.hist$density[tmp.hist$density==0]) # so how many classes are zero? If more than 1, flag
# this is intended to test extreme outliers, or literal gaps in the data
neg.vals<-data.frame(EVT=rep(NA,nrow(data.file)),fuelType=NA)
out.vals<-data.frame(EVT=rep(NA,nrow(data.file)),fuelType=NA)
neg.id<-0
out.id<-0
histWithOuts<-list()
for(i in 1:length(evts1)) # so stick with only those with more than 10 study points
{
  tmp.df<-data.file[data.file$LFEVTGroupCd==evts1[i],12:ncol(data.file)]
  for(j in 1:ncol(tmp.df))
  {
    tmp.loads<-tmp.df[,j]
    tmp.loads<-tmp.loads[!is.na(tmp.loads)]
    if(length(tmp.loads)>10)
    {
      if(min(tmp.loads)<0)
      {
        neg.id<-neg.id+1
        neg.vals$EVT[neg.id]<-evts1[i]
        neg.vals$fuelType[neg.id]<-names(tmp.df)[j]
      }         #print(paste("Negative value in EVT",evts1[i],"load",names(tmp.df)[j]))
      tmp.loads<-tmp.loads[tmp.loads>0]
      if(length(tmp.loads)>10)
      {
        tmp.hist<-hist(tmp.loads)
        tmp.l<-length(tmp.hist$density[tmp.hist$density==0]) # so how many classes are zero? If more than 1, flag
        if(tmp.l>1)
        {
          out.id<-out.id+1
          out.vals$EVT[out.id]<-evts1[i]
          out.vals$fuelType[out.id]<-names(tmp.df)[j]
          histWithOuts[[out.id]]<-tmp.hist
          
        }
   #       print(paste("Possible extreme outlier",evts1[i],"load",names(tmp.df)[j]))
      }
    }
    
  }
}
out.vals<-out.vals[!is.na(out.vals[,1]),]
neg.vals<-out.vals[!is.na(neg.vals[,1]),]

## or the "outer fence" id'd by NIST, > 3*IQR
out3.vals<-data.frame(EVT=rep(NA,nrow(data.file)),fuelType=NA)
out.id<-0
histWithOuts2<-list()
for(i in 1:length(evts1)) # so stick with only those with more than 10 study points
{
  tmp.df<-data.file[data.file$LFEVTGroupCd==evts1[i],start.col:ncol(data.file)]
  for(j in 1:ncol(tmp.df))
  {
    tmp.loads<-tmp.df[,j]
    tmp.loads<-tmp.loads[!is.na(tmp.loads)]
    tmp.loads<-tmp.loads[tmp.loads>0]
    tmp.loads<-log(tmp.loads)
    if(length(tmp.loads)>10)
    {
      tmp.hist<-hist(tmp.loads)
      tmp.iq<-3*IQR(tmp.loads)
      tmp.qs<-quantile(tmp.loads,probs = c(0.25,0.75))
      if(min(tmp.loads)<(tmp.qs[1]-tmp.iq)|max(tmp.loads)>(tmp.qs[2]+tmp.iq))
      {
          out.id<-out.id+1
          out3.vals$EVT[out.id]<-evts1[i]
          out3.vals$fuelType[out.id]<-names(tmp.df)[j]
          histWithOuts2[[out.id]]<-tmp.hist
      }
        #       print(paste("Possible extreme outlier",evts1[i],"load",names(tmp.df)[j]))
    }
    
  }
}

out3.vals<-data.frame(EVT=rep(NA,nrow(data.file)),sourceID=NA,studyPoint=NA,fuelType=NA,data.val=NA)
out.id<-0
histWithOuts3<-list()
for(i in 1:length(evts1)) # so stick with only those with more than 10 study points
{
  tmp.df<-data.file[data.file$LFEVTGroupCd==evts1[i],start.col:ncol(data.file)]
  tmp1.source<-data.file[data.file$LFEVTGroupCd==evts1[i],c(3,5)]
  for(j in 1:ncol(tmp.df))
  {
    tmp.loads<-tmp.df[,j]
    tmp.source<-tmp1.source[!is.na(tmp.loads),]
    tmp.loads<-tmp.loads[!is.na(tmp.loads)]
    tmp.source<-tmp.source[tmp.loads>0,]
    tmp.loads<-tmp.loads[tmp.loads>0]
    tmp.loads<-log(tmp.loads)
    if(length(tmp.loads)>10)
    {
      tmp.hist<-hist(tmp.loads)
      tmp.iq<-3*IQR(tmp.loads)
      tmp.qs<-quantile(tmp.loads,probs = c(0.25,0.75))
      if(max(tmp.loads)>(tmp.qs[2]+tmp.iq))
      {
        out.id<-out.id+1
        out3.vals$EVT[out.id]<-evts1[i]
        out3.vals$fuelType[out.id]<-names(tmp.df)[j]
        histWithOuts3[[out.id]]<-tmp.hist
        out3.vals$sourceID[out.id]<-tmp.source[which(tmp.loads==max(tmp.loads))[1],1]
        out3.vals$studyPoint[out.id]<-tmp.source[which(tmp.loads==max(tmp.loads))[1],2]
        out3.vals$data.val[out.id]<-max(exp(tmp.loads))
      }
      #       print(paste("Possible extreme outlier",evts1[i],"load",names(tmp.df)[j]))
    }
    
  }
}

out4.vals<-data.frame(EVT=rep(NA,nrow(data.file)),sourceID=NA,studyPoint=NA,fuelType=NA,data.val=NA)
out.id<-0
histWithOuts4<-list()
for(i in 1:length(evts1)) # so stick with only those with more than 10 study points
{
  tmp.df<-data.file[data.file$LFEVTGroupCd==evts1[i],start.col:ncol(data.file)]
  tmp1.source<-data.file[data.file$LFEVTGroupCd==evts1[i],c(3,5)]
  for(j in 1:ncol(tmp.df))
  {
    tmp.loads<-tmp.df[,j]
    tmp.source<-tmp1.source[!is.na(tmp.loads),]
    tmp.loads<-tmp.loads[!is.na(tmp.loads)]
    tmp.source<-tmp.source[tmp.loads>0,]
    tmp.loads<-tmp.loads[tmp.loads>0]
    tmp.loads<-log(tmp.loads)
    if(length(tmp.loads)>10)
    {
      tmp.hist<-hist(tmp.loads)
      tmp.iq<-4*IQR(tmp.loads)
      tmp.qs<-quantile(tmp.loads,probs = c(0.25,0.75))
      if(min(tmp.loads)<(tmp.qs[1]-tmp.iq)|max(tmp.loads)>(tmp.qs[2]+tmp.iq))
      {
        out.id<-out.id+1
        out4.vals$EVT[out.id]<-evts1[i]
        out4.vals$fuelType[out.id]<-names(tmp.df)[j]
        histWithOuts4[[out.id]]<-tmp.hist
        if(max(tmp.loads)>(tmp.qs[2]+tmp.iq))
        {
          out4.vals$sourceID[out.id]<-tmp.source[which(tmp.loads==max(tmp.loads))[1],1]
          out4.vals$studyPoint[out.id]<-tmp.source[which(tmp.loads==max(tmp.loads))[1],2]
          out4.vals$data.val[out.id]<-max(exp(tmp.loads))
        }
      }
      #       print(paste("Possible extreme outlier",evts1[i],"load",names(tmp.df)[j]))
    }
    
  }
}
out.vals<-out.vals[!is.na(out.vals[,1]),]
out2.vals<-out2.vals[!is.na(out2.vals[,1]),]
out3.vals<-out3.vals[!is.na(out3.vals[,1]),]
out4.vals<-out4.vals[!is.na(out4.vals[,2]),]
neg.vals<-out.vals[!is.na(neg.vals[,1]),]
table(out4.vals$sourceID)

save(out.vals,out2.vals,neg.vals,file="PrelimQAOutliers.RData")
save(out3.vals,out4.vals,file="PrelimQAOutliersLog.RData")

# have a look at the out.vals
# one example, look at implications for fitting with and without outliers
# 630
# cwd_sound_loading..Mg.ha

tmp.loads<-data.file[data.file$LFEVTGroupCd==630,"cwd_sound_loading..Mg.ha"]
tmp.loads<-tmp.loads[!is.na(tmp.loads)]
tmp.loads<-tmp.loads[tmp.loads>0]
lnorm.fit<-fitdist(tmp.loads,distr="lnorm")
denscomp(lnorm.fit)
qqcomp(lnorm.fit)
cdfcomp(lnorm.fit)

tmp2.loads<-tmp.loads[tmp.loads<60]
lnorm.fit2<-fitdist(tmp2.loads,distr="lnorm")
denscomp(lnorm.fit2)
qqcomp(lnorm.fit2)
cdfcomp(lnorm.fit2)

boxplot(rlnorm(10000,0.95,1.25),rlnorm(10000,1.04,1.36),tmp.loads,tmp2.loads,
        names=c("fit(-)Out","fitWithOut","Obs","Obs(-)"))

library(lcmix)

