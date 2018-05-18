#########
# validation EDA
#########
# double check sage step tree_duff_depth=duff_depth

srs.df<-read.csv("../Data/CurrentData/SavannahRiverSite_Database.csv")
sage.df<-read.csv("../Data/CurrentData/SageSTEP_database.csv")
data.df<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv")
srs.evgroupCol<-"EVT_GP"
sage.evgroupCol<-"EVG_GP"
## Need map between these and the main database column names

# comparisons: boxplots by evg and fuel types; CI in database, mean value of obs by evg inside?; mean by mean plot


sage.evts<-unique(sage.df[,sage.evgroupCol])
srs.evts<-unique(srs.df[,srs.evgroupCol])

sage.start.col<-12
srs.start.col<-12

sage.fuelTypes<-names(sage.df)[sage.start.col:ncol(sage.df)]
srs.fuelTypes<-names(srs.df)[srs.start.col:ncol(srs.df)]

sage.data.cols<-match(sage.fuelTypes,names(data.df))
srs.data.cols<-match(srs.fuelTypes,names(data.df))

# names(data.df)
# names(sage.df)

# Tally!
sage.data.tally<-matrix(NA,ncol=length(sage.fuelTypes),nrow=length(sage.evts))
srs.data.tally<-matrix(NA,ncol=length(srs.fuelTypes),nrow=length(srs.evts))
sage.data.prop<-matrix(NA,ncol=length(sage.fuelTypes),nrow=length(sage.evts))
srs.data.prop<-matrix(NA,ncol=length(srs.fuelTypes),nrow=length(srs.evts))
for(i in 1:length(sage.evts))
{
  for(j in 1:length(sage.fuelTypes))
  {
    tmp.df<-data.df[data.df$LFEVTGroupCd_FINAL==sage.evts[i],sage.data.cols[j]]
    tmp.df<-tmp.df[!is.na(tmp.df)]
    
    sage.data.tally[i,j]<-length(tmp.df[!is.na(tmp.df)])
    if(length(tmp.df[!is.na(tmp.df)])>30)
    {
      q.vals<-quantile(tmp.df,probs = c(0.025,0.975))
      cur.obs<-sage.df[sage.df[,sage.evgroupCol]==sage.evts[i],sage.fuelTypes[j]]
      cur.obs<-cur.obs[!is.na(cur.obs)]
      if(length(cur.obs>0))
      {
        cur.tally<-0
        for(k in 1:length(cur.obs))
        {
          if(cur.obs[k]<=q.vals[2]&cur.obs[k]>=q.vals[1])
            cur.tally<-cur.tally+1
        }
        sage.data.prop[i,j]<-cur.tally/length(cur.obs)
      }
    }
    
  }
}
sage.data.tally<-data.frame(sage.data.tally)
row.names(sage.data.tally)<-sage.evts
names(sage.data.tally)<-sage.fuelTypes
sage.data.prop<-data.frame(sage.data.prop)
row.names(sage.data.prop)<-sage.evts
names(sage.data.prop)<-sage.fuelTypes

for(i in 1:length(srs.evts))
{
  for(j in 1:length(srs.fuelTypes))
  {
    tmp.df<-data.df[data.df$LFEVTGroupCd_FINAL==srs.evts[i],srs.data.cols[j]]
    tmp.df<-tmp.df[!is.na(tmp.df)]
    
    srs.data.tally[i,j]<-length(tmp.df[!is.na(tmp.df)])
    if(length(tmp.df[!is.na(tmp.df)])>30)
    {
      q.vals<-quantile(tmp.df,probs = c(0.025,0.975))
      cur.obs<-srs.df[srs.df[,srs.evgroupCol]==srs.evts[i],srs.fuelTypes[j]]
      cur.obs<-cur.obs[!is.na(cur.obs)]
      if(length(cur.obs>0))
      {
        cur.tally<-0
        for(k in 1:length(cur.obs))
        {
          if(cur.obs[k]<=q.vals[2]&cur.obs[k]>=q.vals[1])
            cur.tally<-cur.tally+1
        }
        srs.data.prop[i,j]<-cur.tally/length(cur.obs)
      }
    }    
  }
}
srs.data.tally<-data.frame(srs.data.tally)
row.names(srs.data.tally)<-srs.evts
names(srs.data.tally)<-srs.fuelTypes
srs.data.prop<-data.frame(srs.data.prop)
row.names(srs.data.prop)<-srs.evts
names(srs.data.prop)<-srs.fuelTypes

test.srs.id<-which(srs.data.tally>10,arr.ind = TRUE)
test.sage.id<-which(sage.data.tally>10,arr.ind = TRUE)

########
# evaluate whether the observed data is within 0.05, 0.95 quantiles of database empirical distribution
# Also compare to estimated distribution fit?
########
# draw a histogram for each evt/fuel type, then lines where the observed data are
pdf(file="ValidationExploratory1_SRS.pdf")
par(mfrow=c(3,3),mar=c(2.5,3.5,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
for(i in 1:nrow(test.srs.id))
{
  cur.base<-data.df[data.df$LFEVTGroupCd_FINAL==srs.evts[test.srs.id[i,1]],srs.fuelTypes[test.srs.id[i,2]]] 
  cur.obs<-srs.df[srs.df[,srs.evgroupCol]==srs.evts[test.srs.id[i,1]],srs.fuelTypes[test.srs.id[i,2]]]
  boxplot(cur.base,cur.obs,names=c("Database","Observed"),ylab=srs.fuelTypes[test.srs.id[i,2]],main=paste("EVT",srs.evts[test.srs.id[i,1]]))
}
dev.off()

pdf(file="ValidationExploratory1_SAGE.pdf")
par(mfrow=c(3,3),mar=c(2.5,3.5,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
for(i in 1:nrow(test.sage.id))
{
  cur.base<-data.df[data.df$LFEVTGroupCd_FINAL==sage.evts[test.sage.id[i,1]],sage.fuelTypes[test.sage.id[i,2]]] 
  cur.obs<-sage.df[sage.df[,sage.evgroupCol]==sage.evts[test.sage.id[i,1]],sage.fuelTypes[test.sage.id[i,2]]]
  boxplot(cur.base,cur.obs,names=c("Database","Observed"),ylab=sage.fuelTypes[test.sage.id[i,2]],main=paste("EVT",sage.evts[test.sage.id[i,1]]))
}
dev.off()

### 
# notes:
# negative loadings in Sage data set!
# Seems to be a mixed bag--often the database is shifted lower than
# the observed values, but often they seem to overlap nicely
# can do a BS p-value for the distribution comparison
# comparing the estimated distribution from the database
# to the observed distribution
######
# Rationale--the empirical hurdle distribution fits are our hypothesis for the distribution
# We assume (for better or worse) that the empirical data are a independent random sample
# from that distribution, under the null hypothesis.
######
# For now, part of the eda, look at proportion for each evt and fuel type, that observed fall
# within 95% middle quantiles of database
# also have to deal with zeroes!
#######


