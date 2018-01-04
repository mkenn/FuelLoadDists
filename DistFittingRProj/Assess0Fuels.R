#####
# describe which EVTs have zeroes, in which fuel categories, and
# in what proportion
#####

data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
start.col=13
evt.col = "LFEVTGroupCd_FINAL"

evts<-unique(data.file[,evt.col])

evts<-evt.vals

cur.cols<-start.col:ncol(data.file)

prop0<-list()

prop0.df<-data.frame(matrix(NA,ncol=length(cur.cols),nrow = length(evts)))
names(prop0.df)<-names(data.file)[cur.cols]
rownames(prop0.df)<-evts

for(i in 1:length(evts))
{
  prop0[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],N0=NA,Ntotal=NA,prop0=NA)
  for(j in cur.cols)
  {
    tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
    cur.loads<-tmp.loads[!is.na(tmp.loads)]
    
    if(length(cur.loads)>0)
    {
      prop0[[i]]$N0[j-(start.col-1)]<-length(cur.loads[cur.loads==0])
      prop0[[i]]$Ntotal[j-(start.col-1)]<-length(cur.loads)
      prop0[[i]]$prop0[j-(start.col-1)]<-prop0[[i]]$N0[j-(start.col-1)]/prop0[[i]]$Ntotal[j-(start.col-1)]
      if(prop0[[i]]$Ntotal[j-(start.col-1)]>30)
        prop0.df[i,j-(start.col-1)]<-round(prop0[[i]]$prop0[j-(start.col-1)],digits=3)
    }
  }
}

## summarize with spread sheet of EVTs, and categories with > 25%

write.csv(prop0.df,file="ProportionZeroEntriesNgt30.csv")
