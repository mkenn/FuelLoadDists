# create summaries for table

load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
source("../Functions/Dist_Fit_Graph_Pub_FN.R")

fuel.names<-read.csv("../ConsumeSARProj/FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv")

# evt group numbers
evt.ew<-c(655,682,666) # 
#evt.cf<-c(683,758,631) #peatland, black spruce woodland, pp
# or
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp

evt.names<-read.csv("../Data/EVT_AbbrevNames.csv",stringsAsFactors=FALSE,allowEscapes=TRUE)

fuels.plot<-c("tree_loading_Mgha","cwd_loading_Mgha","duff_loading_Mgha","litter_loading_Mgha")

summary.stats<-data.frame(evt.group=rep(c(evt.ew,evt.cf2),each=4),fuel=rep(fuels.plot,6),Q1=NA,Q2=NA,Q3=NA)

row.id<-0
for(i in 1:6)
{
  for(j in 1:4)
  {
    row.id<-row.id+1
    tmp.df<-data.file[data.file$LFEVTGroupCd_FINAL==summary.stats$evt.group[row.id],fuels.plot[j]]
    tmp.df<-tmp.df[!is.na(tmp.df)&tmp.df>0]
    summary.stats[row.id,3:5]<-round(quantile(tmp.df,probs = c(0.25,0.5,0.75)),digits=2)
  }
}

write.csv(summary.stats,file="QuartilesForPub.csv",row.names = FALSE)
