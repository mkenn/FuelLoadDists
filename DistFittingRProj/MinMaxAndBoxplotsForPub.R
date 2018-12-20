##########
# boxplots of PM 2.5 emissions
# No outliers indicated (just whiskers to min/max)
##########

load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
source("../Functions/Dist_Fit_Graph_Pub_FN2.R")

fuel.names<-read.csv("../ConsumeSARProj/FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv")


# evt group numbers
evt.ew<-c(655,682,666) # 
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp

evt.names<-read.csv("../Data/EVT_AbbrevNames.csv",stringsAsFactors=FALSE,allowEscapes=TRUE)
names.ew<-c(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.ew[1]]),
                         as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.ew[2]]),
                                      as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.ew[3]]))

names.cf<-c(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.cf2[1]]),
            as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.cf2[2]]),
            as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.cf2[3]]))


evt.all<-c(evt.ew,evt.cf2)

fuel.vals.minMax<-data.frame(EVT=c(evt.ew,evt.cf2),minCWD=NA,maxCWD=NA,minduff=NA,maxduff=NA,minlitter=NA,maxlitter=NA)

fuels.pm25<-c("cwd_loading_Mgha","duff_loading_Mgha","litter_loading_Mgha")
evt.names<-read.csv("../Data/EVT_AbbrevNames.csv",stringsAsFactors=FALSE,allowEscapes=TRUE)


for(i in 1:6)
{
  for(j in 1:3)
  {
    tmp.df<-data.file[data.file$LFEVTGroupCd_FINAL==evt.all[i],fuels.pm25[j]]
    tmp.df<-tmp.df[!is.na(tmp.df)&tmp.df>0]
    # remove outliers
    tmp.df<-tmp.df[tmp.df<(quantile(tmp.df,probs = 0.75)+4*IQR(tmp.df))]
    
    fuel.vals.minMax[i,j*2]<-round(min(tmp.df),digits=3)
    fuel.vals.minMax[i,j*2+1]<-round(max(tmp.df),digits=3)
  }
}

pm25All<-list()

pm25.duff<-scan("DuffPM25Vals.txt")

for(i in 1:3)
{
  pm25All[[i]]<-data.frame(min=rep(NA,8),Q1=NA,Q2=NA,Q3=NA,max=NA,fb=NA,FLM31=NA)
  
}

##########
# create boxplot from pm25 summaries
##########

#pm25All.df<-read.csv("PM2.5KgHaForPub.csv")
pm25All.df<-read.csv("CorrectedPM2.5.csv")
postscript(file="CompBothPM25WithMappedFixLabel.eps",horizontal = FALSE,height=7,width=12)
par(mar=c(3.5,4.25,1.5,0.1),mfrow=c(2,3),las=1,mgp=c(2.75,0.5,0))

for(j in c("CWD","Duff","Litter"))
{
  plot.new()
  plot.window(xlim=c(0.5,3.5),ylim=c(-0.5,max(pm25All.df[1:(3*7),j])))
  

for(i in 1:3)
{
  rect(i-0.45,pm25All.df[i*7-6,j],i+0.45,pm25All.df[i*7-4,j]) # IQR box
  segments(i-0.45,pm25All.df[i*7-5,j],i+0.45,pm25All.df[i*7-5,j]) # line at Q2
  segments(i,pm25All.df[i*7-6,j],i,pm25All.df[i*7-3,j]) # lower whisker
  segments(i-0.2,pm25All.df[i*7-3,j],i+0.2,pm25All.df[i*7-3,j]) # complete lower whisker
  segments(i,pm25All.df[i*7-4,j],i,pm25All.df[i*7-2,j]) # upper whisker
  segments(i-0.2,pm25All.df[i*7-2,j],i+0.2,pm25All.df[i*7-2,j]) # complete upper whisker
  points(i,pm25All.df[i*7-1,j],pch=16,col="orange",cex=1.5) # FB
  points(i,pm25All.df[i*7,j],pch=17,col="green",cex=1.3) # FLM
}
axis(2)
mtext(side=3,text=j)
#if(i==1)
  mtext(side=2,text=expression(paste("Predicted PM 2.5 (kg h",a^-1,")")),las=0,line=2.75,cex=0.75)

axis(1,at=1:3,labels=evt.ew,tick=FALSE,line=0.5) 

#axis(1,at=1:3,labels=names.ew,tick=FALSE,line=0.5) 
box()
}
#dev.off()

# mixed conifer
#postscript(file="CompConiferPM25WithMapped.eps",horizontal = FALSE,height=12,width=10)
#par(mar=c(3.5,4.25,1.5,0.1),mfrow=c(3,1),las=1,mgp=c(2.75,0.5,0))

for(j in c("CWD","Duff","Litter"))
{
  plot.new()
  plot.window(xlim=c(3.5,6.5),ylim=c(-0.5,max(pm25All.df[(3*7+1):(6*7),j])))
  
  
  for(i in 4:6)
  {
    rect(i-0.45,pm25All.df[i*7-6,j],i+0.45,pm25All.df[i*7-4,j]) # IQR box
    segments(i-0.45,pm25All.df[i*7-5,j],i+0.45,pm25All.df[i*7-5,j]) # line at Q2
    segments(i,pm25All.df[i*7-6,j],i,pm25All.df[i*7-3,j]) # lower whisker
    segments(i-0.2,pm25All.df[i*7-3,j],i+0.2,pm25All.df[i*7-3,j]) # complete lower whisker
    segments(i,pm25All.df[i*7-4,j],i,pm25All.df[i*7-2,j]) # upper whisker
    segments(i-0.2,pm25All.df[i*7-2,j],i+0.2,pm25All.df[i*7-2,j]) # complete upper whisker
    points(i,pm25All.df[i*7-1,j],pch=16,col="orange") # FB
    points(i,pm25All.df[i*7,j],pch=17,col="green") # FLM
  }
  axis(2)
  mtext(side=3,text=j) # take out for repetitive titles
  #if(i==4)
    mtext(side=2,text=expression(paste("Predicted PM 2.5 (kg h",a^-1,")")),las=0,line=2.75,cex=0.75)
  
#  cur.labs<-c()
  
#  axis(1,at=4:6,labels=names.cf,tick=FALSE,line=0.5) 
  axis(1,at=4:6,labels=evt.cf2,tick=FALSE,line=0.5) 
  box()
}
dev.off()
################
################
for(i in 1:3)
{
  rect(i-0.3,pm25.duff[i*7-6],i+0.3,pm25.duff[i*7-4])
  segments(i-0.3,pm25.duff[i*7-5],i+0.3,pm25.duff[i*7-5])
  segments(i,pm25.duff[i*7-6],i,pm25.duff[i*7-3])
  segments(i-0.25,pm25.duff[i*7-4],i+0.25,pm25.duff[i*7-4])
  segments(i,pm25.duff[i*7-4],i,pm25.duff[i*7-2])
  segments(i-0.2,pm25.duff[i*7-2],i+0.2,pm25.duff[i*7-2])
  points(i,pm25.duff[i*7-1],pch=18,col="orange")
  points(i,pm25.duff[i*7],pch=18,col="blue")
}
axis(2)
axis(1,at=1:3,labels=c("Peatland","Ponderosa","Mixed Conifer"))            

###
# Example from 1 EVT group
###
postscript(file="PMBox1Example.eps",height=5,width=3.5,
           horizontal=FALSE)
plot.new()
par(mar=c(3.5,3.75,0.1,0.1),las=1,mgp=c(2.75,0.5,0))
plot.window(xlim=c(0.5,1.5),ylim=c(0,2500))
i<-2
rect(0.7,pm25.duff[i*7-6],1+0.3,pm25.duff[i*7-4])
segments(0.7,pm25.duff[i*7-5],1+0.3,pm25.duff[i*7-5])
segments(1,pm25.duff[i*7-6],1,pm25.duff[i*7-3])
segments(0.75,pm25.duff[i*7-4],1+0.25,pm25.duff[i*7-4])
segments(1,pm25.duff[i*7-4],1,pm25.duff[i*7-2])
segments(0.8,pm25.duff[i*7-2],1+0.2,pm25.duff[i*7-2])
points(1,pm25.duff[i*7-1],col="orange",pch=18,cex=1.5)
points(1,pm25.duff[i*7],pch=18,col="blue",cex=1.5)
axis(2)
axis(1,at=1,labels="Ponderosa")
box()
mtext(side=2,text="Predicted PM 2.5 (units?)",las=0,line=2.75)
dev.off()



