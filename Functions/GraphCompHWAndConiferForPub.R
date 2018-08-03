###########
# example groups for pub figures try 1
###########
# three EVT groups for each eastern woodland and conifer forest
# for each, 3-4 fuel loading types
# So either a 3x3 or 4x3 panel graph
###########

load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
source("../Functions/Dist_Fit_Graph_Pub_FN.R")

fuel.names<-read.csv("../ConsumeSARProj/FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv")

# evt group numbers
evt.ew<-c(655,682,666) # 
evt.cf<-c(683,758,631) #peatland, black spruce woodland, pp
# or
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp

evt.names<-read.csv("../Data/EVT_AbbrevNames.csv",stringsAsFactors=FALSE,allowEscapes=TRUE)

fuels.plot<-c("tree_loading_Mgha","cwd_loading_Mgha","duff_loading_Mgha","litter_loading_Mgha")

x.lims<-c(500,50,120,50)

fig.x1<-c(0,1/3,2/3)
fig.x2<-c(1/3,2/3,1)
fig.y1<-c(.72,.48,.24,0)
fig.y2<-c(1,.72,.48,.24)

postscript(file="CompHardwoodTry1.eps",horizontal = FALSE,height=12,width=10)
plot.new()
y.id<-0
x.id<-0
for(i in fuels.plot)
{
  y.id<-y.id+1
  for(j in evt.ew)
  {
    x.id<-x.id+1
    par()
    
    if(y.id==1)
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,3.75,2.5,0.5),mgp=c(2.25,0.5,0),las=1)
    else
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,3.75,0.5,0.5),mgp=c(2.25,0.5,0),las=1)
    tmp.id<-which(evt.vals==j)
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    cur.distr<-switch(distributionCustomRankingHurdleNOut[[tmp.id]]$dist1.fit[tmp.id2],
                      gammaLL="gamma",
                      lnormLL="lnorm",
                      normLL="norm",
                      weibullLL="weibull")
    if(y.id==1)
    {
      main.txt<-as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==j])
    }
    else
      main.txt=""
    distFitForPubGraph2.fn(data.file=data.file,evts=j,cur.cols = tmp.id2+start.col-1,evt.col = EVTCol,
                          distr=cur.distr,
                          plot.qqAxes = TRUE,x.lab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                          main.txt=main.txt,removeOut=TRUE,x.lims = x.lims[y.id],prop0=round(distributionCustomFittingHurdleNOut$HurdleFit[[tmp.id]]$prop0[tmp.id2]*100,digits=1))
    
  }
  x.id<-0
}
dev.off()

x2.lims<-c(600,150,275,50)

postscript(file="CompConiferTry1.eps",horizontal = FALSE,height=12,width=10)
plot.new()
y.id<-0
x.id<-0
for(i in fuels.plot)
{
  y.id<-y.id+1
  for(j in evt.cf2)
  {
    x.id<-x.id+1
    par()
    
    if(y.id==1)
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,3.75,2.5,0.5),mgp=c(2.25,0.5,0),las=1)
    else
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,3.75,0.5,0.5),mgp=c(2.25,0.5,0),las=1)
    tmp.id<-which(evt.vals==j)
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    cur.distr<-switch(distributionCustomRankingHurdleNOut[[tmp.id]]$dist1.fit[tmp.id2],
                      gammaLL="gamma",
                      lnormLL="lnorm",
                      normLL="norm",
                      weibullLL="weibull")
    if(y.id==1)
    {
      main.txt<-as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==j])
    }
    else
      main.txt=""
    distFitForPubGraph2.fn(data.file=data.file,evts=j,cur.cols = tmp.id2+start.col-1,evt.col = EVTCol,
                           distr=cur.distr,
                           plot.qqAxes = TRUE,x.lab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                           main.txt=main.txt,removeOut=TRUE,x.lims = x2.lims[y.id],prop0=round(distributionCustomFittingHurdleNOut$HurdleFit[[tmp.id]]$prop0[tmp.id2]*100,digits=1))
    
  }
  x.id<-0
}
dev.off()


