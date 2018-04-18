#########
# loop to graph each fuel type for each EVT group,
# that meets the minimum sample size requirement
#########
#load("Workspaces/HurdleCustomFitsWithNormWeib.RData") # estimated distributions
#load("Workspaces/HurdleCustomFits.RData") # estimated distributions
#load("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
# note: distributionCustomRankingHurdleAll contains all distributions in the ranking
# distributionCustomRankingHurdle includes only lognormal and gamma in the ranking
# Need to define pub-ready EVT titles and fuel labels
load("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
source("../Functions/Dist_Fit_Graph_Pub_FN.R")

##########
fuel.labels<-c(expression(paste("100-hr (Mg h",a^-1,")")),
               expression(paste("10-hr (Mg h",a^-1,")")),
                expression(paste("10K-hr (Mg h",a^-1,")")),
               expression(paste("10K-hr Rotten (Mg h",a^-1,")")),
               expression(paste("10K-hr Sound (Mg h",a^-1,")")),
               expression(paste("1-hr (Mg h",a^-1,")")),
               expression(paste("1K-hr (Mg h",a^-1,")")),
               expression(paste("1K-hr Rotten (Mg h",a^-1,")")),
               expression(paste("1K-hr Sound (Mg h",a^-1,")")),
               expression(paste("Coarse Woody Debris (Mg h",a^-1,")")),
               expression(paste("Coarse Woody Debris Rotten (Mg h",a^-1,")")),
               expression(paste("Coarse Woody Debris Sound (Mg h",a^-1,")")),
               "Duff Depth (cm)",
               expression(paste("Duff (Mg h",a^-1,")")),
               expression(paste("Fine Woody Debris (Mg h",a^-1,")")),
               expression(paste("> 10K-hr Rotten (Mg h",a^-1,")")),
               expression(paste("> 10K-hr Sound (Mg h",a^-1,")")),
               expression(paste("Herb (Mg h",a^-1,")")),
               "Herb Percent Live",
               expression(paste("Lichen (Mg h",a^-1,")")),
               "Litter Depth (cm)",
               expression(paste("Litter (Mg h",a^-1,")")),
               expression(paste("Moss (Mg h",a^-1,")")),
               expression(paste("Midstory (Mg h",a^-1,")")),
               expression(paste("Overstory (Mg h",a^-1,")")),
               expression(paste("Shrub (Mg h",a^-1,")")),
               expression(paste("Snag (Mg h",a^-1,")")),
               expression(paste("Tree Crown (Mg h",a^-1,")")),
               expression(paste("Tree (Mg h",a^-1,")")),
               expression(paste("Understory (Mg h",a^-1,")"))
               )


fuel.labelsNoUnits<-c("100-hr","10-hr","10K-hr","10K-hr Rotten","10K-hr Sound",
               "1-hr","1K-hr","1K-hr Rotten","1K-hr Sound",
               "Coarse Woody Debris","Coarse Woody Debris Rotten","Coarse Woody Debris Sound",
               "Duff Depth (cm)","Duff","Fine Woody Debris",
               "> 10K-hr Rotten","> 10K-hr Sound ","Herb","Herb Percent Live",
               "Lichen","Litter Depth (cm)","Litter","Moss",
               "Midstory","Overstory","Shrub","Snag",
               "Tree Crown","Tree","Understory")
###########
fuel.labelsNoUnits<-c("100-hr","10-hr","10K-hr","R 10K-hr","S 10K-hr",
                      "1-hr","1K-hr","R 1000-hr","S 1000-hr",
                      "CWD","R CWD","S CWD",
                      "Duff Depth (cm)","Duff","FWD",
                      "R >10K-hr","S >10K-hr","Herb","Herb Percent Live",
                      "Lichen","Litter Depth (cm)","Litter","Moss",
                      "Midstory","Overstory","Shrub","Snag",
                      "Tree Crown","Tree","Understory")


evt.names<-read.csv("../Data/EVT_AbbrevNames.csv")

for(i in 1:length(evt.vals))
{
#  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
#  for(j in start.col:ncol(data.file))
    for(j in 1:nrow(distributionCustomRankingHurdleNOut[[i]]))
    {
     if(!is.na(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j]))
     {
        postscript(file=paste("HurdleFitGraphs/RemoveOutliers/WithQQAxis/NOutEVT",evt.vals[i],"fuelRow",j,".eps",sep=""),
                   horizontal=FALSE,height=3.5,width=4,onefile=FALSE)
       cur.distr<-switch(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j],
                         gammaLL="gamma",
                         lnormLL="lnorm",
                         normLL="norm",
                         weibullLL="weibull")
       distFitForPubGraph.fn(data.file=data.file,evts=evt.vals[i],cur.cols = j+start.col-1,evt.col = EVTCol,
                            distr=cur.distr,
                            plot.qqAxes = TRUE,x.lab=fuel.labelsNoUnits[j],#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                            main.txt=paste(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.vals[i]]),"; ",
                                           round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$prop0[j]*100,digits=1),"% zero; ",
                                                 cur.distr,sep=""),removeOut=TRUE)
                            # main.txt=paste("EVT",evt.vals[i],cur.distr))
        dev.off()
     }
  }
  
}

# without qq axes
for(i in 1:length(evt.vals))
{
  #  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
  #  for(j in start.col:ncol(data.file))
  for(j in 1:nrow(distributionCustomRankingHurdleNOut[[i]]))
  {
    if(!is.na(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j]))
    {
      postscript(file=paste("HurdleFitGraphs/RemoveOutliers/NOutEVT",evt.vals[i],"fuelRow",j,"NoQQAxis.eps",sep=""),
                 horizontal=FALSE,height=3.5,width=4,onefile=FALSE)
      cur.distr<-switch(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j],
                        gammaLL="gamma",
                        lnormLL="lnorm",
                        normLL="norm",
                        weibullLL="weibull")
      distFitForPubGraph.fn(data.file=data.file,evts=evt.vals[i],cur.cols = j+start.col-1,evt.col = EVTCol,
                            distr=cur.distr,
                            plot.qqAxes = FALSE,x.lab=fuel.labelsNoUnits[j],#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                            main.txt=paste(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.vals[i]]),"; ",
                                           round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$prop0[j]*100,digits=1),"% zero; ",
                                           cur.distr,sep=""),removeOut=TRUE)
      dev.off()
    }
  }
  
}


# All graphs in 1 pdf file
# lnorm and gamma only
pdf(file="HurdleFitGraphs/RemoveOutliers/AllGraphsNOut.pdf",height=3.5,width=10)
for(i in 1:length(evt.vals))
{
  #  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
  #  for(j in start.col:ncol(data.file))
  for(j in 1:nrow(distributionCustomRankingHurdleNOut[[i]]))
  {
    if(!is.na(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j]))
    {
      cur.distr<-switch(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j],
                        gammaLL="gamma",
                        lnormLL="lnorm",
                        normLL="norm",
                        weibullLL="weibull")
      distFitForSupplementaryGraph.fn(data.file=data.file,evts=evt.vals[i],cur.cols = j+start.col-1,evt.col = EVTCol,
                                      distr=cur.distr,
                                      plot.qqAxes = TRUE,x.lab=fuel.labelsNoUnits[j],#x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                                      main.txt=paste(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.vals[i]]),"; ",
                                                     round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$prop0[j]*100,digits=1),"% zero; ",
                                                     cur.distr,sep=""),removeOut=TRUE)
      #                                      main.txt=paste("EVT",evt.vals[i],cur.distr))
    }
  }
  
}
dev.off()

#########
# all fit graphs with Weibull
pdf(file="HurdleFitGraphs/AllGraphsWithWeibull.pdf",height=3.5,width=10)
for(i in 1:length(evt.vals))
{
  #  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
  #  for(j in start.col:ncol(data.file))
  for(j in 1:nrow(distributionCustomRankingHurdleAll[[i]]))
  {
    if(!is.na(distributionCustomRankingHurdleAll[[i]]$dist1.fit[j]))
    {
      cur.distr<-switch(distributionCustomRankingHurdleAll[[i]]$dist1.fit[j],
                        gammaLL="gamma",
                        lnormLL="lnorm",
                        normLL="norm",
                        weibullLL="weibull")
      distFitForSupplementaryGraph.fn(data.file=data.file,evts=evt.vals[i],cur.cols = j+start.col-1,evt.col = EVTCol,
                                      distr=cur.distr,
                                      plot.qqAxes = TRUE,x.lab=fuel.labels[j],#x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                                      main.txt=paste("EVT",evt.vals[i],cur.distr))
    }
  }
  
}
dev.off()

