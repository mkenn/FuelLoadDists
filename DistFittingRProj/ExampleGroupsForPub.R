# Ag, Shrubland, Grassland, Deciduous forest, conifer forest, mixed forest
# EVT groups in order of priority (1-3 to start)
# Ag group 64, 67, 68--not in database
# shrubland group 604, 613, 762 (604 works)
# grassland group 646, 648 (646 works, although only herb loading duh!, 648 doesn't)
# Decid forest group 682, 660, 655 (682 works)
# conifer forest group 785, 683, 758 (785 not in database, 683 works--but peatland?! 758 works, black spruce)
# mixed forest group 664, 685 (664 works, 685 works)
############
load("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
source("../Functions/Dist_Fit_Graph_Pub_FN.R")

evt.vals.pub<-c(64,67,68,604,613,762,646,648,682,660,655,785,683,758,664,685)

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

evt.names<-read.csv("../Data/EVT_AbbrevNames.csv")
###########
for(i in 1:length(evt.vals))
{
  #  tmp.df<-data.file[data.file[,EVTCol]==evt.vals[i],]
  #  for(j in start.col:ncol(data.file))
  for(k in 1:length(evt.vals.pub))
  {
    if(evt.vals[i]==evt.vals.pub[k])
    {
      for(j in 1:nrow(distributionCustomRankingHurdleNOut[[i]]))
      {
        if(!is.na(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j]))
        {
          postscript(file=paste("GraphsForPresentation/NOutEVT",evt.vals[i],"fuelRow",j,".eps",sep=""),
                     horizontal=FALSE,height=3.5,width=4,onefile=FALSE)
          cur.distr<-switch(distributionCustomRankingHurdleNOut[[i]]$dist1.fit[j],
                            gammaLL="gamma",
                            lnormLL="lnorm",
                            normLL="norm",
                            weibullLL="weibull")
          distFitForPubGraph.fn(data.file=data.file,evts=evt.vals[i],cur.cols = j+start.col-1,evt.col = EVTCol,
                                distr=cur.distr,
                                plot.qqAxes = TRUE,x.lab=fuel.labels[j],#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                                main.txt=paste(as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.vals[i]]),"; ",
                                               round(distributionCustomFittingHurdleNOut$HurdleFit[[i]]$prop0[j]*100,digits=1),"% zero; ",
                                               cur.distr,sep=""),removeOut=TRUE)
          # main.txt=paste("EVT",evt.vals[i],cur.distr))
          dev.off()
        }
      }
    }
      }
  
}

