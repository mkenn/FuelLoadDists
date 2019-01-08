#####
# identify which EVT groups have sufficient coverage for smoldering SA
# and if those have sufficient coverage for flaming SA
######
library(sensitivity)

load("../DistFittingRProj/Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
source("../Functions/SimHurdleDist_FN.R")
source("../Functions/SensitivitySampleWithCorr_FN.R")
source("CallModsForSA.R") 
source("PlotSobolAndPRCCResults.R") # functions to graph Sobol and PRCC results
source("Kats_SA_WrapperFunctions.R")
source("GenerateConsumeLoadingsFileCorrSamp.R") #This function actually switches between consume and fofem to generate input file. 
# read in the mapping between the EVT groups and a characteristic fuel bed
evtFB.map<-read.csv("EVT_Fuelbed_xwalkForPub.csv") 

# evt group names
evGroupNames<-read.csv("../Data/EVT_AbbrevNames.csv")

smolder1.id<-c("X10KhrR_loading_Mgha","X10KhrS_loading_Mgha","X1000hrR_loading_Mgha","X1000hrS_loading_Mgha",
               "GT10KhrR_loading_Mgha","GT10KhrS_loading_Mgha")

smolder2.id<-c("cwd_sound_loading_Mgha","cwd_rotten_loading_Mgha","duff_loading_Mgha")
flaming2.id<-c("X100hr_loading_Mgha","X10hr_loading_Mgha","X1hr_loading_Mgha","herb_loading_Mgha",  
               "litter_loading_Mgha","shrub_loading_Mgha" )

cases<-data.frame(evt.vals,smolderN=NA,smolder2N=NA,flamingN=NA)

for(i in 1:length(evt.vals))
{
  if(evt.vals[i]>300)
  {
    tmpS.df<-data.file[data.file[,EVTCol]==evt.vals[i],smolder2.id]
    tmpS2.df<-data.file[data.file[,EVTCol]==evt.vals[i],smolder2.id]
    tmpF.df<-data.file[data.file[,EVTCol]==evt.vals[i],flaming2.id]
    
    
    curS.loads<-tmpS.df[complete.cases(tmpS.df),]
    cases$smolderN[i]<-nrow(curS.loads)
    curS2.loads<-tmpS.df[complete.cases(tmpS2.df),]
    cases$smolder2N[i]<-nrow(curS2.loads)
    curF.loads<-tmpF.df[complete.cases(tmpF.df),]
    cases$flamingN[i]<-nrow(curF.loads)
    
  }
}

evtS.vals<-evt.vals[which(cases$smolderN>30)]

evtSNames<-data.frame(evtGroup=evtS.vals,evtName=NA)
for(i in 1:length(evtS.vals))
  evtSNames$evtName[i]<-as.character(evGroupNames$EVT_GP_Name[evGroupNames$EVT_GP==evtS.vals[i]])

evtS2.vals<-evt.vals[which(cases$smolderN>30)]

evtS2Names<-data.frame(evtGroup=evtS2.vals,evtName=NA)
for(i in 1:length(evtS2.vals))
  evtS2Names$evtName[i]<-as.character(evGroupNames$EVT_GP_Name[evGroupNames$EVT_GP==evtS2.vals[i]])

