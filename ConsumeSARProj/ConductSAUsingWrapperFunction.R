##########
# to run consume and fofem SA
# using modular functions
###########
# read in the necessary files
###########
library(sensitivity)

load("../DistFittingRProj/Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
source("../Functions/SimHurdleDist_FN.R")
source("../Functions/SensitivitySampleWithCorr_FN.R")
source("CallModsForSA.R") 
source("PlotSobolAndPRCCResults.R") # functions to graph Sobol and PRCC results
source("SAWrapperFunctions.R")
source("GenerateConsumeLoadingsFileCorrSamp.R") #This function actually switches between consume and fofem to generate input file. 

# read in the mapping between the EVT groups and a characteristic fuel bed
evtFB.map<-read.csv("EVT_Fuelbed_xwalk.csv") 
# read in the FCCS fuelbeds
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",
                    stringsAsFactors=TRUE) 
#  fueltype names in FCCS with those in database names
fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv",
                           stringsAsFactors = FALSE,sep=",",header=TRUE)
base.fofem<-read.csv("fofem/FOFEM_6_May12013_MCKUpdate2.csv") 
# note, this file has the header--our final file will not
# note also this has the fuel moistures matched to consume env input as
# best we can. If that changes, so should this
base.fofem.use<-base.fofem[1,]
# now choose variable groupings for flaming and smoldering
smolder1.id<-c("X10KhrR_loading_Mgha","X10KhrS_loading_Mgha","X1000hrR_loading_Mgha","X1000hrS_loading_Mgha",
               "GT10KhrR_loading_Mgha","GT10KhrS_loading_Mgha")
smolder2.id<-c("cwd_sound_loading_Mgha","cwd_rotten_loading_Mgha","duff_loading_Mgha")

flaming1.id<-c("X100hr_loading_Mgha","X10hr_loading_Mgha","X1hr_loading_Mgha","herb_loading_Mgha",  
               "lichen_loading_Mgha","litter_loading_Mgha","moss_loading_Mgha","shrub_loading_Mgha" )
flaming2.id<-c("X100hr_loading_Mgha","X10hr_loading_Mgha","X1hr_loading_Mgha","herb_loading_Mgha",  
               "litter_loading_Mgha","shrub_loading_Mgha" )

N.samp<-100 # number of random samples for SA, set low for now for efficiency and testing

responseFConsume.vars<-c("e_co_f","e_co2_f","e_pm25_f")
responseSConsume.vars<-c("e_co_s","e_co2_s","e_pm25_s")
responseFFOFEM.vars<-c("COF","CO2F","PM25F")
responseSFOFEM.vars<-c("COS","CO2S","PM25S")


#######
# flaming first
#######
# generate the random samples
pdf(file="FirstTryAllEVTsResultsUsingWrapper2.pdf")
for(m in 1:length(evt.vals))
{
  if(evt.vals[m]>300)
  {
    cur.evt.num<-evt.vals[m]
    cur.evt<-which(evt.vals==cur.evt.num)
    
    fuels.flaming.mats<-makeCorrelatedSampleMat_Wrapper.fn(data.file,fuel.ids=flaming2.id,complete.case=TRUE,
                                                           min.co.occur=30,
                                                           evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
                                                           rankObj=distributionCustomRankingHurdleNOut,
                                                           fitObj=distributionCustomFittingHurdleNOut,cur.evt.id=cur.evt)
    # call the emissions models. This wrapper calls both models, for both
    # prcc and sobol. Below uses correlated sample inputs
    if(!is.na(fuels.flaming.mats)[1])
    {
      flaming.sa.results<-ModSA_Wrapper.fn(corr.samp.vals.sobol = fuels.flaming.mats$corr.samp.vals.sobol,
                                           corr.samp.vals.prcc = fuels.flaming.mats$corr.samp.vals.prcc,nreps = nreps,
                                           fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,evtFB.map=evtFB.map,
                                           cur.evt.num = cur.evt.num,change.units=T,
                                           infilename="FuelLoadInputSA.csv",mod="F",phase = "F",
                                           env.in.name="sample_consume5_input.csv",envfilename="EnvInputSA.csv",
                                           fofem.filename="FOFEM_FlamingSAInput1.csv",newwdF="fofem",oldwdF="../",
                                           newwdC="consume5/apps-consume/",oldwdC="../../",base.fofem=base.fofem.use)
      ###########
      # now graph results
      ###########
      par(mfrow=c(2,3),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
      sens.consume.list<-list()
      for(k in 1:length(responseFConsume.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        sens.consume.list[[k]]<-tell(fuels.flaming.mats$sobol.obj,flaming.sa.results$sobolCResults[,responseFConsume.vars[k]])
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        rownames(sens.consume.list[[k]]$S)<-names(fuels.flaming.mats$corr.samp.vals.sobol)
        my.plot.sobol(n.var = 6,sobol.obj = sens.consume.list[[k]],
                      main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",responseFConsume.vars[k]))
      }
      
      for(k in 1:length(responseSConsume.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        sens.consume.list[[k]]<-tell(fuels.flaming.mats$sobol.obj,
                                   flaming.sa.results$sobolCResults[,responseSConsume.vars[k]])
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        rownames(sens.consume.list[[k]]$S)<-names(fuels.flaming.mats$corr.samp.vals.sobol)
        my.plot.sobol(n.var = 6,sobol.obj = sens.consume.list[[k]],
                      main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",responseSConsume.vars[k]))
      }
      
      sens.fofem.list<-list()
      for(k in 1:length(responseFFOFEM.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        sens.fofem.list[[k]]<-tell(fuels.flaming.mats$sobol.obj,flaming.sa.results$sobolFResults[,responseFFOFEM.vars[k]])
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        rownames(sens.fofem.list[[k]]$S)<-names(fuels.flaming.mats$corr.samp.vals.sobol)
        my.plot.sobol(n.var = 6,sobol.obj = sens.fofem.list[[k]],
                      main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseFFOFEM.vars[k]))
      }
      
      for(k in 1:length(responseFFOFEM.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        sens.fofem.list[[k]]<-tell(fuels.flaming.mats$sobol.obj,
                                   flaming.sa.results$sobolFResults[,responseSFOFEM.vars[k]])
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        rownames(sens.fofem.list[[k]]$S)<-names(fuels.flaming.mats$corr.samp.vals.sobol)
        my.plot.sobol(n.var = 6,sobol.obj = sens.fofem.list[[k]],
                      main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseSFOFEM.vars[k]))
      }
      
      consumeF.prcc<-list()
      for(k in 1:length(responseFConsume.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        consumeF.prcc[[k]]<-pcc(fuels.flaming.mats$corr.samp.vals.prcc,
                              y=flaming.sa.results$prccCResults[,responseFConsume.vars[k]],
                              rank=TRUE,nboot=1000)
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        my.plot.prcc(n.var=6,prcc.obj = consumeF.prcc[[k]],
                     main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",responseFConsume.vars[k]))
      }
      
      consumeF.prcc<-list()
      for(k in 1:length(responseSFOFEM.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        consumeF.prcc[[k]]<-pcc(fuels.flaming.mats$corr.samp.vals.prcc,
                              y=flaming.sa.results$prccCResults[,responseSConsume.vars[k]],
                              rank=TRUE,nboot=1000)
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        my.plot.prcc(n.var=6,prcc.obj = consumeF.prcc[[k]],
                     main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",responseSConsume.vars[k]))
      }

      fofemF.prcc<-list()
      for(k in 1:length(responseFFOFEM.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        fofemF.prcc[[k]]<-pcc(fuels.flaming.mats$corr.samp.vals.prcc,
                              y=flaming.sa.results$prccFResults[,responseFFOFEM.vars[k]],
                              rank=TRUE,nboot=1000)
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        my.plot.prcc(n.var=6,prcc.obj = fofemF.prcc[[k]],
                     main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseFFOFEM.vars[k]))
      }
      
      fofemS.prcc<-list()
      for(k in 1:length(responseSFOFEM.vars))
      { # here we standardize outputs to the z-scoore
        # sens.list[[k]]<-tell(sobolF,
        #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
        fofemS.prcc[[k]]<-pcc(fuels.flaming.mats$corr.samp.vals.prcc,
                              y=flaming.sa.results$prccFResults[,responseSFOFEM.vars[k]],
                              rank=TRUE,nboot=1000)
        # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
        my.plot.prcc(n.var=6,prcc.obj = fofemS.prcc[[k]],
                     main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseSFOFEM.vars[k]))
      }
    }
  }
}

dev.off()

