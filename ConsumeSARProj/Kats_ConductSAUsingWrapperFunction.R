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
source("Kats_SA_WrapperFunctions.R")
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

## Generic file name, abbreviated to 'G' to keep code small
G_C ="sample_consume5_input.csv"
G_F = "FOFEM_FlamingSAInput1.csv"

#target.evts<-evt.vals # if targeting all EVTs
evt.ew<-c(655,682,666) # eastern hardwood used for distribution fitting ms
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp # conifer used for distribution fitting ms
target.evts<-c(evt.ew,evt.cf2)



##Creates a vector of length 75 and fills it with generic input file name 
# MCK: Replace with length of target EVT vector
# env.consume.80.infile.names<- rep(G_C, length(target.evts))
# env.consume.97.infile.names<- rep(G_C, length(target.evts))
# env.fofem.80.infile.names<- rep(G_F, length(target.evts))
# env.fofem.97.infile.names<- rep(G_F, length(target.evts))
# env.consume.80.infile.names<- rep(G_C, 75)
# env.consume.97.infile.names<- rep(G_C, 75)
# env.fofem.80.infile.names<- rep(G_F, 75)
# env.fofem.97.infile.names<- rep(G_F, 75)

##These are the locations in the evt vector that are of interest that need a different file name
v1<-c(8, 15, 19, 25, 36, 39, 49, 58, 59)
consume80Names<-c("../../consume.Infile/BigSagebrushShrublandandSteppe80p.csv", "../../consume.Infile/Grassland80p.csv",
                  "../../consume.Infile/DouglasFirPonderosaPineLodgepoleForestandWoodland80p.csv",
                  "../../consume.Infile/PonderosaPineForestWoodlandandSavanna80p.csv", "../../consume.Infile/MixedGrassPrairie80p.csv",
                  "../../consume.Infile/BeechMapleBasswoodForest80p.csv", 
                  "../../consume.Infile/EasternFloodplainForests80p.csv","../../consume.Infile/YellowBirchSugarMapleForest80p.csv","../../consume.Infile/PeatlandForests80p.csv")
consume97Names<-c("../../consume.Infile/BigSagebrushShrublandandSteppe97p.csv", "../../consume.Infile/Grassland97p.csv","../../consume.Infile/DouglasFirPonderosaPineLodgepoleForestandWoodland97p.csv",
                  "../../consume.Infile/PonderosaPineForestWoodlandandSavanna97p.csv", "../../consume.Infile/MixedGrassPrairie97p.csv","../../consume.Infile/BeechMapleBasswoodForest97p.csv", 
                  "../../consume.Infile/EasternFloodplainForests97p.csv","../../consume.Infile/YellowBirchSugarMapleForest97p.csv","../../consume.Infile/PeatlandForests97p.csv")
fofem80Names<-c("../fofem.infile/BigSagebrushShrublandandSteppe80p.csv", "../fofem.infile/Grassland80p.csv","../fofem.infile/DouglasFirPonderosaPineLodgepoleForestandWoodland80p.csv",
                "../fofem.infile/PonderosaPineForestWoodlandandSavanna80p.csv", "../fofem.infile/MixedGrassPrairie 80p.csv","../fofem.infile/BeechMapleBasswoodForest80p.csv", 
                "../fofem.infile/EasternFloodplainForests80p.csv","../fofem.infile/YellowBirchSugarMapleForest80p.csv","../fofem.infile/PeatlandForests80p.csv")
fofem97Names<-c("../fofem.infile/BigSagebrushShrublandandSteppe97p.csv", "../fofem.infile/Grassland97p.csv","../fofem.infile/DouglasFirPonderosaPineLodgepoleForestandWoodland97p.csv",
                "../fofem.infile/PonderosaPineForestWoodlandandSavanna97p.csv", "../fofem.infile/MixedGrassPrairie97p.csv","../fofem.infile/BeechMapleBasswoodForest97p.csv", 
                "../fofem.infile/EasternFloodplainForests97p.csv","../fofem.infile/YellowBirchSugarMapleForest97p.csv","../fofem.infile/PeatlandForests97p.csv")

##take the vector and replace the locations of interest witht the corresponding file names
# env.consume.80.infile.names[v1] = consume80Names
# env.consume.97.infile.names[v1]= consume97Names
# env.fofem.80.infile.names[v1]= fofem80Names
# env.fofem.97.infile.names[v1]= fofem97Names



#list for fofem and consume that contain both 80 and 97 percentile 
# consume.env.infile.names.list<-list(env.consume.80.infile.names, env.consume.97.infile.names)
# fofem.env.infile.names.list<-list(env.fofem.80.infile.names,env.fofem.97.infile.names)
consume.env.infile.names.list<-list(consume80Names, consume97Names)
fofem.env.infile.names.list<-list(fofem80Names,fofem97Names)

##
# how many samples? Eventually probably 5000, let's just do 10 for now
##
N.samp<-10
#######
# flaming first
#######
# generate the random samples
# Keep all results in a list
# Keep all samples in a list
fuels.flaming.mats<-list()
flaming.sa.results<-list()

make.graph=FALSE

if(make.graph)
  pdf(file="FirstTryAllEVTsResultsUsingWrapper2.pdf")
#for(m in 1:length(evt.vals)) #set m 
for(k in 1:length(target.evts)) #set m 
{
  m<-which(evt.vals==target.evts[[k]])
  fuels.flaming.mats[[k]]<-list()
  flaming.sa.results[[k]]<-list()
  if(evt.vals[m]>300)
  {
    
    cur.evt.num<-evt.vals[m]
    cur.evt<-which(evt.vals==cur.evt.num)
    
    fuels.flaming.mats[[k]]<-makeCorrelatedSampleMat_Wrapper.fn(data.file,fuel.ids=flaming2.id,complete.case=TRUE,
                                                           min.co.occur=30,
                                                           evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
                                                           rankObj=distributionCustomRankingHurdleNOut,
                                                           fitObj=distributionCustomFittingHurdleNOut,cur.evt.id=cur.evt)
    # call the emissions models. This wrapper calls both models, for both
    # prcc and sobol. Below uses correlated sample inputs
    if(!is.na(fuels.flaming.mats[[k]])[1])
    {
      for(j in 1:2) {
        
      
      flaming.sa.results[[k]]<-ModSA_Wrapper.fn(corr.samp.vals.sobol = fuels.flaming.mats[[k]]$corr.samp.vals.sobol,
                                           corr.samp.vals.prcc = fuels.flaming.mats[[k]]$corr.samp.vals.prcc,nreps = nreps,
                                           fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,evtFB.map=evtFB.map,
                                           cur.evt.num = cur.evt.num,change.units=T,
                                           infilename="FuelLoadInputSA.csv",mod="F",phase = "F",
                                           env.in.name=consume.env.infile.names.list[[j]][k], envfilename="EnvInputSA.csv",
                                           fofem.filename=fofem.env.infile.names.list[[j]][k],newwdF="fofem",oldwdF="../",
                                           newwdC="consume5/apps-consume/",oldwdC="../../",base.fofem=base.fofem.use)     ##fofem.inname
      ###########
      # now graph results
      ###########
      if(make.graph)
      {        
        par(mfrow=c(3,3),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
        
        #Sobol Consume Flaming
        graphResult_Wrapper.fn(analysisType = "sobol", modelListType = "sens.consume.list", modelResponse.vars = responseFConsume.vars,
                               mats.sobol.obj = fuels.flaming.mats[[m]]$sobol.obj, sobolResults = flaming.sa.results$sobolCResults,
                               corr.samp.vals.sobol = fuels.flaming.mats[[m]]$corr.samp.vals.sobol, n.var=6,sobol.obj = sens.consume.list,
                               x.lab="Consume Flaming",y.lab="Sobol output:",y.lim=c(-0.15,1), main.txt, corr.samp.vals.prcc,prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj, evt.vals = evt.vals[cur.evt]) 
        
        #Sobol Consume Smoldering
        graphResult_Wrapper.fn(analysisType = "sobol", modelListType = "sens.consume.list",modelResponse.vars = responseSConsume.vars,
                               mats.sobol.obj = fuels.flaming.mats[[m]]$sobol.obj, sobolResults = flaming.sa.results$sobolCResults,
                               corr.samp.vals.sobol = fuels.flaming.mats[[m]]$corr.samp.vals.sobol, n.var=6,sobol.obj = sens.consume.list,
                               x.lab="Consume Smoldering",y.lab="Sobol output:",y.lim=c(-0.15,1), main.txt, corr.samp.vals.prcc,prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj, evt.vals = evt.vals[cur.evt]) 
        
        #Sobol FOFEM Flaming
        graphResult_Wrapper.fn(analysisType = "sobol", modelListType = "sens.fofem.list",modelResponse.vars = responseFFOFEM.vars,
                               mats.sobol.obj = fuels.flaming.mats[[m]]$sobol.obj, sobolResults = flaming.sa.results$sobolFResults,
                               corr.samp.vals.sobol = fuels.flaming.mats[[m]]$corr.samp.vals.sobol, n.var=6,sobol.obj = sens.fofem.list,
                               x.lab="FOFEM Flaming",y.lab="Sobol output:",y.lim=c(-0.15,1), main.txt, corr.samp.vals.prcc,prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj, evt.vals = evt.vals[cur.evt]) 
        #Sobol FOFEM Smoldering
        graphResult_Wrapper.fn(analysisType = "sobol", modelListType = "sens.fofem.list",modelResponse.vars = responseSFOFEM.vars,
                               mats.sobol.obj = fuels.flaming.mats[[m]]$sobol.obj, sobolResults = flaming.sa.results$sobolFResults,
                               corr.samp.vals.sobol = fuels.flaming.mats[[m]]$corr.samp.vals.sobol, n.var=6,sobol.obj = sens.fofem.list,
                               x.lab="FOFEM Smolder",y.lab="Sobol output:",y.lim=c(-0.15,1), main.txt, corr.samp.vals.prcc,prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj, evt.vals = evt.vals[cur.evt]) 
        #PRCC Consume Flaming
        graphResult_Wrapper.fn(analysisType = "PRCC", modelListType = "consumeF.prcc", modelResponse.vars = responseFConsume.vars,
                               mats.sobol.obj, sobolResults,corr.samp.vals.sobol, n.var=6,sobol.obj, x.lab="Consume Flaming",y.lab="PRCC output:",y.lim=c(-0.15,1), main.txt, 
                               corr.samp.vals.prcc = fuels.flaming.mats[[m]]$corr.samp.vals.prcc,prccCResults = flaming.sa.results$prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj = consumeF.prcc, evt.vals = evt.vals[cur.evt]) 
        #PRCC Consume Smoldering
        graphResult_Wrapper.fn(analysisType = "PRCC", modelListType = "consumeS.prcc", modelResponse.vars = responseSConsume.vars,
                               mats.sobol.obj, sobolResults,corr.samp.vals.sobol, n.var=6,sobol.obj,x.lab="Consume Smoldering",y.lab="PRCC output:",y.lim=c(-0.15,1), main.txt, 
                               corr.samp.vals.prcc = fuels.flaming.mats[[m]]$corr.samp.vals.prcc,prccCResults = flaming.sa.results$prccCResults,
                               rank=TRUE,nboot=1000, prcc.obj = consumeF.prcc, evt.vals = evt.vals[cur.evt]) 
        #PRCC FOFEM Flaming
        graphResult_Wrapper.fn(analysisType = "PRCC", modelListType = "fofemF.prcc", modelResponse.vars = responseFFOFEM.vars,
                               mats.sobol.obj, sobolResults,corr.samp.vals.sobol, n.var=6,sobol.obj, x.lab="FOFEM Flaming",y.lab="PRCC output:",y.lim=c(-0.15,1), main.txt, 
                               corr.samp.vals.prcc = fuels.flaming.mats[[m]]$corr.samp.vals.prcc,prccCResults = flaming.sa.results$prccFResults,
                               rank=TRUE,nboot=1000, prcc.obj = consumeF.prcc, evt.vals = evt.vals[cur.evt]) 
        #PRCC FOFEM Smoldering
        graphResult_Wrapper.fn(analysisType = "PRCC", modelListType = "fofemS.prcc", modelResponse.vars = responseSFOFEM.vars,
                               mats.sobol.obj, sobolResults,corr.samp.vals.sobol, n.var=6,sobol.obj, x.lab="FOFEM Smoldering",y.lab="PRCC output:",y.lim=c(-0.15,1), main.txt, 
                               corr.samp.vals.prcc = fuels.flaming.mats[[m]]$corr.samp.vals.prcc,prccCResults = flaming.sa.results$prccFResults,
                               rank=TRUE,nboot=1000, prcc.obj = consumeF.prcc, evt.vals = evt.vals[cur.evt]) 
      }

      }
    }
  }
}

if(make.graph)
  dev.off()

