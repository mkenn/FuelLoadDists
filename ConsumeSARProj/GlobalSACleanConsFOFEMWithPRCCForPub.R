#############
# conduct sensitiviy analysis of
# CONSUME and FOFEM
# using both Sobol variance partitioning and
# partial rank correlation coefficient
# and using correlated input variables
#############

load("../DistFittingRProj/Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
set.seed(NULL) # reset the seed, otherwise get same seed every time based on loading this workspace
# read in the mapping between the EVT groups and a characteristic fuel bed
evtFB.map<-read.csv("EVT_Fuelbed_xwalk.csv") # FCCSID is the fuelbed number, EVT_GP is the EVT group
# read in the FCCS fuelbeds
# skip the header line, this is the FCCS fuelbeds file to give baseline values for the fuel loadings
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",
                    stringsAsFactors=TRUE) 
#  fueltype names in FCCS with those in database names
fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv",
                           stringsAsFactors = FALSE,sep=",",header=TRUE)

# should contain everything necessary to estimate correlation matrices
# Make sure that the woody totals are present
# data.file$X10Khr_loading_Mgha<-data.file$X10KhrR_loading_Mgha+data.file$X10KhrS_loading_Mgha
# data.file$X1Khr_loading_Mgha<-data.file$X1KhrR_loading_Mgha+data.file$X1KhrS_loading_Mgha
# 
# data.file$cwd_loading_Mgha<-data.file$X10Khr_loading_Mgha+data.file$X1Khr_loading_Mgha+data.file$GT10KhrR_loading_Mgha+data.file$GT10KhrS_loading_Mgha
# 
# 
# data.file$fwd_loading_Mgha<-data.file$X1hr_loading_Mgha+data.file$X10hr_loading_Mgha+data.file$X100hr_loading_Mgha

# now choose variable groupings for flaming and smoldering
# by column names, has to be updated if column names change
smolder1.id<-c("X10KhrR_loading_Mgha","X10KhrS_loading_Mgha","X1000hrR_loading_Mgha","X1000hrS_loading_Mgha",
               "GT10KhrR_loading_Mgha","GT10KhrS_loading_Mgha")
# smolder1.id<-c(4,5,8,9,16,17)
# names(data.file)[smolder1.id+start.col-1]
# "X10KhrR_loading_Mgha"  "X10KhrS_loading_Mgha"  "X1KhrR_loading_Mgha"   "X1KhrS_loading_Mgha"  
# "GT10KhrR_loading_Mgha" "GT10KhrS_loading_Mgha"
# These aren't in consume, which requires R and S for all
# coarse fuels
smolder2.id<-c("X10Khr_loading_Mgha","X1000hr_loading_Mgha",
               "GT10KhrR_loading_Mgha","GT10KhrS_loading_Mgha")
#smolder2.id<-c(3,7,16,17) # because we don't seem to have a total column for >10K
# "X10Khr_loading_Mgha"   "X1Khr_loading_Mgha"    "GT10KhrR_loading_Mgha" "GT10KhrS_loading_Mgha"

smolder3.id<-c("cwd_sound_loading_Mgha","cwd_rotten_loading_Mgha","duff_loading_Mgha")

#flaming1.id<-c(1,2,6,18,20,22,23,26)
flaming1.id<-c("X100hr_loading_Mgha","X10hr_loading_Mgha","X1hr_loading_Mgha","herb_loading_Mgha",  
  "lichen_loading_Mgha","litter_loading_Mgha","moss_loading_Mgha","shrub_loading_Mgha" )
flaming2.id<-c("fwd_loading_Mgha","herb_loading_Mgha",  
               "lichen_loading_Mgha","litter_loading_Mgha","moss_loading_Mgha","shrub_loading_Mgha" )
#flaming2.id<-c(15,18,20,22,23,26) # assuming fwd is 1-hr+10-hr+100-hr
#"fwd_loading_Mgha"    "herb_loading_Mgha"   "lichen_loading_Mgha" "litter_loading_Mgha"
#"moss_loading_Mgha"   "shrub_loading_Mgha" 
# except no consume equivalent fwd
flaming3.id<-c("X100hr_loading_Mgha","X10hr_loading_Mgha","X1hr_loading_Mgha","herb_loading_Mgha",  
               "litter_loading_Mgha","shrub_loading_Mgha" )
#flaming3.id<-c(1,2,6,18,22,26) # remove lichen and moss because those data seem to be rare
#"X100hr_loading_Mgha" "X10hr_loading_Mgha"  "X1hr_loading_Mgha"   "herb_loading_Mgha"  
#"litter_loading_Mgha" "shrub_loading_Mgha" 
############
# candidate EVTs for database manuscript, choose from these
# Ag, Shrubland, Grassland, Deciduous forest, conifer forest, mixed forest
# EVT groups in order of priority (1-3 to start)
# Ag group 64, 67, 68--not in database
# shrubland group 604, 613, 762 (604 works)
# grassland group 646, 648 (646 works, although only herb loading duh!, 648 doesn't)
# Decid forest group 682, 660, 655 (682 works)
# conifer forest group 785, 683, 758 (785 not in database, 683 works--but peatland?! 758 works, black spruce)
# mixed forest group 664, 685 (664 works, 685 works)
############

# this gives the index location for the EVT
cur.evt<-58 # example EVT candidate from database paper, with sufficient coverage of flaming fuel types,
# using flaming3.id; EVT 682, Decid forest group
# need to find one for smoldering
###############
# flaming first
N.samp<-1000 # number of random samples for SA, set low for now for efficiency and testing

source("../Functions/SimHurdleDist_FN.R")
source("../Functions/SensitivitySampleWithCorr_FN.R")
# samples from entire distribution, creating a data matrix with
# correlation structure that matches that for the database for that EVT--the matrix X*
# also includes the uncorrelated matrix X for comparison
sens.mats.flame.list<-corr.sa.fn(data.file,fuel.ids=flaming2.id,complete.case=TRUE,
                                 min.co.occur=30,
                                 evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
                                 rankObj=distributionCustomRankingHurdleNOut,
                                 fitObj=distributionCustomFittingHurdleNOut)
sens.mats.smolder.list<-corr.sa.fn(data.file,fuel.ids=smolder3.id,complete.case=TRUE,
                                 min.co.occur=30,
                                 evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
                                 rankObj=distributionCustomRankingHurdleNOut,
                                 fitObj=distributionCustomFittingHurdleNOut)
# samples from truncated distribution (<q_.95), creating a data matrix with
# correlation structure that matches that for the database for that EVT, the matrix X*
sens.mats.flame.list.upper<-corr.sa.fn(data.file,fuel.ids=flaming3.id,complete.case=TRUE,
                                       min.co.occur=30,
                                       evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
                                       rankObj=distributionCustomRankingHurdleNOut,
                                       fitObj=distributionCustomFittingHurdleNOut,
                                       upper.quantile=0.95)

# # now create the sobol object, which rearranges X* to be appropriate for variance partitioning algorithm
library(sensitivity)
sobolF<-sobolEff(X1=sens.mats.flame.list[[1]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.flame.list[[1]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)
sobolS<-sobolEff(X1=sens.mats.smolder.list[[1]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.smolder.list[[1]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)
sobolFNoCorr<-sobolEff(X1=sens.mats.flame.list[[2]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.flame.list[[2]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)

#compare the Sobol correlation matrix to the original sample
# cor(sobolF$X)
# cor(sens.mats.flame.list[[1]][[cur.evt]])


source("GenerateConsumeLoadingsFileCorrSamp.R") #This function actually switches between consume and fofem to generate input file. 
cur.evt.num<-evt.vals[cur.evt]
base.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt.num]# identify the base fb associated with this evt group
base.fb<-unique(base.fb[!is.na(base.fb)])[1]

corr.sampF.vals<-data.frame(sobolF$X)
names(corr.sampF.vals)<-names(sens.mats.flame.list[[1]][[cur.evt]])
corr.sampS.vals<-data.frame(sobolS$X)
names(corr.sampS.vals)<-names(sens.mats.smolder.list[[1]][[cur.evt]])
Nocorr.sampF.vals<-data.frame(sobolFNoCorr$X)
names(Nocorr.sampF.vals)<-names(sens.mats.flame.list[[1]][[cur.evt]])
###############
# first CONSUME
# create a dataframe that combines baseline values with sobol-sampled ones
consume.fuel.loads<-GenerateFuelInput.fn(corr.sampF.vals,nreps,
                                              fbLoadNames.df,all.fbs,base.fb,change.units=T)
consume.smolder.fuel.loads<-GenerateFuelInput.fn(corr.sampS.vals,nreps,
                                         fbLoadNames.df,all.fbs,base.fb,change.units=T,phase = "S")

consumeNoCorr.fuel.loads<-GenerateFuelInput.fn(Nocorr.sampF.vals,nreps,
                                         fbLoadNames.df,all.fbs,base.fb,change.units=T)

source("CallModsForSA.R") #This function actually switches between consume and fofem to generate input file. 
consume.sobol.sa.results<-call.emissions.mods(fuel.loads = consume.fuel.loads)
consume.sobol.sa.smolder.results<-call.emissions.mods(fuel.loads = consume.smolder.fuel.loads)
consume.sobolNoCorr.sa.results<-call.emissions.mods(fuel.loads = consumeNoCorr.fuel.loads)#,outfilename = "consumeNoCorr_output_summary.csv")

source("PlotSobolAndPRCCResults.R") # functions to graph Sobol and PRCC results
# calculate sobol indices for selected model predictions
#response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions","PM25.Emissions")
# responseFConsume.vars<-c("E_co_F","E_co2_F","E_pm25_F")
# responseSConsume.vars<-c("E_co_S","E_co2_S","E_pm25_S")
response.vars<-c("pm","co","co2","pm25")
responseFConsume.vars<-c("e_co_F","e_co2_F","e_pm25_F")
responseSConsume.vars<-c("e_co_S","e_co2_S","e_pm25_S")
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
sens.list<-list()
for(k in 1:length(response.vars))
{ # here we standardize outputs to the z-score
  # sens.list[[k]]<-tell(sobolF,
  #                      (results.sa[,response.vars[k]]-mean(results.sa[,response.vars[k]]))/sd(results.sa[,response.vars[k]]))
  sens.list[[k]]<-tell(sobolF,consume.sobol.sa.results[,response.vars[k]]) # for non-centered
  
    
  rownames(sens.list[[k]]$S)<-names(corr.sampF.vals)
  my.plot.sobol(n.var = nrow(sens.list[[k]]$S),sobol.obj = sens.list[[k]],
                main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",response.vars[k]))
}

par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
sens.list<-list()
for(k in 1:length(response.vars))
{ # here we standardize outputs to the z-score
  # sens.list[[k]]<-tell(sobolF,
  #                      (results.sa[,response.vars[k]]-mean(results.sa[,response.vars[k]]))/sd(results.sa[,response.vars[k]]))
  sens.list[[k]]<-tell(sobolS,consume.sobol.sa.smolder.results[,response.vars[k]]) # for non-centered
  
  
  rownames(sens.list[[k]]$S)<-names(corr.sampS.vals)
  my.plot.sobol(n.var = nrow(sens.list[[k]]$S),sobol.obj = sens.list[[k]],
                main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",response.vars[k]))
}


par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
sensNoCorr.list<-list()
for(k in 1:length(response.vars))
{ # here we standardize outputs to the z-score
  # sens.list[[k]]<-tell(sobolF,
  #                      (results.sa[,response.vars[k]]-mean(results.sa[,response.vars[k]]))/sd(results.sa[,response.vars[k]]))
  sensNoCorr.list[[k]]<-tell(sobolF,consume.sobolNoCorr.sa.results[,response.vars[k]]) # for non-centered
  
  
  rownames(sensNoCorr.list[[k]]$S)<-names(corr.sampF.vals)
  my.plot.sobol(n.var = nrow(sensNoCorr.list[[k]]$S),sobol.obj = sensNoCorr.list[[k]],
                main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",response.vars[k]))
}

# now the prcc
# we need to generate a new loadings file with just the X* matrix, unmodified by Sobol
consume2.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[1]][[cur.evt]],nreps,
                                         fbLoadNames.df,all.fbs,base.fb,change.units=T)
consume2NoCorr.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[2]][[cur.evt]],nreps,
                                          fbLoadNames.df,all.fbs,base.fb,change.units=T)
consume2.prcc.sa.results<-call.emissions.mods(fuel.loads = consume2.fuel.loads)
consume2.prccNoCorr.sa.results<-call.emissions.mods(fuel.loads = consume2NoCorr.fuel.loads)


consumeF.prcc<-list()
consumeFNoCorr.prcc<-list()
for(k in 1:length(response.vars))
  consumeF.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=consume2.prcc.sa.results[,response.vars[k]],
                          rank=TRUE,nboot=1000)
for(k in 1:length(response.vars))
  consumeFNoCorr.prcc[[k]]<-pcc(sens.mats.flame.list[[2]][[cur.evt]],y=consume2.prccNoCorr.sa.results[,response.vars[k]],
                          rank=TRUE,nboot=1000)
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(response.vars))
  my.plot.prcc(n.var=length(flaming3.id),prcc.obj = consumeF.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",response.vars[k]))

par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(response.vars))
  my.plot.prcc(n.var=length(flaming3.id),prcc.obj = consumeFNoCorr.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"Consume No Corr","output:",response.vars[k]))
  
# Start looking at graphing the emissions and determining uncertainty intervals

boxplot(results2.sa[,response.vars[k]]) # CO2 takes the cake!

#########
# now FOFEM
#setwd("fofem")
base.fofem<-read.csv("fofem/FOFEM_6_May12013_MCKUpdate2.csv") 
# note, this file has the header--our final file 
# note also this has the fuel moistures matched to consume env input as
# best we can. If that changes, so should this
base.fofem.use<-base.fofem[1,]

fofem.fuel.loads<-GenerateFuelInput.fn(corr.sampF.vals,nreps,fbLoadNames.df,
                                            all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")
fofemNoCorr.fuel.loads<-GenerateFuelInput.fn(Nocorr.sampF.vals,nreps,fbLoadNames.df,
                                       all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")
results.fofem.sa<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",fuel.loads=fofem.fuel.loads,
                              env.in.name="sample_consume_input.csv",envfilename="EnvInputSA.csv",
                              fofem.filename="FOFEM_FlamingSAInput1.csv",newwd="fofem",oldwd="../")
resultsNoCorr.fofem.sa<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",fuel.loads=fofemNoCorr.fuel.loads,
                                      env.in.name="sample_consume_input.csv",envfilename="EnvInputSA.csv",
                                      fofem.filename="FOFEM_FlamingSAInput1.csv",newwd="fofem",oldwd="../")

  # note: FOFEM divides emissions into flaming and smoldering, which consume doesn't seem
  # to do. Will complicate comparisons. Except it does! See addition above
# the issue really is which units the output is given in
  # for flaming:
  responseF.vars<-c("COF","CO2F","PM25F")
  
  # for smoldering:
  responseS.vars<-c("COS","CO2S","PM25S")
  
  # now plots
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
  sens.fofem.list<-list()
  sens.fofemNoCorr.list<-list()
  for(k in 1:length(responseF.vars))
  { # here we standardize outputs to the z-scoore
     # sens.list[[k]]<-tell(sobolF,
     #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
    sens.fofem.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
     # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
    rownames(sens.fofem.list[[k]]$S)<-names(corr.sampF.vals)
    my.plot.sobol(n.var = 6,sobol.obj = sens.fofem.list[[k]],
                  main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseF.vars[k]))
  }
  
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
  for(k in 1:length(responseF.vars))
  { # here we standardize outputs to the z-scoore
    # sens.list[[k]]<-tell(sobolF,
    #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
    sens.fofemNoCorr.list[[k]]<-tell(sobolF,resultsNoCorr.fofem.sa[,responseF.vars[k]])
    # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
    rownames(sens.fofemNoCorr.list[[k]]$S)<-names(corr.sampF.vals)
    my.plot.sobol(n.var = 6,sobol.obj = sens.fofemNoCorr.list[[k]],
                  main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM NoCorr","output:",responseF.vars[k]))
  }
  
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
  sens.fofemS.list<-list()
  sens.fofemNoCorrS.list<-list()
  for(k in 1:length(responseS.vars))
  { # here we standardize outputs to the z-scoore
    # sens.list[[k]]<-tell(sobolF,
    #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
    sens.fofemS.list[[k]]<-tell(sobolF,results.fofem.sa[,responseS.vars[k]])
    # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
    rownames(sens.fofemS.list[[k]]$S)<-names(corr.sampF.vals)
    my.plot.sobol(n.var = 6,sobol.obj = sens.fofemS.list[[k]],
                  main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseS.vars[k]))
  }
  
  for(k in 1:length(responseS.vars))
  { # here we standardize outputs to the z-scoore
    # sens.list[[k]]<-tell(sobolF,
    #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
    sens.fofemNoCorrS.list[[k]]<-tell(sobolF,resultsNoCorr.fofem.sa[,responseS.vars[k]])
    # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
    rownames(sens.fofemNoCorrS.list[[k]]$S)<-names(corr.sampF.vals)
    my.plot.sobol(n.var = 6,sobol.obj = sens.fofemNoCorrS.list[[k]],
                  main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseS.vars[k]))
  }
  
# now fofem prcc
fofem2.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[1]][[cur.evt]],nreps,fbLoadNames.df,
                                       all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")
fofem2NoCorr.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[2]][[cur.evt]],nreps,fbLoadNames.df,
                                       all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")

results2.fofem.sa<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",fuel.loads=fofem2.fuel.loads,
                                      env.in.name="sample_consume_input.csv",envfilename="EnvInputSA.csv",
                                      fofem.filename="FOFEM_FlamingSAInput1.csv",newwd="fofem",oldwd="../")
results2NoCorr.fofem.sa<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",fuel.loads=fofem2NoCorr.fuel.loads,
                                            env.in.name="sample_consume_input.csv",envfilename="EnvInputSA.csv",
                                            fofem.filename="FOFEM_FlamingSAInput1.csv",newwd="fofem",oldwd="../")
fofemF.prcc<-list()
fofemNoCorrF.prcc<-list()
for(k in 1:length(responseF.vars))
  fofemF.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=results2.fofem.sa[,responseF.vars[k]],
                        rank=TRUE,nboot=1000)
for(k in 1:length(responseF.vars))
  fofemNoCorrF.prcc[[k]]<-pcc(sens.mats.flame.list[[2]][[cur.evt]],y=results2NoCorr.fofem.sa[,responseF.vars[k]],
                        rank=TRUE,nboot=1000)
  
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(responseF.vars))
  my.plot.prcc(n.var=6,prcc.obj = fofemF.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseF.vars[k]))
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(responseF.vars))
  my.plot.prcc(n.var=6,prcc.obj = fofemNoCorrF.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEMNoCorr","output:",responseF.vars[k]))

fofemS.prcc<-list()
for(k in 1:length(responseF.vars))
  fofemS.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=results2.fofem.sa[,responseS.vars[k]],
                        rank=TRUE,nboot=1000)
fofemNoCorrS.prcc<-list()
for(k in 1:length(responseF.vars))
  fofemNoCorrS.prcc[[k]]<-pcc(sens.mats.flame.list[[2]][[cur.evt]],y=results2NoCorr.fofem.sa[,responseS.vars[k]],
                        rank=TRUE,nboot=1000)
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(responseS.vars))
  my.plot.prcc(n.var=6,prcc.obj = fofemS.prcc[[k]],
                 main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseS.vars[k]))

par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(responseS.vars))
  my.plot.prcc(n.var=6,prcc.obj = fofemNoCorrS.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEMNoCorr","output:",responseS.vars[k]))

# try combining the fofem flaming and smoldering for comparison to consume
fofem2.total<-results2.fofem.sa[,responseF.vars]+results2.fofem.sa[,responseS.vars]
fofem.total<-results.fofem.sa[,responseF.vars]+results.fofem.sa[,responseS.vars]

fofemT.prcc<-list()
for(k in 1:ncol(fofem2.total))
  fofemT.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=fofem2.total[,k],
                        rank=TRUE,nboot=1000)
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:ncol(fofem2.total))
{
  plot(1:length(flaming3.id),fofemT.prcc[[k]]$PRCC[,1],ylim=c(0,1),
       axes=FALSE,ylab="Partial rank correlation coefficient",xlab="",pch=16,
       main=paste("EVT:",cur.evt,"FOFEM","output:",responseF.vars[k]))
  segments(x0 = 1:length(flaming3.id),y0=fofemT.prcc[[k]]$PRCC[,4],
           x1 = 1:length(flaming3.id),y1=fofemT.prcc[[k]]$PRCC[,5])
  axis(2)
  axis(1,at=1:length(flaming3.id),labels=names(sens.mats.flame.list[[1]][[cur.evt]]),las=2)
}

fofemT.sobol<-list()
for(k in 1:ncol(fofem2.total))
  fofemT.sobol[[k]]<-tell(sobolF,fofem.total[,k])

par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:ncol(fofem2.total))
{
  plot(1:length(flaming3.id),fofemT.prcc[[k]]$PRCC[,1],ylim=c(0,1),
       axes=FALSE,ylab="Partial rank correlation coefficient",xlab="",pch=16,
       main=paste("EVT:",cur.evt,"FOFEM","output:",responseF.vars[k]))
  segments(x0 = 1:length(flaming3.id),y0=fofemT.prcc[[k]]$PRCC[,4],
           x1 = 1:length(flaming3.id),y1=fofemT.prcc[[k]]$PRCC[,5])
  axis(2)
  axis(1,at=1:length(flaming3.id),labels=names(sens.mats.flame.list[[1]][[cur.evt]]),las=2)
}



