#############
# conduct sensitiviy analysis of
# CONSUME and FOFEM
# using both Sobol variance partitioning and
# partial rank correlation coefficient
# and using correlated input variables
#############

load("../DistFittingRProj/Workspaces/HurdleCustomFitsNoZeroWNoOut.RData")
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
data.file$X10Khr_loading_Mgha<-data.file$X10KhrR_loading_Mgha+data.file$X10KhrS_loading_Mgha
data.file$X1Khr_loading_Mgha<-data.file$X1KhrR_loading_Mgha+data.file$X1KhrS_loading_Mgha

data.file$cwd_loading_Mgha<-data.file$X10Khr_loading_Mgha+data.file$X1Khr_loading_Mgha+data.file$GT10KhrR_loading_Mgha+data.file$GT10KhrS_loading_Mgha


data.file$fwd_loading_Mgha<-data.file$X1hr_loading_Mgha+data.file$X10hr_loading_Mgha+data.file$X100hr_loading_Mgha

# now choose variable groupings for flaming and smoldering
smolder1.id<-c(4,5,8,9,16,17)
names(data.file)[smolder1.id+start.col-1]
# "X10KhrR_loading_Mgha"  "X10KhrS_loading_Mgha"  "X1KhrR_loading_Mgha"   "X1KhrS_loading_Mgha"  
# "GT10KhrR_loading_Mgha" "GT10KhrS_loading_Mgha"
smolder2.id<-c(3,7,16,17) # because we don't seem to have a total column for >10K
# "X10Khr_loading_Mgha"   "X1Khr_loading_Mgha"    "GT10KhrR_loading_Mgha" "GT10KhrS_loading_Mgha"

smolder3.id<-

flaming1.id<-c(1,2,6,18,20,22,23,26)
#  "X100hr_loading_Mgha" "X10hr_loading_Mgha"  "X1hr_loading_Mgha"   "herb_loading_Mgha"  
# "lichen_loading_Mgha" "litter_loading_Mgha" "moss_loading_Mgha"   "shrub_loading_Mgha" 
flaming2.id<-c(15,18,20,22,23,26) # assuming fwd is 1-hr+10-hr+100-hr
#"fwd_loading_Mgha"    "herb_loading_Mgha"   "lichen_loading_Mgha" "litter_loading_Mgha"
#"moss_loading_Mgha"   "shrub_loading_Mgha" 
flaming3.id<-c(1,2,6,18,22,26) # remove lichen and moss because those data seem to be rare
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
# using flaming3.id
# need to find one for smoldering
###############
# flaming first
N.samp<-100 # number of random samples for SA, set low for now for efficiency and testing

source("../Functions/SimHurdleDist_FN.R")
source("../Functions/SensitivitySampleWithCorr_FN.R")
# samples from entire distribution, creating a data matrix with
# correlation structure that matches that for the database for that EVT--the matrix X*
# also includes the uncorrelated matrix X for comparison
sens.mats.flame.list<-corr.sa.fn(data.file,fuel.ids=flaming3.id,complete.case=TRUE,
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
#####
# independent sample from full distribution
# sens.mats.flameNoCorr.list<-corr.sa.fn(data.file,fuel.ids=flaming3.id,complete.case=TRUE,
#                                  min.co.occur=30,
#                                  evts=evt.vals,EVTCol=EVTCol,start.col=start.col,n.samp=N.samp,
#                                  rankObj=distributionCustomRankingHurdleNOut,
#                                  fitObj=distributionCustomFittingHurdleNOut,use.corr = FALSE)
##### 
# # now create the sobol object, which rearranges X* to be appropriate for variance partitioning algorithm
library(sensitivity)
sobolF<-sobolEff(X1=sens.mats.flame.list[[1]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.flame.list[[1]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)
sobolFNoCorr<-sobolEff(X1=sens.mats.flame.list[[2]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.flame.list[[2]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)

#compare the Sobol correlation matrx to the original sample
# cor(sobolF$X)
# cor(sens.mats.flame.list[[1]][[cur.evt]])


source("GenerateConsumeLoadingsFileCorrSamp.R") #This function actually switches between consume and fofem to generate input file. 
cur.evt.num<-evt.vals[cur.evt]
base.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt.num]# identify the base fb associated with this evt group
base.fb<-unique(base.fb[!is.na(base.fb)])

corr.sampF.vals<-data.frame(sobolF$X)
names(corr.sampF.vals)<-names(sens.mats.flame.list[[1]][[cur.evt]])
Nocorr.sampF.vals<-data.frame(sobolFNoCorr$X)
names(Nocorr.sampF.vals)<-names(sens.mats.flame.list[[1]][[cur.evt]])
###############
# first CONSUME
# create a dataframe that combines baseline values with sobol-sampled ones
consume.fuel.loads<-GenerateFuelInput.fn(corr.sampF.vals,nreps,
                                              fbLoadNames.df,all.fbs,base.fb,change.units=T)
consumeNoCorr.fuel.loads<-GenerateFuelInput.fn(Nocorr.sampF.vals,nreps,
                                         fbLoadNames.df,all.fbs,base.fb,change.units=T)

source("CallModsForSA.R") #This function actually switches between consume and fofem to generate input file. 
consume.sobol.sa.results<-call.emissions.mods(fuel.loads = consume.fuel.loads)
consume.sobolNoCorr.sa.results<-call.emissions.mods(fuel.loads = consumeNoCorr.fuel.loads)
###########
# define the filename for the consume input file
# infilename<-"FuelLoadInputSA.csv"
# loadInFile.head<-matrix(c("GeneratorName=FCCS   3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)
# # didn't like today's date
# # write the file with the proper header information
# write.table(loadInFile.head,file=infilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# # Now append that file with the generated loadings
# write.table(consume.fuel.loads,file=infilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)
# 
# # and create an environmental input file that is just repeated for all rows of the fuels file
# # This is baseline environmental conditions. Consider including in SA
# env.in<-read.csv("sample_consume_input.csv") ###Note, if this file changes so should the FOFEM input file moistures
# new.env.in<-env.in[1,]
# for(m in 2:nrow(consume.fuel.loads))
# {
#   new.env.in<-rbind(new.env.in,env.in[1,])
# }
# new.env.in$fuelbeds<-consume.fuel.loads$fuelbed_number
# envfilename<-"EnvInputSA.csv"
# write.table(new.env.in,file=envfilename,row.names=FALSE,sep=",")
# 
# # and now we call consume
# # first format the system call
# system.call<-paste("python consume_batch.py natural",envfilename,  "-f", infilename)
# try1<-try(system(system.call)) # tells R to execute this system call in the working directory
# # Then we readin the results, and calculate the Sobol sensitivity indices
# 
# if(try1==1)
# {
#   print("Failed consume call")
# }
# if(try1==0) # then consume was successful, and we can readin the output file
# {
#   results.sa<-read.csv("consume_results.csv") # writes to this file every time, replacing previous    
# }
#########
source("PlotSobolAndPRCCResults.R") # functions to graph Sobol and PRCC results
# calculate sobol indices for selected model predictions
response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions","PM25.Emissions")
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
##########  
  # plot(1:nrow(sens.list[[k]]$S),sens.list[[k]]$S$original,axes=FALSE,ylim=c(-.15,1),pch=16,
  #      xlab="",ylab="Sensitivity index",
  #      main=paste("EVT:",cur.evt,"FB",base.fb,"output:",response.vars[k]))#las=2)
  # 
  # axis(1,at=1:nrow(sens.list[[k]]$S),labels=rownames(sens.list[[k]]$S),
  #      las=2,cex.lab=0.9,tick = F)
  # segments(x0 = 1:nrow(sens.list[[k]]$S),x1=1:nrow(sens.list[[k]]$S),
  #          y0=sens.list[[k]]$S$`min. c.i.`,y1=sens.list[[k]]$S$`max. c.i.`)
  # axis(2,las=1)
  # abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  # box()
  ##########
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

#########
# infilename<-"FuelLoadInput2SA.csv"
# loadInFile.head<-matrix(c("GeneratorName=FCCS   3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)
# # didn't like today's date
# # write the file with the proper header information
# write.table(loadInFile.head,file=infilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# # Now append that file with the generated loadings
# write.table(consume2.fuel.loads,file=infilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)
# 
# # and create an environmental input file that is just repeated for all rows of the fuels file
# # This is baseline environmental conditions. Consider including in SA
# env.in<-read.csv("sample_consume_input.csv") ###Note, if this file changes so should the FOFEM input file moistures
# new2.env.in<-env.in[1,]
# for(m in 2:nrow(consume2.fuel.loads))
# {
#   new2.env.in<-rbind(new2.env.in,env.in[1,])
# }
# new2.env.in$fuelbeds<-consume2.fuel.loads$fuelbed_number
# envfilename<-"Env2InputSA.csv"
# write.table(new2.env.in,file=envfilename,row.names=FALSE,sep=",")
# 
# # and now we call consume
# # first format the system call
# system.call<-paste("python consume_batch.py natural",envfilename,  "-f", infilename)
# try1<-try(system(system.call)) # tells R to execute this system call in the working directory
# # Then we readin the results, and calculate the Sobol sensitivity indices
# 
# if(try1==1)
# {
#   print("Failed consume call")
# }
# if(try1==0) # then consume was successful, and we can readin the output file
# {
#   results2.sa<-read.csv("consume_results.csv") # writes to this file every time, replacing previous    
# }
###########

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
{
  my.plot.prcc(n.var=length(flaming3.id),prcc.obj = consumeF.prcc[[k]],
               main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","output:",response.vars[k]))
  
  #   plot(1:length(flaming3.id),consumeF.prcc[[k]]$PRCC[,1],ylim=c(0,1),
  #      axes=FALSE,ylab="Partial rank correlation coefficient",xlab="",pch=16,
  #      main=paste("EVT:",cur.evt,"CONSUME","output:",responseF.vars[k]))
  # segments(x0 = 1:length(flaming3.id),y0=consumeF.prcc[[k]]$PRCC[,4],
  #          x1 = 1:length(flaming3.id),y1=consumeF.prcc[[k]]$PRCC[,5])
  # axis(2)
  # axis(1,at=1:length(flaming3.id),labels=names(sens.mats.flame.list[[1]][[cur.evt]]),las=2)
}

# Start looking at graphing the emissions and determining uncertainty intervals

boxplot(results2.sa[,response.vars[k]]) # CO2 takes the cake!

#########
# now FOFEM
setwd("fofem")
base.fofem<-read.csv("FOFEM_6_May12013_MCKUpdate2.csv") 
# note, this file has the header--our final file 
# note also this has the fuel moistures matched to consume env input as
# best we can. If that changes, so should this
base.fofem.use<-base.fofem[1,]

fofem.fuel.loads<-GenerateFuelInput.fn(corr.sampF.vals,nreps,fbLoadNames.df,
                                            all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")
fofem.filename<-"FOFEM_FlamingSAInput1.csv"
write("#1k-SizeClass",file=fofem.filename) # switch header to indicate 
write.table(fofem.fuel.loads,file=fofem.filename,append = TRUE,sep=",",row.names = FALSE,col.names = FALSE)
system.call<-paste("FOF_GUI C",fofem.filename,"ConE-Out.txt ConE-run.txt ConE-Err.txt H", sep=" ")
system(system.call) # tells R to execute this system call in the working directory

check.fofem<-scan("ConE-Err.txt",what = character())# if there is no error recorded, then this file will be empty

if(length(check.fofem)==0) # if there is no error recorded, then this file will be empty
{
  results.fofem.sa<-read.csv("ConE-Out.txt") # writes to this file every time, replacing previous results
  # note: FOFEM divides emissions into flaming and smoldering, which consume doesn't seem
  # to do. Will complicate comparisons. 
  # for flaming:
  responseF.vars<-c("COF","CO2F","PM25F")
  
  # for smoldering:
  responseS.vars<-c("COS","CO2S","PM25S")
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
  sens.list<-list()
  for(k in 1:length(responseF.vars))
  { # here we standardize outputs to the z-scoore
     sens.list[[k]]<-tell(sobolF,
                          (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
   # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
    rownames(sens.list[[k]]$S)<-names(corr.sampF.vals)
    my.plot.sobol(n.var = 6,sobol.obj = sens.list[[k]],
                  main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseF.vars[k]))
  }
  
}

# now fofem prcc
fofem.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[1]][[cur.evt]],nreps,fbLoadNames.df,
                                       all.fbs,base.fb,base.fofem=base.fofem.use,change.units=T,mod="F")
fofem.filename<-"FOFEM_FlamingSAInput2.csv"
write("#1k-SizeClass",file=fofem.filename) # switch header to indicate 
write.table(fofem.fuel.loads,file=fofem.filename,append = TRUE,sep=",",row.names = FALSE,col.names = FALSE)
system.call<-paste("FOF_GUI C",fofem.filename,"ConE-Out.txt ConE-run.txt ConE-Err.txt H", sep=" ")
system(system.call) # tells R to execute this system call in the working directory

check.fofem<-scan("ConE-Err.txt",what = character())# if there is no error recorded, then this file will be empty

if(length(check.fofem)==0)
{
  results2.fofem.sa<-read.csv("ConE-Out.txt") # writes to this file every time, replacing previous results
  fofemF.prcc<-list()
  for(k in 1:length(responseF.vars))
    fofemF.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=results2.fofem.sa[,responseF.vars[k]],
                          rank=TRUE,nboot=1000)
    
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
  for(k in 1:length(responseF.vars))
  {
    my.plot.prcc(n.var=6,prcc.obj = fofemF.prcc[[k]],
                 main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseF.vars[k]))
  }
  
  fofemS.prcc<-list()
  for(k in 1:length(responseF.vars))
    fofemS.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=results2.fofem.sa[,responseS.vars[k]],
                          rank=TRUE,nboot=1000)
  par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
  for(k in 1:length(responseF.vars))
  {
    my.plot.prcc(n.var=6,prcc.obj = fofemS.prcc[[k]],
                 main.txt=paste("EVT:",evt.vals[cur.evt],"FOFEM","output:",responseS.vars[k]))
  }
  
}

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



