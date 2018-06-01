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

flaming1.id<-c(1,2,6,18,20,22,23,26)
#  "X100hr_loading_Mgha" "X10hr_loading_Mgha"  "X1hr_loading_Mgha"   "herb_loading_Mgha"  
# "lichen_loading_Mgha" "litter_loading_Mgha" "moss_loading_Mgha"   "shrub_loading_Mgha" 
flaming2.id<-c(15,18,20,22,23,26) # assuming fwd is 1-hr+10-hr+100-hr
#"fwd_loading_Mgha"    "herb_loading_Mgha"   "lichen_loading_Mgha" "litter_loading_Mgha"
#"moss_loading_Mgha"   "shrub_loading_Mgha" 
flaming3.id<-c(1,2,6,18,22,26) # remove lichen and moss because those data seem to be rare
#"X100hr_loading_Mgha" "X10hr_loading_Mgha"  "X1hr_loading_Mgha"   "herb_loading_Mgha"  
#"litter_loading_Mgha" "shrub_loading_Mgha" 

cur.evt<-58 # example EVT candidate from database paper, with sufficient coverage of flaming fuel types,
# using flaming3.id
# need to find one for smoldering
###############
# flaming first
N.samp<-5000 # number of random samples for SA

source("../Functions/SimHurdleDist_FN.R")
source("../Functions/SensitivitySampleWithCorr_FN.R")
# samples from entire distribution, creating a data matrix with
# correlation structure that matches that for the database for that EVT--the matrix X*
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

# now create the sobol object, which rearranges X* to be appropriate for variance partitioning algorithm
library(sensitivity)
sobolF<-sobolEff(X1=sens.mats.flame.list[[1]][[cur.evt]][1:(N.samp/2),],
                 X2=sens.mats.flame.list[[1]][[cur.evt]][(N.samp/2+1):N.samp,],nboot = 1000)
#compare the Sobol correlation matrx to the original sample
cor(sobolF$X)
cor(sens.mats.flame.list[[1]][[cur.evt]])


source("GenerateConsumeLoadingsFileCorrSamp.R") #This function actually switches between consume and fofem to generate input file. 
cur.evt.num<-evt.vals[cur.evt]
base.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt.num]# identify the base fb associated with this evt group
base.fb<-unique(base.fb[!is.na(base.fb)])

corr.sampF.vals<-data.frame(sobolF$X)
names(corr.sampF.vals)<-names(sens.mats.flame.list[[1]][[cur.evt]])
###############
# first CONSUME
# create a dataframe that combines baseline values with sobol-sampled ones
consume.fuel.loads<-GenerateFuelInput.fn(corr.sampF.vals,nreps,
                                              fbLoadNames.df,all.fbs,base.fb,change.units=T)
# define the filename for the consume input file
infilename<-"FuelLoadInputSA.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS   3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)
# didn't like today's date
# write the file with the proper header information
write.table(loadInFile.head,file=infilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# Now append that file with the generated loadings
write.table(consume.fuel.loads,file=infilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)

# and create an environmental input file that is just repeated for all rows of the fuels file
# This is baseline environmental conditions. Consider including in SA
env.in<-read.csv("sample_consume_input.csv") ###Note, if this file changes so should the FOFEM input file moistures
new.env.in<-env.in[1,]
for(m in 2:nrow(consume.fuel.loads))
{
  new.env.in<-rbind(new.env.in,env.in[1,])
}
new.env.in$fuelbeds<-consume.fuel.loads$fuelbed_number
envfilename<-"EnvInputSA.csv"
write.table(new.env.in,file=envfilename,row.names=FALSE,sep=",")

# and now we call consume
# first format the system call
system.call<-paste("python consume_batch.py natural",envfilename,  "-f", infilename)
try1<-try(system(system.call)) # tells R to execute this system call in the working directory
# Then we readin the results, and calculate the Sobol sensitivity indices

if(try1==1)
{
  print("Failed consume call")
}
if(try1==0) # then consume was successful, and we can readin the output file
{
  results.sa<-read.csv("consume_results.csv") # writes to this file every time, replacing previous    
}

# calculate sobol indices for selected model predictions
response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions","PM25.Emissions")
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
sens.list<-list()
for(k in 1:length(response.vars))
{ # here we standardize outputs to the z-score
  # sens.list[[k]]<-tell(sobolF,
  #                      (results.sa[,response.vars[k]]-mean(results.sa[,response.vars[k]]))/sd(results.sa[,response.vars[k]]))
  sens.list[[k]]<-tell(sobolF,results.sa[,response.vars[k]]) # for non-centered
    
  rownames(sens.list[[k]]$S)<-names(corr.sampF.vals)
  plot(1:nrow(sens.list[[k]]$S),sens.list[[k]]$S$original,axes=FALSE,ylim=c(-.15,1),pch=16,
       xlab="",ylab="Sensitivity index",
       main=paste("EVT:",cur.evt,"FB",base.fb,"output:",response.vars[k]))#las=2)
  
  axis(1,at=1:nrow(sens.list[[k]]$S),labels=rownames(sens.list[[k]]$S),
       las=2,cex.lab=0.9,tick = F)
  segments(x0 = 1:nrow(sens.list[[k]]$S),x1=1:nrow(sens.list[[k]]$S),
           y0=sens.list[[k]]$S$`min. c.i.`,y1=sens.list[[k]]$S$`max. c.i.`)
  axis(2,las=1)
  abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  box()
}

# now the prcc
# we need to generate a new loadings file with just the X* matrix, unmodified by Sobol
consume2.fuel.loads<-GenerateFuelInput.fn(sens.mats.flame.list[[1]][[cur.evt]],nreps,
                                         fbLoadNames.df,all.fbs,base.fb,change.units=T)
infilename<-"FuelLoadInput2SA.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS   3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)
# didn't like today's date
# write the file with the proper header information
write.table(loadInFile.head,file=infilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# Now append that file with the generated loadings
write.table(consume2.fuel.loads,file=infilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)

# and create an environmental input file that is just repeated for all rows of the fuels file
# This is baseline environmental conditions. Consider including in SA
env.in<-read.csv("sample_consume_input.csv") ###Note, if this file changes so should the FOFEM input file moistures
new2.env.in<-env.in[1,]
for(m in 2:nrow(consume2.fuel.loads))
{
  new2.env.in<-rbind(new2.env.in,env.in[1,])
}
new2.env.in$fuelbeds<-consume2.fuel.loads$fuelbed_number
envfilename<-"Env2InputSA.csv"
write.table(new2.env.in,file=envfilename,row.names=FALSE,sep=",")

# and now we call consume
# first format the system call
system.call<-paste("python consume_batch.py natural",envfilename,  "-f", infilename)
try1<-try(system(system.call)) # tells R to execute this system call in the working directory
# Then we readin the results, and calculate the Sobol sensitivity indices

if(try1==1)
{
  print("Failed consume call")
}
if(try1==0) # then consume was successful, and we can readin the output file
{
  results2.sa<-read.csv("consume_results.csv") # writes to this file every time, replacing previous    
}

consumeF.prcc<-list()
for(k in 1:length(response.vars))
  consumeF.prcc[[k]]<-pcc(sens.mats.flame.list[[1]][[cur.evt]],y=results2.sa[,response.vars[k]],
                          rank=TRUE,nboot=1000)
par(mfrow=c(2,2),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0),las=1)
for(k in 1:length(response.vars))
{
  plot(1:length(flaming3.id),consumeF.prcc[[k]]$PRCC[,1],ylim=c(0,1),
       axes=FALSE,ylab="Partial rank correlation coefficient",xlab="",pch=16)
  segments(x0 = 1:length(flaming3.id),y0=consumeF.prcc[[k]]$PRCC[,4],
           x1 = 1:length(flaming3.id),y1=consumeF.prcc[[k]]$PRCC[,5])
  axis(2)
  axis(1,at=1:length(flaming3.id),labels=names(sens.mats.flame.list[[1]][[cur.evt]]),las=2)
}

# Start looking at graphing the emissions and determining uncertainty intervals

boxplot(results2.sa[,response.vars[k]])

#########
# now FOFEM

