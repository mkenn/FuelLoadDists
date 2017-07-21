##########
# global SA with independent sampling based on database distributions
##########
source("../Functions/SensitivityAnalysis_FN.R")
library(sensitivity)
# this function returns the values for designated lower and upper quartiles and q3, 
# as well as min and max, for all fuel loading categories that meet
#  a given threshold for n for a given EVT
AllLoads.df<-read.csv("../Data/loadingsByEVTGroup_20170518.csv")
cur.evt<-660 # so this function will return a list of the same length of this vector,
# with the quantile files for each evt
# match fb 90 to cur.evt 660 ***CHECK***
#ur.evt<-unique(AllLoads.df$LFEVTGroupCd) # this identifies ALL of the unique EVTs in the database
#cur.evt.fccsBase<- # here we're goint want a 2-column file that maps EVT groups to FCCS fuelbeds
range.vals<-samo.fn(data.file=AllLoads.df,evts=cur.evt,start.col=12,cur.cols=c(12:ncol(AllLoads.df)),evt.col="LFEVTGroupCd",min.n=30,q.lower=0.1,q.upper=0.9,write.file=FALSE,file.name="SensitivityAnalysis_")
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",stringsAsFactors=TRUE) # skip the header line

# We will now populate a new loadings file, first with the baseline values for
# the fuelbeds that represent the EVTs of interest, and sampling from ranges
# generated above. Requires mapping database category names to fccs category names

fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdated.csv",stringsAsFactors = FALSE,sep=",",header=TRUE)
col.matches<-match(names(AllLoads.df),fbLoadNames.df[,3]) # gives matches wrt the rows of fbLoadNames
range.matches<-match(range.vals[[1]][[2]][,1],fbLoadNames.df[,3])
range.vals[[1]][[2]]$ConsumeNames<-NA # create a new column the gives the Consume column name corresponding to the database column names
range.vals[[1]][[2]]$ConsumeNames[!is.na(range.matches)]<-fbLoadNames.df[range.matches[!is.na(range.matches)],1]

nreps=100
which.change<-which(!is.na(range.vals[[1]][[2]][,2])) # for which of our categories do we have sufficient coverage?
which2.change<-which.change[!is.na(fbLoadNames.df[range.matches[which.change],1])] # only keep those that have consume names

myX1.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change))
for(i in 1:length(which2.change))
  myX1.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
myX2.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change))
for(i in 1:length(which2.change))
  myX2.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])

sobol1<-sobolEff(X1=myX1.sobol,X2=myX2.sobol,nboot = 1000)

new.loads.file<-all.fbs[all.fbs$fuelbed_number==90,]
for(i in 2:(nrow(sobol1$X))) # 
  new.loads.file<-rbind(new.loads.file,all.fbs[all.fbs$fuelbed_number==90,])
# and next we replace these baseline values with randomly drawn values based on the range.vals matrix
# first figure out which actually have ranges
new.loads.file$filename<-as.character(new.loads.file$filename)

for(i in 1:length(which2.change))
{
  # index skipping the first row
  new.loads.file[(1:(nrow(sobol1$X))),fbLoadNames.df[range.matches[which2.change[i]],1]]<-sobol1$X[,i]
}
new.loads.file$filename[(1:(nrow(sobol1$X)))]<-rep("sobolSamplefb",nrow(sobol1$X))
new.loads.file$fuelbed_number[(1:(nrow(sobol1$X)))]<-9000+1:nrow(sobol1$X)

outfilename<-"FuelLoadInputEVT660Try2.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS 3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)# didn't like today's date
# write the file with the proper header information
write.table(loadInFile.head,file=outfilename,row.names=FALSE,col.names=FALSE,sep=",")
# Now append that file with the generated loadings
write.table(new.loads.file,file=outfilename,row.names=FALSE,append=TRUE,sep=",")

########
# now the environmental inputs
########
env.in<-read.csv("sample_consume_input.csv") #####******* Need to change units to metric!?**********
new.env.in<-env.in[1,]
for(i in 2:nrow(new.loads.file))
{
  new.env.in<-rbind(new.env.in,env.in[1,])
}
new.env.in$fuelbeds<-new.loads.file$fuelbed_number
envoutfilename<-"EnvInputEVT660Try1.csv"
write.table(new.env.in,file=envoutfilename,row.names=FALSE,sep=",")
##########
# now run consume

system.call<-paste("python consume_batch.py natural",envoutfilename,  "-f", outfilename)
# calls python to run consume_batch.py, then indicate natural (v. activity fuels), 
# then the patch and filename to the environmental input file, then
# the -f flag, and the path and filename to the loadings input file
# all filepaths assume start at working directory
system(system.call) # tells R to execture this system call in the working directory
# writes the results file to the current directory, ALWAYS called consume_results.csv
# next step move the file to a results directory with a unique name
# The /Y flag suppresses prompt to confirm overwrite of existing file.
# be careful! 
# Errors from dos prompt:
# Function "_load_emissions_factor_eqid()". Error with fuelbed id: 90
# Function "_load_emissions_factor_eqid()". Error with fuelbed id: 90
# Error: emissions database does not contain equation id for fuelbed 90
# Error: emissions database does not contain equation id for fuelbed 90
# list index out of range
# The sampling is working, and at a glance the file looks fine

resultsfilename<-"ConsumeResultsEVT660Try3.csv"
move.call<-paste("move /Y consume_results.csv SAResults\\",resultsfilename,sep="")
system(move.call) # need to debug. system call didn't work, but cutting and pasting to dos prompt did

results.sa<-read.csv(paste("SAResults/",resultsfilename,sep=""))
# next step is to match loading category being modified to change in desired output
# for OAT SA. This requires matching the loadings and results files, 
# which should match row to row
head(results.sa)

tell(sobol1,y = results.sa$PM.Emissions) # updates the sobol object with the output
par(mar=c(8,3,0.5,0.5),mgp=c(2,0.5,0))
plot(sobol1,axes=FALSE)
axis(1,at=1:10,labels=fbLoadNames.df[range.matches[which2.change],1],las=2)
axis(2)
