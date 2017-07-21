###########
# generate loadings file with baseline fbs for OAT SA,
# with loadings based either on database distribution or 
# based on reasonable min/max
###########
# requires SA file for a given EVT
source("../Functions/SensitivityAnalysis_FN.R")
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

# this is current FCCS database of fuelbeds. It is from these we will be 
# finding baseline conditions for each evt
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",stringsAsFactors=TRUE) # skip the header line

# We will now populate a new loadings file, first with the baseline values for
# the fuelbeds that represent the EVTs of interest, and sampling from ranges
# generated above. Requires mapping database category names to fccs category names

fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdated.csv",stringsAsFactors = FALSE,sep=",",header=TRUE)
col.matches<-match(names(AllLoads.df),fbLoadNames.df[,3]) # gives matches wrt the rows of fbLoadNames
# so the ith entry in this vector is the row in fbLoadsNames.df that corresponds to the ith column of AllLoads.df
head(AllLoads.df[,fbLoadNames.df[col.matches[!is.na(col.matches)],3]]) # these are the column names in AllLoads that correspond to the matches
names(AllLoads.df)[col.matches[!is.na(col.matches)]]
fbLoadNames.df[col.matches[!is.na(col.matches)],]
# also need to match the range.vals column 1 to the fbLoadNames column 1
range.matches<-match(range.vals[[1]][[2]][,1],fbLoadNames.df[,3])
fbLoadNames.df[range.matches[!is.na(range.matches)],]
range.vals[[1]][[2]][!is.na(range.matches),1]
range.vals[[1]][[2]]$ConsumeNames<-NA # create a new column the gives the Consume column name corresponding to the database column names
range.vals[[1]][[2]]$ConsumeNames[!is.na(range.matches)]<-fbLoadNames.df[range.matches[!is.na(range.matches)],1]
# so when we have a fuel column in AllLoads we have to make sure it corresponds
# to the correct FCCS column name required for Consume

new.loads.file<-all.fbs[all.fbs$fuelbed_number==90,]
nreps=20
which.change<-which(!is.na(range.vals[[1]][[2]][,2])) # for which of our categories do we have sufficient coverage?
which2.change<-which.change[!is.na(fbLoadNames.df[range.matches[which.change],1])] # only keep those that have consume names
for(i in 1:(nreps*length(which2.change))) # 
  new.loads.file<-rbind(new.loads.file,all.fbs[all.fbs$fuelbed_number==90,])
# and next we replace these baseline values with randomly drawn values based on the range.vals matrix
# first figure out which actually have ranges
new.loads.file$filename<-as.character(new.loads.file$filename)
for(i in 1:length(which2.change))
{
  # index skipping the first row
  new.loads.file[(i*nreps-18):(i*nreps+1),fbLoadNames.df[range.matches[which2.change[i]],1]]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
  new.loads.file$filename[(i*nreps-18):(i*nreps+1)]<-rep(fbLoadNames.df[range.matches[which2.change[i]],1],nreps)
  new.loads.file$fuelbed_number[(i*nreps-18):(i*nreps+1)]<-new.loads.file$fuelbed_number[1]*1000+(i*nreps-19):(i*nreps)
}
#*****Need to convert from Mg/ha to tons/acre for consume runs, then back again**********
#*******depth probably from cm to in**************
# 1 Mg = 1.10231 tons; 1 ton = 0.907185 Mg; 1 ha = 2.47105 acres; 1 acre = 0.404686 ha
outfilename<-"FuelLoadInputEVT660Try1.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS 3.0","GeneratorVersion=3.0.0","DateCreated=07/11/2016"),ncol=3)# didn't like today's date
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

