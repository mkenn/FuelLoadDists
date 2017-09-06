#####Functions#####
#install fitdistrplus into console 

library(fitdistrplus)
###########
# 1. Read in the data
# this is the old data base
#data.file<-read.csv("../FuelLoadDists/Data/loadingsByEVTGroup_20170328.csv") #substitute filepath and name for local system
# this is the current database
data.file<-read.csv("../FuelLoadDists/Data/loadingsByEVTGroup_20170518.csv") #substitute filepath and name for local system
data.file<-read.csv("../Data/loadingsByEVTGroup_20170518.csv") #substitute filepath and name for local system
# 2. Source the function scripts; works if all functions in 1 directory
file.sources<-list.files("../FuelLoadDists/Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../FuelLoadDists/Functions/",x,sep=""))

file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))

sapply(file.sources,FUN="source")
#####
#EVT tally function 
# arguments: 
# return objects:
# MK: assign these to objects so the results are stored in the workspace
# here and for all function calls
EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd_FINAL", write.file = TRUE)
EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd", write.file = TRUE)
#####
#Loading Tally Function 

loadingTallies <- loading.tally.fn(data.file, start.col = 12, write.file = TRUE)
#####
#Sensitivity Analysis Function 
#EVT 624 as example

sensitivityAnalysis <- samo.fn(data.file, evts = EVTTallies$evt.min_tally, start.col = 12)
#####
#Distribution Fitting Function
#EVT 624 is shown as example
#Minimum plot 100 

evt.vals<-EVTTallies[[1]][EVTTallies[[1]][,2]>100,1]

distributionFitting <- dist.fit.fn(data.file, evts = EVTTallies$evt.min_tally, start.col = 12, write.file = FALSE, min.plot = 100)
distributionFitting <- dist.fit.fn(data.file, evts = 624, start.col = 12, write.file = FALSE, min.plot = 100)
distributionFitting <- dist.fit.fn(data.file, evts = evt.vals, start.col = 12, write.file = FALSE, min.plot = 100)
distributionFittingNoOut <- dist.fitNoOut.fn(data.file, evts = evt.vals, start.col = 12, write.file = FALSE, min.plot = 100)
distributionRanking<-distfit.rank.fn(evts = 624,DistFitSum = distributionFitting)
distributionRanking<-distfit.rank.fn(evts = evt.vals,DistFitSum = distributionFitting)
distributionRankingNoOut<-distfit.rank.fn(evts = evt.vals,DistFitSum = distributionFittingNoOut)



#####
#Distribution Fitting Graphing Function
#EVT 624 as example 
#Minimum plot 100 

corr.vals<-corrpairs.fn(data.file = data.file,start.col = 12,evts = evt.vals,evt.col = "LFEVTGroupCd",min.co = 30)
corr.all.vals<-corrpairs.fn(data.file = data.file,start.col = 12,evts = NA,evt.col = "LFEVTGroupCd",min.co = 30)


dist.fit.graph.fn(data.file, evts = 624, start.col = 12, min.plot = 100)
# error in running, need to update function with start.col indexing 
