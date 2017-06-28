#####Functions#####
#install fitdistrplus into console 

library(fitdistrplus)
###########
# 1. Read in the data
data.file<-read.csv("../FuelLoadDists/Data/loadingsByEVTGroup_20170328.csv") #substitute filepath and name for local system
# 2. Source the function scripts; works if all functions in 1 directory
file.sources<-list.files("../FuelLoadDists/Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../FuelLoadDists/Functions/",x,sep=""))
sapply(file.sources,FUN="source")
#####
#EVT tally function 
# MK: assign these to objects so the results are stored in the workspace
# here and for all function calls
EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd_FINAL", write.file = TRUE)
EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd", write.file = TRUE)
#####
#Loading Tally Function 

loadTallies<-loading.tally.fn(data.file, start.col = 3, write.file = TRUE)
#####
#Sensitivity Analysis Function 
#EVT 624 as example

samo.fn(data.file, evts = 624, start.col = 12)
#####
#Distribution Fitting Function
#EVT 624 is shown as example

dist.fit.fn(data.file, evts = 624, start.col = 12, write.file = FALSE,min.plot=100)
#####
#Distribution Fitting Graphing Function
#EVT 624 as example 

dist.fit.graph.fn(data.file, evts = 624, start.col = 3)

#####
#dist.fit.fn function for evts over 100
 