#####Functions#####
#install fitdistrplus into console 

library(fitdistrplus)

#use loadingsByEVTGroup_20170328.csv data for data.file

data.file <- read.csv(choose.files())
#####
#EVT tally function 

EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd_FINAL", write.file = TRUE)
#####
#Loading Tally Function 

loading.tally.fn(data.file, start.col = 3, write.file = TRUE)
#####
#Sensitivity Analysis Function 
#EVT 624 as example

samo.fn(data.file, evts = 624, start.col = 3)
#####
#Distribution Fitting Function
#EVT 624 is shown as example

dist.fit.fn(data.file, evts = 624, start.col = 3, write.file = TRUE)
#####
#Distribution Fitting Graphing Function
#EVT 624 as example 

dist.fit.graph.fn(data.file, evts = 624, start.col = 3)

#####
#dist.fit.fn function for evts over 100

tdist.fit.fn<-function(data.file,evts,evt.col,start.col,cur.cols=c(start.col:ncol(data.file)),min.plot=100,write.file=TRUE,file.name="DistFitSummaryEVT")
  