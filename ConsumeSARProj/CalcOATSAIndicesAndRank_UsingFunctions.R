#######
# calls functions to calculate sensitivity indices, rank, and summarize
# will be expanded to include multiple EVTs
#######
# starts with results.sa from script ConductSAFromR.R
consume.out<-read.csv(paste("SAResults/",resultsfilename,sep=""))
consume.in<-read.csv("FuelLoadInputEVT660Try1.csv",skip=1)
  #new.loads.file # created in the GenerateLoadingsFileOAT.R script
var.sa<-unique(as.character(consume.out$Filename))[-1] # the loadings varied in the SA
all.out.var<-c("PM25.Emissions","PM10.Emissions","PM.Emissions","CH4.Emissions","CO.Emissions","NMHC.Emissions") # these are the emissions output chosen as response variables

sa.indices<-calc.indices.fn(all.out.var,var.sa,consume.in,consume.out) # calculates sensitivity indices
order.sa.list<-order.indices.fn(sa.indices,var.sa) # orders the variables from most to least sensitive for each SA index
rank.sa.list<-rank.indices.fn(order.sa.list,var.sa) # returns ranks for each loading variable
rank.summary.sa.list<-rank.summary.fn(rank.sa.list,var.sa)
  