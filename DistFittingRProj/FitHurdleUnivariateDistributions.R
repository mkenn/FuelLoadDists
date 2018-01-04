########
# a script to estimate hurdle models to 
# fuel loading distributions, particularly for those distributions 
# with zero-inflated data
########

data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
start.col=13
EVTCol = "LFEVTGroupCd_FINAL"
# first read the functions into the current session
file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))

sapply(file.sources,FUN="source")

# tally number of observations by EVT, to find the subset for which we will perform
# distribution fitting

EVTTallies<-EVT.tally.fn(data.file, evt.col = EVTCol, write.file = TRUE)

# These are the EVTs that meet the minimum threshold
# of at least 100 total entries.
evt.vals<-EVTTallies$evt.min_tally
# there are 50 EVTs that meet the criterion
# Estimate the 4 candidate distributions for each evt/loading combination
# that meets the minimum data requirement
library(fitdistrplus) # required for distribution fitting

min.plot<-30

distributionFittingHurdle <- dist.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                        min.plot = min.plot, evt.col = EVTCol,add.val=0.1)
# Note: this object contains distfitW0.df and distfitHurdle.df. 
# distfitW0.df has all values including 0 in a single fitted distribution
# 

distributionRankingWZero<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingHurdle[[1]],start.col=13)
distributionRankingHurdle<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingHurdle[[2]],start.col=13)

##Now, how to compare distribution fits between the two. Numerically at first.
