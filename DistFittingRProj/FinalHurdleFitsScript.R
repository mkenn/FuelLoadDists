##########
# script to estimate hurdle distribution
##########
# read in the current version of the database
# data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
# start.col=13 # the first fuel-loading column
data.file<-read.csv("../Data/CurrentData/loadingsByEVTGroup_20180706.csv") #substitute filepath and name for local system
start.col=11 # the first fuel-loading column
EVTCol = "LFEVTGroupCd_FINAL"
# read the functions into the current session
file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))
sapply(file.sources,FUN="source")

# decide on the minimum number of entries for distribution fitting
min.plot<-30

# tally number of observations by EVT, to find the subset for which we will perform
# distribution fitting
EVTTallies<-EVT.tally.fn(data.file, evt.col = EVTCol, write.file = TRUE,min.tally = min.plot)

# These are the EVTs that meet the minimum threshold
# of at least 30 total entries.
evt.vals<-EVTTallies$evt.min_tally
# Estimate the 4 candidate distributions for each evt/loading combination
# that meets the minimum data requirement
library(fitdistrplus) # required for distribution fitting

distributionCustomFittingHurdleAll <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                             min.plot = min.plot, evt.col = EVTCol,add.val=0.1)
distributionCustomFittingHurdle <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                             min.plot = min.plot, evt.col = EVTCol,add.val=0.1,
                                                             distrs = c("lnorm","gamma"))
distributionCustomFittingHurdleNOut <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                             min.plot = min.plot, evt.col = EVTCol,add.val=0.1,
                                                             distrs = c("lnorm","gamma"),removeOut = TRUE)
# the custom function allows for user-specified distributions, lnorm and gamma by defaul.
# These are characters that should follow the p* nomenclature (e.g., use lnorm because plnorm is the R distribution function)
# the custom function only estimates hurdle, no distributions with zeroes
# distributionFittingHurdle <- dist.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
#                                                 min.plot = min.plot, evt.col = EVTCol,add.val=0.1)
# now identify the distribution with the maximum likelihood
# note: this is the same as the distribution chosen by AIC
# because all continuous distributions have 2 parameters
# Not also this only chooses the continuous portion of the distribution fitting
# It assumes the proportion of zeroes is the best estimate for pi
# distributionRankingWZero<-distfit.rank.fn(evts = evt.vals,
#                                           DistFitSum = distributionFittingHurdle$WithZeroFit,start.col=13)
distributionCustomRankingHurdleAll<-distfit.rank.fn(evts = evt.vals,
                                           DistFitSum = distributionCustomFittingHurdleAll$HurdleFit,
                                           start.col=start.col)
distributionCustomRankingHurdle<-distfit.rank.fn(evts = evt.vals,
                                                 DistFitSum = distributionCustomFittingHurdle$HurdleFit,
                                                 start.col=start.col,dist.cols = c("lnormLL","gammaLL"))
distributionCustomRankingHurdleNOut<-distfit.rank.fn(evts = evt.vals,
                                                 DistFitSum = distributionCustomFittingHurdleNOut$HurdleFit,
                                                 start.col=start.col,dist.cols = c("lnormLL","gammaLL"))

save.image("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
