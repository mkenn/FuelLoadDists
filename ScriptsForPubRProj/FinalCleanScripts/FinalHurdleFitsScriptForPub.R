##########
# script to estimate hurdle distribution
##########
# read in the current version of the database
data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
start.col=13 # the first fuel-loading column
EVTCol = "LFEVTGroupCd_FINAL" # the column name corresponding to the EVT group number
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
# Estimate candidate distributions for each evt/loading combination
# that meets the minimum data requirement
library(fitdistrplus) # required for distribution fitting

distributionCustomFittingHurdle <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                             min.plot = min.plot, evt.col = EVTCol,
                                                             distrs = c("lnorm","gamma"))
distributionCustomFittingHurdleNOut <- dist.custom.hurdle.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                                             min.plot = min.plot, evt.col = EVTCol,
                                                             distrs = c("lnorm","gamma"),removeOut = TRUE)
# the custom function allows for user-specified distributions, lnorm and gamma by default.
# These are characters that should follow the p* nomenclature (e.g., use lnorm because plnorm is the R distribution function)
# the custom function only estimates hurdle, no distributions with zeroes

# now identify the distribution with the maximum likelihood
# note: this is the same as the distribution chosen by AIC
# because all continuous distributions have 2 parameters
# Note also this only chooses the continuous portion of the distribution fitting
# It assumes the proportion of zeroes is the best estimate for pi
distributionCustomRankingHurdle<-distfit.rank.fn(evts = evt.vals,
                                                 DistFitSum = distributionCustomFittingHurdle$HurdleFit,
                                                 start.col=13,dist.cols = c("lnormLL","gammaLL"))
distributionCustomRankingHurdleNOut<-distfit.rank.fn(evts = evt.vals,
                                                 DistFitSum = distributionCustomFittingHurdleNOut$HurdleFit,
                                                 start.col=13,dist.cols = c("lnormLL","gammaLL"))

save.image("Workspaces/HurdleCustomFitsNoZeroWNoOut.RData") # save the workspace
