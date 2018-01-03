#######
# Estimate marginal fuel loading distributions,
# where 0 values are included in distribution fitting
# add 0.01 to all values for distribution fitting
# ***Find reference for best tweak for log-transformation
# Evaluate how much this changes distribution fits
# and for which EVTs
# MCK, Nov 27, 2017
#######

data.file<-read.csv("../Data/CurrentData/_metricLoadingsCrosstab.csv") #substitute filepath and name for local system
# start.col=13
# EVTCol = LFEVTGroupCd_FINAL
# first read the functions into the current session
file.sources<-list.files("../Functions/") # all functions in this directory
file.sources<-sapply(file.sources,FUN=function(x) paste("../Functions/",x,sep=""))

sapply(file.sources,FUN="source")

# tally number of observations by EVT, to find the subset for which we will perform
# distribution fitting

EVTTallies<-EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd_FINAL", write.file = TRUE)

# These are the EVTs that meet the minimum threshold
# of at least 100 total entries.
evt.vals<-EVTTallies$evt.min_tally
# there are 50 EVTs that meet the criterion
# Estimate the 4 candidate distributions for each evt/loading combination
# that meets the minimum data requirement
library(fitdistrplus) # required for distribution fitting
# note, in new file 1 value < 0
distributionFittingWZero <- dist.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                   write.file = FALSE, min.plot = min.plot, 
                                    evt.col = evt.col,include0=TRUE)
# see warnings in fitting with 0, but only 1-3
# Warning messages:
#   1: In sqrt(diag(varcovar)) : NaNs produced
# 2: In sqrt(1/diag(V)) : NaNs produced
# 3: In cov2cor(varcovar) :
#   diag(.) had 0 or NA entries; non-finite result is doubtful

min.plot<-100
start.col<-13
evt.col<-"LFEVTGroupCd_FINAL"

distributionFittingWOZero <- dist.fit.fn(data.file, evts = evt.vals, start.col = start.col, 
                                        write.file = FALSE, min.plot = min.plot, 
                                        evt.col = evt.col,include0=FALSE)

#########
# now look at which distributions are chosen
##########
distributionRankingWZero<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingWZero,start.col=13)

distributionRankingWOZero<-distfit.rank.fn(evts = evt.vals,
                                          DistFitSum = distributionFittingWOZero,start.col=13)
##########
# look at some of the fits
##########

# summarize across all EVTs how often each distribution is chosen for each
# loading category

distSummaryWZero<-distfit.summary.fn(distributionRankingWZero,data.file,start.col=13)
distSummaryWOZero<-distfit.summary.fn(distributionRankingWOZero,data.file,start.col=13)
# On a cursory glance, with the exception of a few of the 
# larger categories, the distributions seem to shift to
# more lnormal being chosen when 0's are excluded
# in those cases where there is a shift, rather than a reduction across
# the board. Note, there are also several categories where 
# no distributions are chosen, presumably because they do not meet
# the minimum sample size.

write.csv(distSummaryWZero,file="DistributionsChosenWZeroes.csv",row.names=FALSE)
write.csv(distSummaryWOZero,file="DistributionsChosenWOZeroes.csv",row.names=FALSE)

# Look at some of the distribution fits.

#Next we will assess the QUALITY of the fits, using equivalence testing
library(equivalence)

