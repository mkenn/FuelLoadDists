##########
# take fit objects and see if equivalance between theoretical
# and observed can be rejected
##########

# requires the equivalence library
library(equivalence)

## Assumes distribution fitting has been completed, either in the current session
## or that the workspace was saved. If the latter, run this line to load
## the workspace

load("distributionFits.RData")

# data.file is the fuel loading data base
# evt.vals is the vector of EVTs for which distribution fitting was attempted 
# (meeting the minimum number of observations)
# distributionFittingWZero, distributionFittingWOZero gives the two 
# distribution fitting objects 
# use also start.col, min.plot, evt.col

#Here is the function call:

# equivalence.fn(data.file,distFit.obj,evts,evt.col,start.col,cur.cols,
#                          min.plot=30,write.file=FALSE,file.name="EquivalenceEVT",
#                          ep.val=.25, include0=FALSE,add.val=0.01)

# Many of the arguments are the same as in the distribution fitting function

ep.val<-0.25 # What is the epsilon value for "acceptable" error? 0.25 is very generous

equivalenceWZero<-equivalence.fn(data.file,distFit.obj=distributionFittingWZero,evts=evt.vals,
                              evt.col=evt.col,start.col=start.col,
                                min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                ep.val=ep.val, include0=FALSE,add.val=add.val)
                              
equivalenceWOZero<-equivalence.fn(data.file,distFit.obj=distributionFittingWOZero,evts=evt.vals,
                                 evt.col=evt.col,start.col=start.col,cur.cols=start.col:ncol(data.file),
                                 min.plot=min.plot,write.file=FALSE,file.name="EquivalenceEVT",
                                 ep.val=ep.val, include0=FALSE,add.val=add.val)
