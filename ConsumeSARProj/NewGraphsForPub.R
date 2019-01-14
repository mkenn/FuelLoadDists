######
# alternative graphs for senstivity analysis
######

load("SmolderingFirstTryN1000B1000.RData")

# These are erroneously only 97th percentile weather
# so the sensitivity indices list first level is targetevt1
# Then second level is, emissions1,emission2,emission3, etc
# 9 emission types for consume,
# 6 for fofem
# 3 target evts:
length(smoldering.sa.indices)
# 9 emissions
length(smoldering.sa.indices[[1]]$ConsumeSobol)

##########
# draft code, all three evt groups on 1 graph,
# same emissions
##########
n.var<-length(sobol.obj$S$original)
y.lim=c(-.15,1)
main.txt=""
sobol.obj<-smoldering.sa.indices[[1]]$ConsumeSobol[[1]]
plot(1:n.var,sobol.obj$S$original,axes=FALSE,ylim=y.lim,pch=16,
     xlab="",ylab="Sobol Sensitivity Index",
     main=main.txt,cex.main=0.7)
axis(1,at=1:nrow(sobol.obj$S),labels=rownames(sobol.obj$S),
     las=2,cex.lab=0.9,tick = F)
segments(x0 = 1:nrow(sobol.obj$S),x1=1:nrow(sobol.obj$S),
         y0=sobol.obj$S$`min. c.i.`,y1=sobol.obj$S$`max. c.i.`)
axis(2,las=1)
abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0

sobol.obj<-smoldering.sa.indices[[2]]$ConsumeSobol[[1]]
points(1:n.var,sobol.obj$S$original,pch=1,col="blue",cex=1.25)
segments(x0 = 1:nrow(sobol.obj$S),x1=1:nrow(sobol.obj$S),
         y0=sobol.obj$S$`min. c.i.`,y1=sobol.obj$S$`max. c.i.`,
         col="blue",lty=2,lwd=2)

sobol.obj<-smoldering.sa.indices[[3]]$ConsumeSobol[[1]]
points(1:n.var,sobol.obj$S$original,pch=7,col="purple",cex=1.25)
segments(x0 = 1:nrow(sobol.obj$S),x1=1:nrow(sobol.obj$S),
         y0=sobol.obj$S$`min. c.i.`,y1=sobol.obj$S$`max. c.i.`,
         col="purple",lty=3,lwd=2)
box()


# So assume 6 emissions each for fofem and consume
# 2x3 graphs for each fofem and consume, x environmental variables
# which would be 4 2x3 graphs
# or try to offset within emissions
