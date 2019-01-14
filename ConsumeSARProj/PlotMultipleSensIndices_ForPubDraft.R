##########
# plot prcc and Sobol for comparison
##########

# load the workspaces
# "SmolderingSAJan11_N5000BS1000.RData", "FlamingSAJan11_N5000BS1000.RData"
# Smoldering first
# note, still have to run FOFEM on these
load("g:/My Drive/SmolderingSAJan11_N5000BS1000.RData")
#########
# so first compare Consume between environmental settings
# here are the equivalent response variables (model outputs)
responseConsume.vars[4:9]
responseFOFEM.vars
par(mfrow=c(2,3),mar=c(12,3.5,1.5,0),mgp=c(2.25,0.5,0),las=1) # 6 total plots, one for each model variable

for(k in 1:length(target.evts))
{
  all.id1<-k*2-1
  all.id2<-k*2
  for(var in 4:9)
  {
    plot(1:length(smolder2.id),smoldering.sa.indices[[all.id1]]$ConsumeSobol[[var]]$S$original,
         axes=FALSE,ylim=c(-0.15,1),pch=16,
          xlab="Fuels type",ylab="Sobol sensitivity index",
          main=paste("Consume Smoldering",responseConsume.vars[var]),cex.main=0.7)
    points(1:length(smolder2.id),smoldering.sa.indices[[all.id2]]$ConsumeSobol[[var]]$S$original,
         pch=7,col="cyan")
  }
}

# looks identical! Environmental conditions do not impact the sensitivity of individual fuels
# for flaming or smoldering in Consume
# so for graphics, for now, don't need to worry about environmental conditions
# just the comparisons betweent EVT groups
########
# Comparing EVT groups smoldering fuels, for Consume
par(mfrow=c(2,3),mar=c(12,3.5,1.5,0),mgp=c(2.25,0.5,0),las=1) # 6 total plots, one for each model variable
for(var in 4:9)
{
  plot(1:length(smolder2.id),smoldering.sa.indices[[1]]$ConsumeSobol[[var]]$S$original,
       axes=FALSE,ylim=c(-0.15,1),pch=16,cex=0.9,
       xlab="Fuels type",ylab="Sobol sensitivity index",
       main=paste("Consume Smoldering",responseConsume.vars[var]),cex.main=0.7)
  points(1:length(smolder2.id),smoldering.sa.indices[[3]]$ConsumeSobol[[var]]$S$original,
         pch=7,col="cyan")
  points(1:length(smolder2.id),smoldering.sa.indices[[5]]$ConsumeSobol[[var]]$S$original,
         pch=5,col="magenta",cex=1.1)
}
# so values of the sensitivity index differ slightly among EVT groups, 
# but not the relative rankings. 

# comparing environmental conditions flaming fuels consume
load("g:/My Drive/FlamingSAJan11_N5000BS1000.RData")
#########
# so first compare Consume between environmental settings
# here are the equivalent response variables (model outputs)
par(mfrow=c(4,3),mar=c(12,3.5,1.5,0),mgp=c(2.25,0.5,0),las=1) # 6 total plots, one for each model variable

for(k in 1:length(target.evts))
{
  all.id1<-k*2-1
  all.id2<-k*2
  for(var in 4:9)
  {
    plot(1:length(flaming2.id),flaming.sa.indices[[all.id1]]$ConsumeSobol[[var]]$S$original,
         axes=FALSE,ylim=c(-0.15,1),pch=16,
         xlab="",ylab="Sobol sensitivity index",
         main=paste("Consume Flaming",responseConsume.vars[var]),cex.main=0.7)
    points(1:length(flaming2.id),flaming.sa.indices[[all.id2]]$ConsumeSobol[[var]]$S$original,
           pch=7,col="cyan")
    axis(1,at=1:length(flaming2.id),labels=rownames(flaming.sa.indices[[all.id1]]$ConsumeSobol[[var]]$S),
         las=2,cex.lab=0.9,tick = F)
    axis(2)
    
  }
}

# looks identical! Environmental conditions do not impact the sensitivity of individual fuels
# for flaming or smoldering in Consume
# so for graphics, for now, don't need to worry about environmental conditions
# just the comparisons betweent EVT groups
for(var in 4:9)
{
  plot(1:length(flaming2.id),flaming.sa.indices[[1]]$ConsumeSobol[[var]]$S$original,
       axes=FALSE,ylim=c(-0.15,1),pch=16,
       xlab="Fuels type",ylab="Sobol sensitivity index",
       main=paste("Consume Flaming",responseConsume.vars[var]),cex.main=0.7)
  points(1:length(flaming2.id),flaming.sa.indices[[3]]$ConsumeSobol[[var]]$S$original,
         pch=7,col="cyan")
  points(1:length(flaming2.id),flaming.sa.indices[[5]]$ConsumeSobol[[var]]$S$original,
         pch=5,col="magenta")
  points(1:length(flaming2.id),flaming.sa.indices[[7]]$ConsumeSobol[[var]]$S$original,
         pch=18,col="purple")
  points(1:length(flaming2.id),flaming.sa.indices[[9]]$ConsumeSobol[[var]]$S$original,
         pch=3,col="brown")
  points(1:length(flaming2.id),flaming.sa.indices[[11]]$ConsumeSobol[[var]]$S$original,
         pch=8,col="orange")
}
# as with smoldering, the relative rankings don't matter among baseline EVT groups
# so good news and simplicity for applications--very little interactions with baselines
