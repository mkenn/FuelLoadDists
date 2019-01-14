###########
# run FOFEM on existing fuels matrices
###########
# first smoldering
# load the workspace
load("g:/My Drive/SmolderingSAJan11_N5000BS1000.RData")
# all.id goes through evt*env indexing
# evt1, env1, env2; evt2, env1, env2
all.id<-0
for(k in 1:length(target.evts))
{
  for(j in 1:2)
  {
    all.id<-all.id+1
   smoldering.sa.results[[all.id]]$sobolFResults<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",
                                                                     fuel.loads=smoldering.sa.results[[all.id]]$sobolFfuelLoads,
                                                                     env.in.name=consume.env.infile.names.list[[j]][k],
                                                                     envfilename="EnvInputSA.csv",
                                                                     fofem.filename="FOFEM_FlamingSAInput1.csv",
                                                                     newwd="fofem",oldwd="../")
  smoldering.sa.results[[all.id]]$prccFResults<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",
                                                                     fuel.loads=smoldering.sa.results[[all.id]]$prccFfuelLoads,
                                                                     env.in.name=consume.env.infile.names.list[[j]][k],
                                                                     envfilename="EnvInputSA.csv",
                                                                     fofem.filename="FOFEM_FlamingSAInput1.csv",
                                                                     newwd="fofem",oldwd="../")
  }
}

# Now flaming
# clear the previous workspace
rm(list=objects())
# load the workspace
load("g:/My Drive/FlamingSAJan11_N5000BS1000.RData")
# all.id goes through evt*env indexing
# evt1, env1, env2; evt2, env1, env2
all.id<-0
for(k in 1:length(target.evts))
{
  for(j in 1:2)
  {
    all.id<-all.id+1
    flaming.sa.results[[all.id]]$sobolFResults<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",
                                                                       fuel.loads=flaming.sa.results[[all.id]]$sobolFfuelLoads,
                                                                       env.in.name=consume.env.infile.names.list[[j]][k],
                                                                       envfilename="EnvInputSA.csv",
                                                                       fofem.filename="FOFEM_FlamingSAInput1.csv",
                                                                       newwd="fofem",oldwd="../")
    flaming.sa.results[[all.id]]$prccFResults<-call.emissions.mods(infilename="FuelLoadInputSA.csv",mod="F",
                                                                      fuel.loads=flaming.sa.results[[all.id]]$prccFfuelLoads,
                                                                      env.in.name=consume.env.infile.names.list[[j]][k],
                                                                      envfilename="EnvInputSA.csv",
                                                                      fofem.filename="FOFEM_FlamingSAInput1.csv",
                                                                      newwd="fofem",oldwd="../")
  }
}