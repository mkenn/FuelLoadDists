###########
# function to generate consume input data
# combining a sobol (correlated) sample
# with baseline values from the example fuelbed
###########
# mod="C" for consume,"F" for fofem
# phase="F" for flaming, "S" for smoldering
GenerateSobolFuelInput.fn<-function(corr.samp.vals,nreps,fbLoadNames.df,all.fbs,base.fb,
                                    base.fofem=NA,change.units=T,mod="C",phase="F")
{
  input.samp<-corr.samp.vals
  fuel.matches<-match(names(corr.samp.vals),fbLoadNames.df[,3]) # match the database column names to the FVS names
  # these are the columns we won't do anything with
  # match.Names<-switch(mod,
  #                            C=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],1],
  #                            F=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],4])
  match.Names<-switch(mod,
                      C=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],1],
                      F=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],4])
  
  # range.vals[[1]][[2]]$ConsumeNames<-NA # create a new column the gives the Consume column name corresponding to the database column names
  # range.vals[[1]][[2]]$ConsumeNames[!is.na(range.matches)]<-fbLoadNames.df[range.matches[!is.na(range.matches)],1]
  # create a corresponding column for the consume names to the range.values data frame
  # match2.names<-fbLoadNames.df[!is.na(fbLoadNames.df[,4]),1] # for fofem, to move between consume names and fofem names
  # match2a.names<-match2.names[!is.na(match2.names)]
  if(change.units) # convert from Mg/ha to tons/acre, or from cm to in if depth. ignore percent
  {
    for(ii in 1:length(match.Names))
    {
      if(!is.na(match.Names[ii]))
      {
        if(match.Names[ii]=="litter_depth"|match.Names[ii]=="duff_upper_depth")
        {
          input.samp[,ii]<-corr.samp.vals[,ii]*0.393701
        }
        else # 1Mg = 1.10231 tons, 1 ha = 2.47105 acre
        {
          input.samp[,ii]<-corr.samp.vals[,ii]*1.10231/2.47105
        }
      }
    }
  }
  
  # base.loads<-switch(mod,
  #                    C=all.fbs[all.fbs$fuelbed_number==base.fb,], # identify the baseline fb and fill in the new fuels dataframe
  #                    F=all.fbs[all.fbs$fuelbed_number==base.fb,match2a.names])
  # for(i in 2:(nrow(corr.samp.vals))) # 
  #   base.loads<-switch(mod,
  #                      C=rbind(base.loads,all.fbs[all.fbs$fuelbed_number==base.fb,]),
  #                      F=rbind(base.loads,all.fbs[all.fbs$fuelbed_number==base.fb,match2a.names])) # only rbind the necessary columns
  
  #
  if(mod=="C")
  {
    base.loads<-all.fbs[all.fbs$fuelbed_number==base.fb,]
    for(i in 2:(nrow(corr.samp.vals))) # 
      base.loads<-rbind(base.loads,all.fbs[all.fbs$fuelbed_number==base.fb,]) # only rbind the necessary columns
    
    base.loads$filename<-as.character(base.loads$filename)
    
    base.loads[,match.Names]<-corr.samp.vals
    
    
    base.loads$filename[(1:(nrow(corr.samp.vals)))]<-rep("sobolSamplefb",nrow(corr.samp.vals)) # and give each fuelbed a unique name, here, and number, below
    base.loads$fuelbed_number[(1:(nrow(corr.samp.vals)))]<-base.fb*1000+1:nrow(corr.samp.vals)
  }
  if(mod=="F")
  {
#    base.loads$filename<-as.character(base.loads$filename)
    base.loads<-base.fofem
    for(i in 2:(nrow(corr.samp.vals))) # 
      base.loads<-rbind(base.loads,base.fofem) # only rbind the necessary columns
    
    # new fofem, with expanded cwd/lwd fuel types
    # if(phase=="F")
    # {
    #   base.loads[,match.Names]<-corr.samp.vals
    #   base.loads$X.Stand<-as.numeric(base.loads$X.Stand)
    #   base.loads$X.Stand[(1:(nrow(corr.samp.vals)))]<-base.fb*1000+1:nrow(corr.samp.vals)
    # }
    # 
    # if(phase=="S") # if flaming, convert the sound and rotten 1000 hr to log and percent rotten
    # {
    #   base.loads$Log<-corr.samp.vals$
    # }
    match3a.names<-match3.names[!is.na(match2.names)]
    names(base.loads)<-match3a.names
    
    # so the problem is that log is sound + rotten 1000K, with a percent rotten. Can probably just 
    # take the total for 1000K fuels, then 
  }
  return(base.loads)
}

