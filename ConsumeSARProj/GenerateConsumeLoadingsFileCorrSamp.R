###########
# function to generate consume input data
# combining a sobol (correlated) sample
# with baseline values from the example fuelbed
###########
# mod="C" for consume,"F" for fofem
# phase="F" for flaming, "S" for smoldering
GenerateFuelInput.fn<-function(corr.samp.vals,nreps=NA,fbLoadNames.df,all.fbs,base.fb,
                                    base.fofem=NA,change.units=T,mod="C",phase="F")
{
  input.samp<-corr.samp.vals
  fuel.matches<-match(names(corr.samp.vals),fbLoadNames.df[,3]) 
  # match the database column names to rows in the matching dataframe
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
  if(change.units) # convert from Mg/ha to tons/acre, or from cm to in if depth. ignore percent
  {
    for(ii in 1:length(match.Names))
    {
      if(!is.na(match.Names[ii]))
      {
        if(match.Names[ii]=="litter_depth"|match.Names[ii]=="duff_upper_depth"|match.Names[ii]=="DuffDepth")
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
    
    base.loads[,match.Names]<-input.samp
    
    
    base.loads$filename[(1:(nrow(corr.samp.vals)))]<-rep("sobolSamplefb",nrow(corr.samp.vals)) # and give each fuelbed a unique name, here, and number, below
    base.loads$fuelbed_number[(1:(nrow(corr.samp.vals)))]<-base.fb*1000+1:nrow(corr.samp.vals)
  }
  if(mod=="F")
  {
    cur.fofem<-base.fofem[1,]
    match2.names<-fbLoadNames.df[!is.na(fbLoadNames.df[,4]),1] # for fofem, to move between consume names and fofem names
    match2a.names<-match2.names[!is.na(match2.names)]
    match3.names<-fbLoadNames.df[!is.na(fbLoadNames.df[,1]),4] # for fofem, to move between consume names and fofem names
    match3a.names<-match3.names[!is.na(match3.names)]
    
    cur.fb<-all.fbs[all.fbs$fuelbed_number==base.fb,]
    cur.fofem[,match3a.names]<-cur.fb[,match2a.names] # replace the non-SA fuel values with those in the base fb
    base.loads<-cur.fofem[1,] # use this for the non-loading values
    
    for(i in 2:(nrow(corr.samp.vals))) # 
      base.loads<-rbind(base.loads,cur.fofem) # only rbind the necessary columns
    
    base.loads[,match.Names]<-input.samp

    
    base.loads$Stand<-as.numeric(base.loads$Stand)
    base.loads$Stand[(1:(nrow(input.samp)))]<-base.fb*1000+1:nrow(input.samp)
    # but here we need to give baseline values for the remaining fuels types, for now 
    # based on the base.fb
  }
  return(base.loads)
}

