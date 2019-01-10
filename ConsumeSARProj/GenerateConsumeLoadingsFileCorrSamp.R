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
  if(phase=="S") # then the it's possible we have aggregated CWD R and CWD S, 
#so here to be compatible we fofem and consume we fill in the remaining size classes
  {
    tmp1.col<-which(names(corr.samp.vals)=="cwd_sound_loading_Mgha")
    tmp2.col<-which(names(corr.samp.vals)=="cwd_rotten_loading_Mgha")
    if(length(tmp1.col)>0)
    {
      input.samp$X1000hrS_loading_Mgha<-0
      input.samp$GT10KhrS_loading_Mgha<-0
      input.samp$X1000hrR_loading_Mgha<-0
      input.samp$GT10KhrR_loading_Mgha<-0
      
    }
  }
  fuel.matches<-match(names(input.samp),fbLoadNames.df[,3]) 
  # match the database column names to rows in the matching dataframe
  # these are the columns we won't do anything with
  match.Names<-switch(mod,
                      C=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],1],
                      F=fbLoadNames.df[fuel.matches[!is.na(fuel.matches)],4])
  
  if(change.units) # convert from Mg/ha to tons/acre, or from cm to in if depth. ignore percent
  {
    for(ii in 1:length(match.Names))
    {
      if(!is.na(match.Names[ii]))
      {
        if(match.Names[ii]=="litter_depth"|match.Names[ii]=="duff_upper_depth"|match.Names[ii]=="DuffDepth")
        {
          input.samp[,ii]<-round(input.samp[,ii]*0.393701,digits=5)
        }
         
         else # 1.10231 tons/Mg, 2.47105 acre/ha
        {
          input.samp[,ii]<-round(input.samp[,ii]*1.10231/2.47105,digits=5)
        }
      }
    }
  }
  
  if(mod=="C")
  {
    base.loads<-all.fbs[all.fbs$fuelbed_number==base.fb,]
    for(i in 2:(nrow(corr.samp.vals))) # 
      base.loads<-rbind(base.loads,all.fbs[all.fbs$fuelbed_number==base.fb,]) # only rbind the necessary columns
    
    base.loads$filename<-as.character(base.loads$filename)
    
    base.loads[,match.Names]<-input.samp
    
    
    base.loads$filename[(1:(nrow(input.samp)))]<-rep("sobolSamplefb",nrow(input.samp)) # and give each fuelbed a unique name, here, and number, below
    base.loads$fuelbed_number[(1:(nrow(input.samp)))]<-base.fb*1000+1:nrow(input.samp)
    base.loads$duff_upper_loading[base.loads$duff_upper_loading<0.1]<-0.1 # for consistency with FOFEM
  }
  if(mod=="F")
  {
    cur.fofem<-base.fofem[1,]
    match2.names<-fbLoadNames.df[!is.na(fbLoadNames.df[,4]),1] # for fofem, to move between consume names and fofem names
    match2a.names<-unique(match2.names[!is.na(match2.names)])
    match3.names<-fbLoadNames.df[!is.na(fbLoadNames.df[,1]),4] # for fofem, to move between consume names and fofem names
    match3a.names<-unique(match3.names[!is.na(match3.names)])
    
    cur.fb<-all.fbs[all.fbs$fuelbed_number==base.fb,]
    cur.fofem[,match3a.names]<-cur.fb[,match2a.names] # replace the non-SA fuel values with those in the base fb
    base.loads<-cur.fofem[1,] # use this for the non-loading values
    
    for(i in 2:(nrow(input.samp))) # 
      base.loads<-rbind(base.loads,cur.fofem) # only rbind the necessary columns
    
    base.loads[,match.Names]<-input.samp # replace baseline fuel loading with the sampled values for the target types

    
    base.loads$Stand<-as.numeric(base.loads$Stand)
    base.loads$Stand[(1:(nrow(input.samp)))]<-base.fb*1000+1:nrow(input.samp)
    base.loads$Duff_tpa[base.loads$Duff_tpa<0.1]<-0.10 # for FOFEM, does not allow a value of zero for duff
  }
  return(base.loads)
}

