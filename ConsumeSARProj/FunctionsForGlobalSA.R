######
# generate sobol fuel loads file, independent samples
library(sensitivity)
sobolFuelsFile.fn<-function(range.vals,range.id=1,nreps,fbLoadNames.df,all.fbs,base.fb,change.units=T)
{
  range.matches<-match(range.vals[[1]][[2]][,1],fbLoadNames.df[,3]) # match the database column names to the FVS names
  range.vals[[1]][[2]]$ConsumeNames<-NA # create a new column the gives the Consume column name corresponding to the database column names
  range.vals[[1]][[2]]$ConsumeNames[!is.na(range.matches)]<-fbLoadNames.df[range.matches[!is.na(range.matches)],1]
  # create a corresponding column for the consume names to the range.values data frame
  
  if(change.units) # convert from Mg/ha to tons/acre, or from cm to in if depth. ignore percent
  {
    for(ii in 1:length(range.vals[[1]][[2]]$ConsumeNames))
    {
      if(!is.na(range.vals[[1]][[2]]$ConsumeNames[ii]))
      {
        if(range.vals[[1]][[2]]$ConsumeNames[ii]=="litter_depth"|range.vals[[1]][[2]]$ConsumeNames[ii]=="duff_upper_depth")
        {
          range.vals[[1]][[2]]$lower.q[ii]<-range.vals[[1]][[2]]$lower.q[ii]*0.393701
          range.vals[[1]][[2]]$upper.q[ii]<-range.vals[[1]][[2]]$upper.q[ii]*0.393701
          range.vals[[1]][[2]]$max[ii]<-range.vals[[1]][[2]]$max[ii]*0.393701
          range.vals[[1]][[2]]$min[ii]<-range.vals[[1]][[2]]$min[ii]*0.393701
        }
        else # 1Mg = 1.10231 tons, 1 ha = 2.47105 acre
        {
          range.vals[[1]][[2]]$lower.q[ii]<-range.vals[[1]][[2]]$lower.q[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$upper.q[ii]<-range.vals[[1]][[2]]$upper.q[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$max[ii]<-range.vals[[1]][[2]]$max[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$min[ii]<-range.vals[[1]][[2]]$min[ii]*1.10231/2.47105
        }
      }
    }
  }
  
  which.change<-which(!is.na(range.vals[[1]][[2]][,2])) # for which of our categories do we have sufficient coverage?
  which2.change<-which.change[!is.na(fbLoadNames.df[range.matches[which.change],1])] # only keep those that have consume names
  
  myX1.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change)) # create the first sobol sample
  for(i in 1:length(which2.change)) # for each variable, sample uniformly on the range values q_.1,q_.9
    myX1.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
  myX2.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change))
  for(i in 1:length(which2.change))
    myX2.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
  
#  sobol1<-sobolEff(X1=myX1.sobol,X2=myX2.sobol,nboot = 1000) # create the sobol sample
  sobol1<-sobolEff(X1=myX1.sobol,X2=myX2.sobol,nboot = 10000) # create the sobol sample
  
  new.loads.file<-all.fbs[all.fbs$fuelbed_number==base.fb,] # identify the baseline fb and fill in the new fuels dataframe
  for(i in 2:(nrow(sobol1$X))) # 
    new.loads.file<-rbind(new.loads.file,all.fbs[all.fbs$fuelbed_number==base.fb,])
  # and next we replace these baseline values with randomly drawn values based on the range.vals matrix
  # first figure out which actually have ranges
  new.loads.file$filename<-as.character(new.loads.file$filename)
  
  for(i in 1:length(which2.change)) # for each variable with sufficient representation in the database
  {
    # index skipping the first row
    new.loads.file[(1:(nrow(sobol1$X))),fbLoadNames.df[range.matches[which2.change[i]],1]]<-sobol1$X[,i]
  }
  new.loads.file$filename[(1:(nrow(sobol1$X)))]<-rep("sobolSamplefb",nrow(sobol1$X)) # and give each fuelbed a unique name, here, and number, below
  new.loads.file$fuelbed_number[(1:(nrow(sobol1$X)))]<-base.fb*1000+1:nrow(sobol1$X)
  
  return(list(new.loads.file,sobol1,fbLoadNames.df[range.matches[which2.change],1]))
}

##########
# adapted for FOFEM
##########
sobolFuelsFileFOFEM.fn<-function(range.vals,range.id=1,nreps,
                                 fbLoadNames.df,all.fbs,base.fb,change.units=FALSE)
{
  range.matches<-match(range.vals[[1]][[2]][,1],fbLoadNames.df[,3]) # match the database column names to the FVS names
  range.vals[[1]][[2]]$FOFEMNames<-NA # create a new column the gives the Consume column name corresponding to the database column names
  range.vals[[1]][[2]]$FOFEMNames[!is.na(range.matches)]<-fbLoadNames.df[range.matches[!is.na(range.matches)],4]
  # create a corresponding column for the consume names to the range.values data frame
  
  if(change.units) # convert from Mg/ha to tons/acre, or from cm to in if depth. ignore percent
  {
    for(ii in 1:length(range.vals[[1]][[2]]$FOFEMNames))
    {
      if(!is.na(range.vals[[1]][[2]]$FOFEMNames[ii]))
      {
        if(range.vals[[1]][[2]]$FOFEMNames[ii]=="litter_depth"|range.vals[[1]][[2]]$FOFEMNames[ii]=="duff_upper_depth")
        {
          range.vals[[1]][[2]]$lower.q[ii]<-range.vals[[1]][[2]]$lower.q[ii]*0.393701
          range.vals[[1]][[2]]$upper.q[ii]<-range.vals[[1]][[2]]$upper.q[ii]*0.393701
          range.vals[[1]][[2]]$max[ii]<-range.vals[[1]][[2]]$max[ii]*0.393701
          range.vals[[1]][[2]]$min[ii]<-range.vals[[1]][[2]]$min[ii]*0.393701
        }
        else # 1Mg = 1.10231 tons, 1 ha = 2.47105 acre
        {
          range.vals[[1]][[2]]$lower.q[ii]<-range.vals[[1]][[2]]$lower.q[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$upper.q[ii]<-range.vals[[1]][[2]]$upper.q[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$max[ii]<-range.vals[[1]][[2]]$max[ii]*1.10231/2.47105
          range.vals[[1]][[2]]$min[ii]<-range.vals[[1]][[2]]$min[ii]*1.10231/2.47105
        }
      }
    }
  }
  
  which.change<-which(!is.na(range.vals[[1]][[2]][,2])) # for which of our categories do we have sufficient coverage?
  which2.change<-which.change[!is.na(fbLoadNames.df[range.matches[which.change],4])] # only keep those that have consume names
  
  myX1.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change)) # create the first sobol sample
  for(i in 1:length(which2.change)) # for each variable, sample uniformly on the range values q_.1,q_.9
    myX1.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
  myX2.sobol<-matrix(NA,nrow=nreps,ncol=length(which2.change))
  for(i in 1:length(which2.change))
    myX2.sobol[,i]<-runif(nreps,range.vals[[1]][[2]][which2.change[i],2],range.vals[[1]][[2]][which2.change[i],3])
  
  #  sobol1<-sobolEff(X1=myX1.sobol,X2=myX2.sobol,nboot = 1000) # create the sobol sample
  sobol1<-sobolEff(X1=myX1.sobol,X2=myX2.sobol,nboot = 10000) # create the sobol sample
  
  new.loads.file<-all.fbs[all.fbs$fuelbed_number==base.fb,] # identify the baseline fb and fill in the new fuels dataframe
  for(i in 2:(nrow(sobol1$X))) # 
    new.loads.file<-rbind(new.loads.file,all.fbs[all.fbs$fuelbed_number==base.fb,])
  # and next we replace these baseline values with randomly drawn values based on the range.vals matrix
  # first figure out which actually have ranges
  new.loads.file$filename<-as.character(new.loads.file$filename)
  
  for(i in 1:length(which2.change)) # for each variable with sufficient representation in the database
  {
    # index skipping the first row
    new.loads.file[(1:(nrow(sobol1$X))),fbLoadNames.df[range.matches[which2.change[i]],1]]<-sobol1$X[,i]
  }
  new.loads.file$filename[(1:(nrow(sobol1$X)))]<-rep("sobolSamplefb",nrow(sobol1$X)) # and give each fuelbed a unique name, here, and number, below
  new.loads.file$fuelbed_number[(1:(nrow(sobol1$X)))]<-base.fb*1000+1:nrow(sobol1$X)
  
  return(list(new.loads.file,sobol1,fbLoadNames.df[range.matches[which2.change],1]))
}