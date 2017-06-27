# Create a way to compare the number of pixels that have been covered
# Comparing coverage to database

# database file - 
data.file<-read.csv(choose.files())

# coverage file - EVTRepresentationUS.csv
coverage.file<-read.csv(choose.files())

# evt from each file
cover.evt="EVT_GP" #from coverage.file
evt.col="LFEVTGroupCd_FINAL" #from data.file

# how can we compare the coverage
# do we compare the observations per evt
# results dataframe
unique.evts<-unique(coverage.file[,cover.evt])
datacomp<-data.frame(evt=unique.evts,dataobs=NA,cover=NA)

#looking at coverage database
#for(i in 1:nrow(coverage.file)) # don't need to loop through the rows, and you're using i twice as an index
#{
  #determining unique evts
  
  #filing the results dataframe with results
  for(i in 1:length(unique.evts))
  {
    # first column is the evts
#    datacomp[,1]<-unique.evts[i]
    
    # second column is the number of observations from the database for each evt
#    datacomp[,2]<-data.file[i,evt.col==unique.evts]
    datacomp[i,2]<-length(data.file[data.file[,evt.col]==unique.evts[i],evt.col])
    
    # third column is the total number of observations
#    datacomp[,3]<-coverage.file[,cover.evt]
    datacomp[i,3]<-sum(coverage.file[coverage.file[,cover.evt]==unique.evts[i],"COUNT"])
  }
datacomp$coverProp<-datacomp$cover/sum(as.numeric(datacomp$cover))
#}
datacomp[datacomp$coverProp>.05&datacomp$dataobs<1000,] # open water and Agriculture
datacomp[datacomp$coverProp>.02&datacomp$dataobs<100,] # grassland low

#########
# next, account for each fuel loading catgory by expanding the columns in datacomp and including a loop over the fuel category columns, adapting the code above