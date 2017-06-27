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
datacomp<-data.frame(evt=NA,dataobs=NA,cover=NA)

#looking at coverage database
for(i in 1:nrow(coverage.file))
{
  #determining unique evts
  unique.evts<-unique(coverage.file[,cover.evt])
  
  #filing the results dataframe with results
  for(i in 1:unique.evts)
  {
    # first column is the evts
    datacomp[,1]<-unique.evts[i]
    
    # second column is the number of observations from the database for each evt
    datacomp[,2]<-data.file[i,evt.col==unique.evts]
    
    # third column is the total number of observations
    datacomp[,3]<-coverage.file[,cover.evt]
  }
}

