# Create a way to compare the number of pixels that have been covered
# Comparing coverage to database

# database file - 
data.file<-read.csv(choose.files())

# coverage file - EVTRepresentationUS.csv
coverage.file<-read.csv(choose.files())

# evt from each file
cover.evt="EVT_GP" #from coverage.file
evt.col="LFEVTGroupCd_FINAL" #from data.file
evt.col="LFEVTGroupCd" #from data.file

# how can we compare the coverage
# do we compare the observations per evt
# results dataframe
unique.evts<-unique(coverage.file[,cover.evt])
datacomp<-data.frame(evt=unique.evts,dataobs=NA,cover=NA)
start.col=3
start.col=12
cur.cols=c(start.col:ncol(data.file))

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

    # fuel loading coverage tally - i have a problem with the actual input
    # when I do portions it is coming back with the results but its not tallying-counting the observations. I think it might also be the !is.na part.
    for(j in 1:length(cur.cols))
    {
      tmp.vec<-data.file[data.file[,evt.col]==unique.evts[i],cur.cols[j]]
#      datacomp[i,j+1]<-length(data.file[!is.na(data.file[,evt.col]==unique.evts[i]),"COUNT"])
      datacomp[i,j+3]<-length(tmp.vec[!is.na(tmp.vec)])
    }
  }
#}

names(datacomp)<-c("evt","dataobs","cover",names(data.file)[cur.cols])

# determing proportion of coverage
# this feels like it should be in the table - gives descriptive information about what the pixel coverage is for each evt and its total number of observations
datacomp$coverProp<-datacomp$cover/sum(as.numeric(datacomp$cover))

plot(datacomp$coverProp,datacomp$dataobs,ylim=c(0,500),xlim=c(0,.05))

# determining what the holes are, if proportion is less than a set amount and less than a set of observations
datacomp[datacomp$coverProp > .05 & datacomp$dataobs < 1000,] # open water and Agriculture
datacomp[datacomp$coverProp > .02 & datacomp$dataobs < 100,] # grassland low

datacomp$evt[datacomp$dataobs<5]

par(mfrow=c(3,3))
for(i in 1:9)
{
  plot(datacomp$coverProp,datacomp[,i+3],ylab=names(datacomp)[i+3],
       ylim=c(0,500),xlim=c(0,.05))
}

for(i in 10:18)
{
  plot(datacomp$coverProp,datacomp[,i+3],ylab=names(datacomp)[i+3],
       ylim=c(0,500),xlim=c(0,.05))
}

######
# next, account for each fuel loading catgory by expanding the columns in datacomp and including a loop over the fuel category columns, adapting the code above






