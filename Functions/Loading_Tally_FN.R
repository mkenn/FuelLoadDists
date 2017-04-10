# Loading Tally Function
# The function Will determine the number of observations per loading type, ignoring the evts
# this will help determine:
# holes in dataset, loadings to be used in pair correlation, sensitivity analysis and distribution fitting

# loading type names
# "os_loading","ms_loading","us_loading","tree_loading"
# "snag_loading","shrub_loading","herb_loading","herb_percentlive","moss_loading","lichen_loading"
# "litter_loading","litter_depth","duff_loading","duff_depth"
# "X1hr_loading","X10hr_loading","X100hr_loading","fwd_loading",
# "X1000hrS_loading","X1000hrR_loading","X1000hr_loading","X10000hrS_loading","X10000hrR_loading","X10000hr_loading"
# "GT10000hrS_loading","GT10000hrR_loading","GT10000hr_loading","cwd_sound_loading","cwd_rotten_loading","cwd_loading"

loading.tally.fn<-function(data,start.col=3,cur.cols=c(start.col:ncol(data)),write.file=FALSE,file.name="LoadingTally.csv",show.plot=FALSE,plot.name="LoadingTallyGraph.pdf")
{
  tally.load<-data.frame(fueltype=names(data)[cur.cols],tally=NA)
  
  for(i in cur.cols)
  {
    tally.load[i-(start.col-1),2]<-length(data[!is.na(data[,i]),start.col])
  }
  
  if(write.file)
    write.csv(tally.load,file=file.name,row.names=FALSE)
  
  if(show.plot)
  {
    pdf(file=paste(plot.name,sep=""))
    plot(tally.load)
    dev.off()
  }
  
  return(tally.load)
}
