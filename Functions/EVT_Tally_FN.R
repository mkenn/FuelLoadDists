# EVT Tally Function
# The function Will determine the number of observations per EVT
# Results will help determine which evts are used for following:
# pair correlations, higher dimensional analysis and potential distribution fitting.
# The function creates tally of all observations per evt, and graphs the tally
# min.tally is the minimum # of obs and will help determine how many evts are used
# max.tally is the maximum # of obs
# both the minimum and maximum were arbitrarily selected


EVT.tally.fn<-function(data.file,evt.col,write.file=FALSE,file.name="EVT_Tally.csv",min.tally=1000,max.tally=10,show.plot=FALSE,plot.name="EVT.Tally.Graph.pdf")
{
  EVT.tab<-as.data.frame(table(data.file[,evt.col]))
  
  # EVT tally to csv
  if(write.file)
    write.csv(EVT.tab,file=file.name,row.names = FALSE)
  
  # Graphing to pdf
  if(show.plot)
  {
    pdf(file=paste(plot.name,sep=""))
    plot(EVT.tab[,1],EVT.tab[,2],xlab=paste("EVT"),ylab=paste("# Observations"),main=paste("EVT Tally"))  
    dev.off()
  }
  
  return(list(evt.min_tally=EVT.tab[EVT.tab[,2]>min.tally,1],evt.max_tally=EVT.tab[EVT.tab[,2]<max.tally,1]))
  #return(EVT.tab)
}



