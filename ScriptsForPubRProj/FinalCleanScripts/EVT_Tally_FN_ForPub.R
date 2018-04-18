# EVT Tally Function
# The function Will determine the number of observations per EVT
# and identify those that meet a pre-specified threshold
# Results will help determine which evts are used for following:
# pair correlations, higher dimensional analysis and potential distribution fitting.
# The function creates tally of all observations per evt, and graphs the tally
# min.tally is the minimum # of obs and will help determine how many evts are used
# max.tally is the maximum # of obs
# both the minimum and maximum were arbitrarily selected


EVT.tally.fn<-function(data.file,evt.col,min.tally=100)
{
  EVT.tab<-as.data.frame(table(data.file[,evt.col])) # create a table that counts the occurrences of each EVT
  EVT.tab[,1]<-sort(unique(data.file[,evt.col]))
  
  return(list(evt.tally=EVT.tab,evt.min_tally=EVT.tab[EVT.tab[,2]>min.tally,1]))#,evt.max_tally=EVT.tab[EVT.tab[,2]<max.tally,1]))
}



