# Correlation Pairs Function
# this function will determine the correlation between different loading pairs at specific evts
# the function will first determine the number of co-occurences between each fuel loading
# then will determine the correlation between the fuel loadings, if greater than a specific number of co-occurences
# the function acts as a screen to determine which fuel loading pairs that potentially have a relationship
# this will determine the correlation between different pairs in the dataset
# the minimum co-occurence was arbitrarily chosen

corrpairs.fn<-function(data,evts,start.col=3,evt.col="LFEVTGroupCd_FINAL",min.co=10,write.file.cooccur=FALSE,cooccur.filename="Co-occurence_EVT",write.file.corr=FALSE,corr.filename="Correlation_EVT")
{
  cooccur.list<-list()
  corr.list<-list()
  
  for(i in 1:length(evts))
  {
    cooccur.list[[i]]<-matrix(NA,nrow=30,ncol=30)
    corr.list[[i]]<-matrix(NA,nrow=30,ncol=30)
    
    tmp.loads<-data[data[,evt.col]==evts[i],]
    
    ## co-occurence
    for(j in 3:(ncol(data)-1))
    {
      tmp.load1<-tmp.loads[!is.na(tmp.loads[,j]),]
      for(k in (j+1):ncol(data)) 
      {
        tmp.load2<-tmp.load1[!is.na(tmp.load1[,k]),1]
        cooccur.list[[i]][j-2,k-2]<-length(tmp.load2)
      }
    }
    
    ## correlation
    cor.id<-which(cooccur.list[[i]]>min.co,arr.ind=TRUE)
    
    for(l in 1:length(cor.id[,1]))
    {
      curloads<-tmp.loads[,c(cor.id[l,1]+2,cor.id[l,2]+2)]
      curloads<-curloads[!is.na(curloads[,1])&!is.na(curloads[,2]),]
      corr.list[[i]][cor.id[l,1],cor.id[l,2]]<-cor(x=curloads[,1],y=curloads[,2])
    }
    
    # writing cooccurence to csv
    cooccur.list[[i]]<-as.data.frame(cooccur.list[[i]])
    names(cooccur.list[[i]])<-names(data)[start.col:ncol(data)]
    row.names(cooccur.list[[i]])<-names(data)[start.col:ncol(data)]
    
    if(write.file.cooccur)
      write.csv(cooccur.list[[i]],file=paste(cooccur.filename,evts[i],".csv",sep=""))
    
    # writing correlation to csv
    corr.list[[i]]<-as.data.frame(corr.list[[i]])
    names(corr.list[[i]])<-names(data)[start.col:ncol(data)]
    row.names(corr.list[[i]])<-names(data)[start.col:ncol(data)]
    
    pdf(file=paste("LAROCQUE_624_DuffGraph.pdf",sep = ""))
    {
      plot(AllLoads[,15],AllLoads[,16],main="Correlation duff_loading and duff_depth",pch=1,xlab="duff_loading",ylab="duff_depth")
    }
    dev.off()
    
    
    
    #if(write.file.corr)
      #write.csv(corr.list[[i]],file=paste(corr.filename,evts[i],".csv",sep=""))
    
    
  }
  return(corr.list)
}


