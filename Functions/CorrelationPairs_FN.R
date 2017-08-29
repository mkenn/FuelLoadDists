# Correlation Pairs Function
# this function will determine the correlation between different loading pairs at specific evts
# the function will first determine the number of co-occurences between each fuel loading
# then will determine the correlation between the fuel loadings, if greater than a specific number of co-occurences
# the function acts as a screen to determine which fuel loading pairs that potentially have a relationship
# this will determine the correlation between different pairs in the dataset
# the minimum co-occurence was arbitrarily chosen

corrpairs.fn<-function(data.file,start.col,evts=NA,evt.col,min.co=10,write.file.cooccur=FALSE,cooccur.filename="Co-occurence_EVT",write.file.corr=FALSE,corr.filename="Correlation_EVT")
{
  cooccur.list<-list()
  corr.list<-list()
  
  if(is.na(evts)) # calculate correlations for whole database regardless
  {
    cooccur.list[[1]]<-matrix(NA,nrow=30,ncol=30)
    corr.list[[1]]<-matrix(NA,nrow=30,ncol=30)
    
    #resaves datafile. WHY? How about here we go ahead and make the tmp.loads only be loading
#    tmp.loads<-data.file
    #So:
    tmp.loads<-data.file[,start.col:ncol(data.file)]
    # and now we can just work j and k with tmp.loads
    
#    for(j in start.col:(ncol(data.file)-1))
     for(j in 1:(ncol(tmp.loads)-1))
      {
      # this part is not removing the NAs from each column, it does so when it is done by itself, not as the whole for loop
      tmp.load1<-tmp.loads[!is.na(tmp.loads[,j]),]
    
#      for(k in (j+1):ncol(data.file)) 
      for(k in (j+1):ncol(tmp.loads)) 
      {
        tmp.load2<-tmp.load1[!is.na(tmp.load1[,k]),1]
       
        cooccur.list[[1]][j,k]<-length(tmp.load2)
        
#        cooccur.list[[1]][j-(start.col-1),k-(start.col-1)]<-length(tmp.load2)
      }
    }
    
    ## correlation
    cor.id<-which(cooccur.list[[1]]>min.co,arr.ind=TRUE)
    
    for(l in 1:length(cor.id[,1]))
    {
#      curloads<-tmp.loads[,c(cor.id[l,1]+2,cor.id[l,2]+2)]
      curloads<-tmp.loads[,c(cor.id[l,1],cor.id[l,2])]
      curloads<-curloads[!is.na(curloads[,1])&!is.na(curloads[,2]),]
      if(sd(curloads[,1])>0&sd(curloads[,2])>0)
        corr.list[[1]][cor.id[l,1],cor.id[l,2]]<-cor(x=curloads[,1],y=curloads[,2])
     }
    
    # writing cooccurence to csv
    cooccur.list[[1]]<-as.data.frame(cooccur.list[[1]])
    names(cooccur.list[[1]])<-names(data.file)[start.col:ncol(data.file)]
    row.names(cooccur.list[[1]])<-names(data.file)[start.col:ncol(data.file)]
    
    if(write.file.cooccur)
      write.csv(cooccur.list[[1]],file=paste(cooccur.filename,".csv",sep=""))
    
    # writing correlation to csv
    corr.list[[1]]<-as.data.frame(corr.list[[1]])
    names(corr.list[[1]])<-names(data.file)[start.col:ncol(data.file)]
    row.names(corr.list[[1]])<-names(data.file)[start.col:ncol(data.file)]
  }
  
  else
  {  
    for(i in 1:length(evts))
    {
      cooccur.list[[i]]<-matrix(NA,nrow=30,ncol=30)
      corr.list[[i]]<-matrix(NA,nrow=30,ncol=30)
      
#      tmp.loads<-data.file[data.file[,evt.col]==evts[i],]
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],start.col:ncol(data.file)]
      
      ## co-occurence
#      for(j in start.col:(ncol(data.file)-1))
      for(j in 1:(ncol(tmp.loads)-1))
        {
        tmp.load1<-tmp.loads[!is.na(tmp.loads[,j]),]
      
#        for(k in (j+1):ncol(data.file)) 
          for(k in (j+1):ncol(tmp.loads)) 
          {
          tmp.load2<-tmp.load1[!is.na(tmp.load1[,k]),1]
#          cooccur.list[[i]][j-2,k-2]<-length(tmp.load2)
          cooccur.list[[i]][j,k]<-length(tmp.load2)
          }
      }
      
      ## correlation
      cor.id<-which(cooccur.list[[i]]>min.co,arr.ind=TRUE)
      
      if(length(cor.id[,1])>0)
      {
        for(l in 1:length(cor.id[,1]))
        {
  #        curloads<-tmp.loads[,c(cor.id[l,1]+2,cor.id[l,2]+2)]
          curloads<-tmp.loads[,c(cor.id[l,1],cor.id[l,2])]
          curloads<-curloads[!is.na(curloads[,1])&!is.na(curloads[,2]),]
          if(sd(curloads[,1])>0&sd(curloads[,2])>0)
            corr.list[[i]][cor.id[l,1],cor.id[l,2]]<-cor(x=curloads[,1],y=curloads[,2])
          else
          {
            if(mean(curloads[,1])!=0&mean(curloads[,2])!=0)
            print(paste("Zero variability EVT:",evts[i],"loading: ",names(curloads)[1],names(curloads)[2]))
          }
          
        }
      }
      
      # writing cooccurence to csv
      cooccur.list[[i]]<-as.data.frame(cooccur.list[[i]])
      names(cooccur.list[[i]])<-names(data.file)[start.col:ncol(data.file)]
      row.names(cooccur.list[[i]])<-names(data.file)[start.col:ncol(data.file)]
      
       if(write.file.cooccur)
         write.csv(cooccur.list[[i]],file=paste(cooccur.filename,evts[i],".csv",sep=""))
      
      # writing correlation to csv
      corr.list[[i]]<-as.data.frame(corr.list[[i]])
    names(corr.list[[i]])<-names(data.file)[start.col:ncol(data.file)]
      row.names(corr.list[[i]])<-names(data.file)[start.col:ncol(data.file)]
      
      #pdf(file=paste("Graph_",evts,".pdf",sep = ""))
      #{
        #plot(AllLoads[,15],AllLoads[,16],main="Correlation_",names(data.file),"_and_",names(data.file),pch=1,xlab="duff_loading",ylab="duff_depth")
      #}
      #dev.off()
      
      if(write.file.corr)
        write.csv(corr.list[[i]],file=paste(corr.filename,evts[i],".csv",sep=""))
      
    }
  }
  return(corr.list)
}


