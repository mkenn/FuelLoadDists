#########
# functions to rank OAT SA output
#########

# sensitivity indices adapted from Hamby
# sens.index is the output percent change (max-min)/min
# imp.index is the ratio of the variance of the input to the variance of the output (so lower values = more sensitive)
# relDev is the relative deviance, or the ratio of the std.dev of the output to its mean (aka CV)
# relDevRat is the ratio of relative deviances, or the ratio of the CV of the output to the CV of the input
# create a data frame to contain the sensitivity indices for each loading for each emissions output
calc.indices.fn<-function(all.out.var,var.sa,consume.in,consume.out) # calculates sensitivity indices
{
  sa.indices<-list()
  for(k in 1:length(all.out.var))
  {
    cur.out.var=all.out.var[k]
    cor.sa<-data.frame(var.sa,cor=NA,RelChange=NA,sens.index=NA,imp.index=NA,
                       relDev=NA,relDevRat=NA,relSlope=NA,slope=NA)
    for(i in 1:length(var.sa))
    {
      
      cur.in.vec<-consume.in[as.character(consume.out$Filename)==var.sa[i],]
      cur.out.vec<-consume.out[as.character(consume.out$Filename)==var.sa[i],]
      cor.sa$cor[i]<-cor(cur.in.vec[,var.sa[i]],cur.out.vec[,cur.out.var])
      cor.sa$RelChange[i]<-(max(cur.out.vec[,cur.out.var])-min(cur.out.vec[,cur.out.var]))/(max(cur.in.vec[,var.sa[i]])-min(cur.in.vec[,var.sa[i]]))
      cor.sa$sens.index[i]<-(max(cur.out.vec[,cur.out.var])-min(cur.out.vec[,cur.out.var]))/(min(cur.in.vec[,var.sa[i]]))
      cor.sa$imp.index[i]<-(var(cur.in.vec[,var.sa[i]])/var(cur.out.vec[,cur.out.var]))
      cor.sa$relDev[i]<-sqrt(var(cur.out.vec[,cur.out.var]))/mean(cur.out.vec[,cur.out.var])
      cor.sa$relDevRat[i]<-(sqrt(var(cur.out.vec[,cur.out.var]))/mean(cur.out.vec[,cur.out.var]))/(sqrt(var(cur.in.vec[,var.sa[i]]))/mean(cur.in.vec[,var.sa[i]]))
      tmp.vec<-cur.in.vec[,var.sa[i]]/sqrt(var(cur.in.vec[,var.sa[i]]))
      tmp.lm<-try(lm(cur.out.vec[,cur.out.var]~tmp.vec))
      cor.sa$relSlope[i]<-tmp.lm$coefficients[2]
      
      tmp.lm<-lm(cur.out.vec[,cur.out.var]~cur.in.vec[,var.sa[i]])
      cor.sa$slope[i]<-tmp.lm$coefficients[2]
    }
    
    sa.indices[[k]]<-cor.sa
  }
  names(sa.indices)<-all.out.var
  sa.indices
}

order.indices.fn<-function(sa.indices,var.sa) # orders the variables from most to least sensitive for each SA index
{
  order.sa.list<-list()
  for(k in 1:length(sa.indices))
  {
    order.var<-as.data.frame(matrix(NA,ncol=ncol(sa.indices[[k]])-2,nrow=length(var.sa)))
    names(order.var)<-names(sa.indices[[k]])[-(1:2)]
    for(i in 3:ncol(sa.indices[[k]]))
    {
      tmp.cor<-sa.indices[[k]][!is.na(sa.indices[[k]][,i]),]
      if(names(sa.indices[[k]])[i]!="imp.index")
        tmp.sort<-sort.int(tmp.cor[,i],index.return=TRUE,decreasing=TRUE) # note importance index is decreasing not increasing
      else
        tmp.sort<-sort.int(tmp.cor[,i],index.return=TRUE,decreasing=FALSE) # note importance index is decreasing not increasing
      if(length(tmp.sort$ix)<length(var.sa))
      {
        tmp.ord<-as.character(tmp.cor[tmp.sort$ix,1])
        tmp.ord<-c(tmp.ord,as.character(sa.indices[[k]][is.na(sa.indices[[k]][,i]),1]))
        order.var[,i-2]<-tmp.ord
      }
      else
        order.var[,i-2]<-as.character(sa.indices[[k]][tmp.sort$ix,1])
    }
    order.sa.list[[k]]<-order.var
  }
  names(order.sa.list)<-names(sa.indices)
  order.sa.list
}

rank.indices.fn<-function(order.sa.list,var.sa) # returns ranks for each loading variable
{
  rank.sa.list<-list()
  for(k in 1:length(order.sa.list))
  {
    rank.var<-as.data.frame(matrix(NA,ncol=ncol(order.sa.list[[k]]),nrow=length(var.sa)))
    for(i in 1:ncol(order.sa.list[[k]]))
    {
      for(j in 1:length(var.sa))
      {
        #   tmp.id<-
        rank.var[j,i]<-which(order.sa.list[[k]][,i]==var.sa[j])
      }
    }
    names(rank.var)<-names(order.sa.list[[k]]) 
    rownames(rank.var)<-var.sa
    rank.sa.list[[k]]<-rank.var
  }
  names(rank.sa.list)<-names(order.sa.list)
  rank.sa.list
}

rank.summary.fn<-function(rank.sa.list,var.sa)
{
  rank.summary.sa.list<-list()
  for(k in 1:length(rank.sa.list))
  {
    rank.summary<-as.data.frame(matrix(ncol=length(var.sa)+2,nrow=length(var.sa)))
    #  rank.summary[,1]<-var.sa
    name1<-"prop1"
    for(i in 2:length(var.sa))
      name1<-c(name1,paste("prop",i,sep=""))
    names(rank.summary)<-c(name1,"medRank","meanRank")
    rownames(rank.summary)<-var.sa
    for(i in 1:length(var.sa))
    {
      for(j in 1:(ncol(rank.summary)-2))
      {
        rank.summary[i,j]<-length(rank.sa.list[[k]][i,rank.sa.list[[k]][i,]==j])/ncol(rank.sa.list[[k]])
      }
      rank.summary$medRank[i]<-median(as.numeric(rank.sa.list[[k]][i,]))
      rank.summary$meanRank[i]<-mean(as.numeric(rank.sa.list[[k]][i,]))
    }
    
    rank.summary.sa.list[[k]]<-rank.summary
  }
  names(rank.summary.sa.list)<-names(rank.sa.list)
  rank.summary.sa.list
}