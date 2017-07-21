#########
# process consume OAT SA
#########
# starts with results.sa from script ConductSAFromR.R
#*****Need to convert from Mg/ha to tons/acre for consume runs, then back again**********
#*******depth probably from cm to in**************
consume.out<-read.csv(paste("SAResults/",resultsfilename,sep=""))
consume.in<-new.loads.file # created in the GenerateLoadingsFileOAT.R script
var.sa<-unique(as.character(consume.out$Filename))[-1] # the loadings varied in the SA
all.out.var<-c("PM25.Emissions","PM10.Emissions","PM.Emissions","CH4.Emissions","CO.Emissions","NMHC.Emissions") # these are the emissions output chosen as response variables
# cur.out.var<-"PM10.Emissions" # defined individually
# cur.out.var<-"PM.Emissions"
# cur.out.var<-"CH4.Emissions"
# cur.out.var<-"CO.Emissions"

# sensitivity indices adapted from Hamby
# sens.index is the output percent change (max-min)/min
# imp.index is the ratio of the variance of the input to the variance of the output (so lower values = more sensitive)
# relDev is the relative deviance, or the ratio of the std.dev of the output to its mean (aka CV)
# relDevRat is the ratio of relative deviances, or the ratio of the CV of the output to the CV of the input
# create a data frame to contain the sensitivity indices for each loading for each emissions output
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

############
# rank the variables by each of the 
# sensitivity measures, for each evt
############
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
# write to file
#write.csv(order.var,file=paste("SAResults/OAT_EVT_642_660/VariableRanking",cur.out.var,".csv",sep=""),row.names=FALSE)

# retrieve rankings for each variable for each sensitivity measure
rank.sa.list<-list()
for(k in 1:length(sa.indices))
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

#colSums(rank.var)

# summarize rankings for each variable--how often number 1? how often top 5? Top10? Mean ranking? Median ranking?
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
# write to file?

# top.five<-signif(rowSums(rank.summary[,1:5]),digits=3)
# bottom.five<-signif(rowSums(rank.summary[,6:10]),digits=3)

# sort by median and mean ranks, and by top.five
# tmp.sort<-sort.int(rank.summary$medRank,decreasing=FALSE,index.return = TRUE)
# var.sort.med<-var.sa[tmp.sort$ix]
# tmp.sort<-sort.int(rank.summary$meanRank,decreasing=FALSE,index.return = TRUE)
# var.sort.mean<-var.sa[tmp.sort$ix]
# tmp.sort<-sort.int(top.five,decreasing=TRUE,index.return = TRUE)
# var.sort.top5<-var.sa[tmp.sort$ix]
# tmp.sort<-sort.int(bottom.five,decreasing=FALSE,index.return = TRUE)
# var.sort.bottom5<-var.sa[tmp.sort$ix]
# 
# cbind(var.sort.med,var.sort.mean,var.sort.top5,var.sort.bottom5)
# 

