############
# evaluate first Consume SA
# EVTs 642 and 660
############
# library(sensitivity)
# sobol2007, etc, investigate
consume.out<-read.csv("SAResults/OAT_EVT_642_660/consume_sa_results.csv")
consume.in<-read.csv("SAResults/OAT_EVT_642_660/consume_sa_generated.csv",skip=1)

var.sa<-unique(as.character(consume.out$Filename))[3:24]
var.sa<-var.sa[var.sa!="w_rotten_gt20_loading"]
all.out.var<-c("PM25.Emissions","PM10.Emissions","PM.Emissions","CH4.Emissions","CO.Emissions","NMHC.Emissions")
cur.out.var<-"PM10.Emissions"
cur.out.var<-"PM.Emissions"
cur.out.var<-"CH4.Emissions"
cur.out.var<-"CO.Emissions"
par(mfrow=c(3,3))
cor.sa<-data.frame(var.sa,cor.PM10.642=NA,cor.PM10.660=NA,RelChange.PM10.642=NA,RelChange.PM10.660=NA,
                   sens.index.PM10.642=NA,sens.index.PM10.660=NA,imp.index.PM10.642=NA,imp.index.PM10.660=NA,
                   relDev.PM10.642=NA,relDev.PM10.660=NA,relDevRat.PM10.642=NA,relDevRat.PM10.660=NA,
                   relSlope.PM10.642=NA,relSlope.PM10.660=NA,slope.PM10.642=NA,slope.PM10.660=NA)

#lm.beta<-
# function (MOD) 
# {
#   b <- summary(MOD)$coef[-1, 1]
#   sx <- sd(MOD$model[,-1])
#   sy <- sd(MOD$model[,1])
#   beta <- b * sx/sy
#   return(beta)
# }

# sensitivity indices adapted from Hamby
# sens.index is the output percent change (max-min)/min
# imp.index is the ratio of the variance of the input to the variance of the output (so lower values = more sensitive)
# relDev is the relative deviance, or the ratio of the std.dev of the output to its mean (aka CV)
# relDevRat is the ratio of relative deviances, or the ratio of the CV of the output to the CV of the input
#for(i in 1:16)
pdf(file="SAResults/OAT_EVT_642_660/ScatterAll.pdf")
par(mfrow=c(3,4),mar=c(3.5,3.5,0.1,0.1),mgp=c(2.5,0.5,0))
for(k in all.out.var)
{ 
  par(mfrow=c(3,4))
  for(i in 1:length(var.sa))
  {
    cur.in.vec<-consume.in[as.character(consume.out$Filename)==var.sa[i],]
    cur.out.vec<-consume.out[as.character(consume.out$Filename)==var.sa[i],]
    plot(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds<15000,k],
         xlab=var.sa[i],ylab=k,ylim=c(min(consume.out[as.character(consume.out$Filename)==var.sa[i],k]),
                                                max(consume.out[as.character(consume.out$Filename)==var.sa[i],k])))
    points(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds>15000,k],pch=16)#,lwd=2,lty=1)#,type="b"
  }
}
dev.off()
for(i in 1:length(var.sa))
{
  cur.in.vec<-consume.in[as.character(consume.out$Filename)==var.sa[i],]
  cur.out.vec<-consume.out[as.character(consume.out$Filename)==var.sa[i],]
#  plot(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var],
#       xlab=var.sa[i],ylab=cur.out.var,ylim=c(min(consume.out[as.character(consume.out$Filename)==var.sa[i],cur.out.var]),max(consume.out[as.character(consume.out$Filename)==var.sa[i],cur.out.var])))
#  points(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var],pch=16,
#       xlab=var.sa[i],ylab=cur.out.var,ylim=c(min(consume.out[as.character(consume.out$Filename)==var.sa[i],cur.out.var]),max(consume.out[as.character(consume.out$Filename)==var.sa[i],cur.out.var])))
  cor.sa$cor.PM10.642[i]<-cor(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var])
  cor.sa$cor.PM10.660[i]<-cor(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]],cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var])
  cor.sa$RelChange.PM10.642[i]<-(max(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var])-min(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))/(max(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]])-min(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))
  cor.sa$RelChange.PM10.660[i]<-(max(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var])-min(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))/(max(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]])-min(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))
  cor.sa$sens.index.PM10.642[i]<-(max(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var])-min(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))/min(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var])
  cor.sa$sens.index.PM10.660[i]<-(max(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var])-min(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))/(min(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))
  cor.sa$imp.index.PM10.642[i]<-(var(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]])/var(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))
  cor.sa$imp.index.PM10.660[i]<-(var(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]])/var(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))
  cor.sa$relDev.PM10.642[i]<-sqrt(var(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))/mean(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var])
  cor.sa$relDev.PM10.660[i]<-sqrt(var(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))/mean(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var])
  cor.sa$relDevRat.PM10.642[i]<-(sqrt(var(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))/mean(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]))/(sqrt(var(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))/mean(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))
  cor.sa$relDevRat.PM10.660[i]<-(sqrt(var(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))/mean(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]))/(sqrt(var(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]]))/mean(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]]))
  tmp.vec<-cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]/sqrt(var(cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]]))
  tmp.lm<-try(lm(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]~tmp.vec))
  cor.sa$relSlope.PM10.642[i]<-tmp.lm$coefficients[2]
  tmp.vec<-cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]]/sqrt(var(cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]]))
  tmp.lm<-lm(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]~tmp.vec)
  cor.sa$relSlope.PM10.660[i]<-tmp.lm$coefficients[2]
  
  tmp.lm<-lm(cur.out.vec[cur.out.vec$Fuelbeds<15000,cur.out.var]~cur.in.vec[cur.out.vec$Fuelbeds<15000,var.sa[i]])
  cor.sa$slope.PM10.642[i]<-tmp.lm$coefficients[2]

  tmp.lm<-lm(cur.out.vec[cur.out.vec$Fuelbeds>15000,cur.out.var]~cur.in.vec[cur.out.vec$Fuelbeds>15000,var.sa[i]])
  cor.sa$slope.PM10.660[i]<-tmp.lm$coefficients[2]
}

############
# rank the variables by each of the 
# sensitivity measures, for each evt
############
order.var<-as.data.frame(matrix(NA,ncol=14,nrow=length(var.sa)))
names(order.var)<-names(cor.sa)[4:ncol(cor.sa)]
for(i in 4:ncol(cor.sa))
{
  tmp.cor<-cor.sa[!is.na(cor.sa[,i]),]
  if(names(cor.sa)[i]!="imp.index.PM10.642"&names(cor.sa)[i]!="imp.index.PM10.660")
    tmp.sort<-sort.int(tmp.cor[,i],index.return=TRUE,decreasing=TRUE) # note importance index is decreasing not increasing
  else
    tmp.sort<-sort.int(tmp.cor[,i],index.return=TRUE,decreasing=FALSE) # note importance index is decreasing not increasing
  if(length(tmp.sort$ix)<length(var.sa))
  {
    tmp.ord<-as.character(tmp.cor[tmp.sort$ix,1])
    tmp.ord<-c(tmp.ord,as.character(cor.sa[is.na(cor.sa[,i]),1]))
    order.var[,i-3]<-tmp.ord
  }
  else
    order.var[,i-3]<-as.character(cor.sa[tmp.sort$ix,1])
}
write.csv(order.var,file=paste("SAResults/OAT_EVT_642_660/VariableRanking",cur.out.var,".csv",sep=""),row.names=FALSE)

# retrieve rankings for each variable for each sensitivity measure
rank.var<-as.data.frame(matrix(NA,ncol=14,nrow=length(var.sa)))
for(i in 1:ncol(order.var))
{
  for(j in 1:length(var.sa))
  {
 #   tmp.id<-
    rank.var[j,i]<-which(order.var[,i]==var.sa[j])
  }
}
names(rank.var)<-names(order.var)
colSums(rank.var)

# summarize rankings for each variable--how often number 1? how often top 5? Top10? Mean ranking? Median ranking?
rank.summary<-data.frame(var.sa,prop1=NA,prop2=NA,prop3=NA,prop4=NA,prop5=NA,prop6=NA,prop7=NA,prop8=NA,
                         prop9=NA,prop10=NA,prop11=NA,prop12=NA,prop13=NA,prop14=NA,prop15=NA,prop16=NA,prop17=NA,
                         prop18=NA,prop19=NA,prop20=NA,prop21=NA,medRank=NA,meanRank=NA)
for(i in 1:length(var.sa))
{
  for(j in 1:21)
  {
    rank.summary[i,j+1]<-length(rank.var[i,rank.var[i,]==j])/ncol(rank.var)
  }
  rank.summary$medRank[i]<-median(as.numeric(rank.var[i,]))
  rank.summary$meanRank[i]<-mean(as.numeric(rank.var[i,]))
}

top.five<-signif(rowSums(rank.summary[,2:6]),digits=3)
bottom.five<-signif(rowSums(rank.summary[,18:22]),digits=3)

# sort by median and mean ranks, and by top.five
tmp.sort<-sort.int(rank.summary$medRank,decreasing=FALSE,index.return = TRUE)
var.sort.med<-var.sa[tmp.sort$ix]
tmp.sort<-sort.int(rank.summary$meanRank,decreasing=FALSE,index.return = TRUE)
var.sort.mean<-var.sa[tmp.sort$ix]
tmp.sort<-sort.int(top.five,decreasing=TRUE,index.return = TRUE)
var.sort.top5<-var.sa[tmp.sort$ix]
tmp.sort<-sort.int(bottom.five,decreasing=FALSE,index.return = TRUE)
var.sort.bottom5<-var.sa[tmp.sort$ix]

cbind(var.sort.med,var.sort.mean,var.sort.top5,var.sort.bottom5)
