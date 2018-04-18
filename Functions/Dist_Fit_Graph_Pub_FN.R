#######
# function to graph distribution fit
# For publication
#######

distFitForPubGraph.fn<-function(data.file,evts,evt.col,cur.cols,
                          write.file=FALSE,file.name="DistFitGraph.pdf",
                          include0=FALSE,add.val=0.1,distr="norm",x.lab,plot.qqAxes=FALSE,
                          main.txt=NA,removeOut=FALSE,out.mult=4)
{
  if(write.file)
    pdf(file=file.name)
  
  for(i in evts)
  {
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==i,j]
      if(include0)
        cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0]+add.val
      else
        cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
      if(removeOut) # exclude outliers from distribution estimation
        cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]

      fit.obj<-switch(distr,
                      norm=fitdist(cur.loads.vals,distr="norm"),
                      lnorm=fitdist(cur.loads.vals,distr="lnorm"),
                      gamma=fitdist(cur.loads.vals,distr="gamma"),
                      weibull=fitdist(cur.loads.vals,distr="weibull"),
                      pareto=fitdist(cur.loads.vals,distr="pareto"))
      par(mar=c(3.5,3.75,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
      denscomp(fit.obj,lwd=2,xlab=x.lab,addlegend = FALSE,#legendtext="Estimated distribution",
               datacol = "grey",fitcol="black",main=main.txt,cex.axis=0.75,cex.main=0.75)#,fitlty=2)
      par(new=TRUE,fig=c(0.55,0.99,0.4,0.90),mar=c(2.5,2.5,0.1,0.1))
#      par(new=TRUE,fig=c(0.45,0.9,0.25,0.8),mar=c(2.5,2.5,0.1,0.1))
      qqcomp(fit.obj,fitpch = 16,fitcol="black",addlegend=FALSE,
             axes=FALSE,xlab="",ylab="",main="")#,legendtext = distr)
      box()
      if(plot.qqAxes)
      {
        axis(1,cex.axis=0.5)
        mtext(side=1,text="Theoretical quantiles",cex=0.5,line=1.0)
        axis(2,cex.axis=0.5)
        mtext(side=2,text="Empirical quantiles",cex=0.5,line=1.0,las=0)
      }
    }
  }
  if(write.file)
    dev.off()
}

distFitForSupplementaryGraph.fn<-function(data.file,evts,evt.col,cur.cols,
                                write.file=FALSE,file.name="DistFitGraph.pdf",
                                include0=FALSE,add.val=0.1,distr="norm",x.lab,plot.qqAxes=FALSE,
                                main.txt=NA,removeOut=FALSE,out.mult=4)
{
  if(write.file)
    pdf(file=file.name)
  
  for(i in evts)
  {
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==i,j]
      if(include0)
        cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0]+add.val
      else
        cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
      if(removeOut) # exclude outliers from distribution estimation
        cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]
      fit.obj<-switch(distr,
                      norm=fitdist(cur.loads.vals,distr="norm"),
                      lnorm=fitdist(cur.loads.vals,distr="lnorm"),
                      gamma=fitdist(cur.loads.vals,distr="gamma"),
                      weibull=fitdist(cur.loads.vals,distr="weibull"),
                      pareto=fitdist(cur.loads.vals,distr="pareto"))
      par(mfrow=c(1,3),mar=c(3.5,3.5,1.5,0.5),mgp=c(2.25,0.5,0),las=1,oma=c(0,0,2,0))
      denscomp(fit.obj,lwd=2,xlab=x.lab,legendtext="Estimated distribution",
               datacol = "grey",fitcol="black",main="")#,fitlty=2)
      qqcomp(fit.obj,fitpch = 16,fitcol="black",legendtext = distr)
      cdfcomp(fit.obj,datapch = 16,fitcol="black",legendtext = distr,xlab=x.lab)
      mtext(side=3,outer=TRUE,line=0.25,text=main.txt)
    }
  }
  if(write.file)
    dev.off()
}