#######
# function to graph distribution fit
#######

distFitGraph.fn<-function(data.file,evts,evt.col,cur.cols,
                          write.file=FALSE,file.name="DistFitGraph.pdf",main.txt=NA,
                          include0=FALSE,add.val=0.1,distr="norm",removeOut=FALSE,out.mult=4)
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
                      logNormal=fitdist(cur.loads.vals,distr="lnorm"),
                      gamma=fitdist(cur.loads.vals,distr="gamma"),
                      weibull=fitdist(cur.loads.vals,distr="weibull"),
                      pareto=fitdist(cur.loads.vals,distr="pareto"))
      par(mfrow=c(1,3),oma=c(2,0,0,0))
      denscomp(fit.obj,lwd=2,xlab=names(data.file)[j],legendtext="distr",datacol = "grey")
      qqcomp(fit.obj,fitpch = 16,legendtext = distr)
      cdfcomp(fit.obj,xlab=names(data.file)[j],legendtext = distr)
      mtext(text=paste(distr,"fit to", names(data.file)[j],"for",i),outer=TRUE,side=1)
    }
  }
  if(write.file)
    dev.off()
}