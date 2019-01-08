my.plot.sobol<-function(n.var=6,sobol.obj,x.lab="",y.lab="Sensitivity index",y.lim=c(-0.15,1),
                        main.txt)
{
  plot(1:n.var,sobol.obj$S$original,axes=FALSE,ylim=y.lim,pch=16,
       xlab=x.lab,ylab=y.lab,
       main=main.txt,cex.main=0.7)
  axis(1,at=1:nrow(sobol.obj$S),labels=rownames(sobol.obj$S),
       las=2,cex.lab=0.9,tick = F)
  segments(x0 = 1:nrow(sobol.obj$S),x1=1:nrow(sobol.obj$S),
           y0=sobol.obj$S$`min. c.i.`,y1=sobol.obj$S$`max. c.i.`)
  axis(2,las=1)
  abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  box()
  
}

my.plot.prcc<-function(n.var=6,prcc.obj, x.lab="",y.lab="Partial rank correlation coefficient",
                       y.lim=c(-0.15,1),main.txt)
{
  plot(1:n.var,prcc.obj$PRCC[,1],ylim=y.lim,
       axes=FALSE,ylab=y.lab,xlab="",pch=16,
       main=main.txt,cex.main=0.7)
  segments(x0 = 1:n.var,y0=prcc.obj$PRCC[,4],
           x1 = 1:n.var,y1=prcc.obj$PRCC[,5])
  axis(2)
  axis(1,at=1:n.var,labels=rownames(prcc.obj$PRCC),las=2)
  abline(h=0,lwd=2,col="grey")
  box()
  
}