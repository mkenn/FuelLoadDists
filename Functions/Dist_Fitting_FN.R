# Distribution Fitting Function
# This function will determine the loglikelihood of the distribution for each fuel loading type
# The function looks at four distributions: normal, lognormal, gamma, weibull
# The results are then entered into a dataframe that can compare likelihoods
# the minimum plot was arbitrarily selected

# After the function, there is script to create additional graphics to be use in presenations


dist.fit.fn<-function(data.file,evts,evt.col,start.col,min.plot=30,
                      write.file=FALSE,file.name="DistFitSummaryEVT",
                      include0=FALSE,add.val=0.1)
{
  distfit.df<-list()
  if(include0)
    distfitW0.df<-list()
  cur.cols=c(start.col:ncol(data.file))
  for(i in 1:length(evts))
  {
    # summary dataframe
    print(paste("starting evt",evts[i]))
    distfit.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.mu=NA,norm.sigma=NA,lgnormLL=NA,lnorm.mu=NA,lnorm.sigma=NA,gammaLL=NA,gamma.shape=NA,gamma.rate=NA,weibullLL=NA,weibull.shape=NA,weibull.scale=NA)
    if(include0)
      distfitW0.df[[i]]<-data.frame(fueltype=names(data.file)[cur.cols],normLL=NA,norm.mu=NA,norm.sigma=NA,lgnormLL=NA,lnorm.mu=NA,lnorm.sigma=NA,gammaLL=NA,gamma.shape=NA,gamma.rate=NA,weibullLL=NA,weibull.shape=NA,weibull.scale=NA)
    
    for(j in cur.cols)
    {
      tmp.loads<-data.file[data.file[,evt.col]==evts[i],j]
      if(include0)
        cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # perturb all values by 0.01
      cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
      
      if(length(cur.loads.vals)>min.plot)
      {
        # First exclude 0's
        ############# normal ##############
        normal.fit<-fitdist(cur.loads.vals,distr="norm")
        normal.ll<-normal.fit$loglik
      #  test.normal<-gofstat(normal.fit)
        distfit.df[[i]][j-(start.col-1),2]<-round(normal.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),3]<-round(normal.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),4]<-round(normal.fit$estimate[2],digits=2)

        ############# lognormal ##############
       lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")
        lnorm.ll<-lnorm.fit$loglik
      #  test.lnorm<-gofstat(lnorm.fit)
        distfit.df[[i]][j-(start.col-1),5]<-round(lnorm.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),6]<-round(lnorm.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),7]<-round(lnorm.fit$estimate[2],digits=2)

        ############# gamma ##############
        gamma.fit<-fitdist(cur.loads.vals,distr="gamma")
        gamma.ll<-gamma.fit$loglik
      #  test.gamma<-gofstat(gamma.fit)
        distfit.df[[i]][j-(start.col-1),8]<-round(gamma.ll,digits=0)
        distfit.df[[i]][j-(start.col-1),9]<-round(gamma.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),10]<-round(gamma.fit$estimate[2],digits=2)

        ############# weibull ##############
        weibull.fit<-fitdist(cur.loads.vals,distr="weibull")
        weibull.ll<-weibull.fit$loglik
     #   test.weibull<-gofstat(weibull.fit)
        distfit.df[[i]][j-(start.col-1),11]<-round(weibull.ll,digits = 0)
        distfit.df[[i]][j-(start.col-1),12]<-round(weibull.fit$estimate[1],digits=2)
        distfit.df[[i]][j-(start.col-1),13]<-round(weibull.fit$estimate[2],digits=2)

        ###
        # Including 0's
        ###
        if(include0)
        {
          ############# normal ##############
          normal.fit<-fitdist(cur.loads.vals0+add.val,distr="norm")
          normal.ll<-normal.fit$loglik
          #  test.normal<-gofstat(normal.fit)
          distfitW0.df[[i]][j-(start.col-1),2]<-round(normal.ll,digits=0)
          distfitW0.df[[i]][j-(start.col-1),3]<-round(normal.fit$estimate[1],digits=2)
          distfitW0.df[[i]][j-(start.col-1),4]<-round(normal.fit$estimate[2],digits=2)

          ############# lognormal ##############
          lnorm.fit<-fitdist(cur.loads.vals0+add.val,distr="lnorm") # lognormal does not accommodate non-zero entries
          lnorm.ll<-lnorm.fit$loglik
          #  test.lnorm<-gofstat(lnorm.fit)
          distfitW0.df[[i]][j-(start.col-1),5]<-round(lnorm.ll,digits=0)
          distfitW0.df[[i]][j-(start.col-1),6]<-round(lnorm.fit$estimate[1],digits=2)
          distfitW0.df[[i]][j-(start.col-1),7]<-round(lnorm.fit$estimate[2],digits=2)

          ############# gamma ##############
          gamma.fit<-fitdist(cur.loads.vals0+add.val,distr="gamma")
          gamma.ll<-gamma.fit$loglik
          #  test.gamma<-gofstat(gamma.fit)
          distfitW0.df[[i]][j-(start.col-1),8]<-round(gamma.ll,digits=0)
          distfitW0.df[[i]][j-(start.col-1),9]<-round(gamma.fit$estimate[1],digits=2)
          distfitW0.df[[i]][j-(start.col-1),10]<-round(gamma.fit$estimate[2],digits=2)

                    ############# weibull ##############
          weibull.fit<-fitdist(cur.loads.vals0+add.val,distr="weibull")
          weibull.ll<-weibull.fit$loglik
          #   test.weibull<-gofstat(weibull.fit)
          distfitW0.df[[i]][j-(start.col-1),11]<-round(weibull.ll,digits = 0)
          distfitW0.df[[i]][j-(start.col-1),12]<-round(weibull.fit$estimate[1],digits=2)
          distfitW0.df[[i]][j-(start.col-1),13]<-round(weibull.fit$estimate[2],digits=2)
        }
        
        if(write.file)
        {
          write.csv(distfit.df[[i]],file=paste(file.name,evts[i],".csv",sep = ""),row.names=FALSE)
        }
      }
    }
  }
  return(list(distfit.df,distfitW0.df))
}


# outside function, pick evt, pick loading with good fit
# repeat fit object - repeat cur.loads j index (reset j to the specific loading type)
# based on fit, copy paste fit
# example
#j<-18
#tmp.loads<-data.file[data.file[,evt.col]==666,j]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#gamma.fit<-fitdist(cur.loads.vals,distr="gamma")
#lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")

# Where fit1b is your fitdistr object
#postscript(file = "ExampleFuelFit.eps",height=4,width=8,onefile=FALSE,horizontal=FALSE)
#par(mfrow=c(1,3),oma=c(2,0,0,0))
#denscomp(gamma.fit,lwd=2,xlab="Litter loading (Mg/ha)",legendtext="Gamma distribution",datacol = "grey")
#qqcomp(gamma.fit,fitpch = 16,legendtext = "Gamma distribution")
#cdfcomp(gamma.fit,xlab="Litter loading (Mg/ha)",legendtext = "Gamma distribution")
#mtext(text="Gamma distribution fit to litter loading (>0 Mg/ha) for US Eastern Floodplain vegetation type",outer=TRUE,side=1)
#dev.off()

#par(mfrow=c(1,1),oma=c(2,0,0,0))
#denscomp(norm.fit,lwd=2,xlab="Shrub loading (Mg/ha)",
#  legendtext="Normal theoretical distribution",datacol = "grey")
#denscomp(lnorm.fit,lwd=2,xlab="Shrub loading (Mg/ha)",
#  legendtext="Log-normal theoretical distribution",datacol = "grey")
#qqcomp(gamma.fit,fitpch = 16,legendtext = "Gamma distribution")
#cdfcomp(gamma.fit,xlab="Litter loading (Mg/ha)",legendtext = "Gamma distribution")
#mtext(text="Gamma distribution fit to litter loading (>0 Mg/ha) for US Eastern Floodplain vegetation type",outer=TRUE,side=1)



### practice - evt=615, distirbution=weibull
#j<-9
#tmp.loads<-data[data[,evt.col]==615,j]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#weibull.fit<-fitdist(cur.loads.vals,distr="weibull")

#postscript(file = "615evt_shrub.eps",height=4,width=8,onefile=FALSE,horizontal=FALSE)
#par(mfrow=c(1,3),oma=c(2,0,0,0))
#denscomp(weibull.fit,lwd=2,xlab="Shrub Loading (Mg/ha)",legendtext="Weibull Distribution",datacol = "grey")
#qqcomp(weibull.fit,fitpch = 16,legendtext = "Weibull Distribution")
#cdfcomp(weibull.fit,xlab="Shrub Loading (Mg/ha)",legendtext = "Weibull Distribution")
#mtext(text="Weibull distribution fit to shrub loading (>0 Mg/ha) for Western Hemlock Forest vegetation type",outer=TRUE,side=1)
#dev.off()

#######################################################################################

### practice - evt=624, distribution=lognormal
#j<-15
#tmp.loads<-data[data[,evt.col]==624,j]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")

#postscript(file = "624evt_duff.eps",height=4,width=8,onefile=FALSE,horizontal=FALSE)
#par(mfrow=c(1,3),oma=c(2,0,0,0))
#denscomp(lnorm.fit,lwd=2,xlab="Duff Loading (Mg/ha)",legendtext="Lognormal Distribution",datacol = "grey")
#qqcomp(lnorm.fit,fitpch = 16,legendtext = "Lognorm Distribution")
#cdfcomp(lnorm.fit,xlab="Duff Loading (Mg/ha)",legendtext = "Lognormal Distribution")
#mtext(text="Lognorm distribution fit to duff loading (>0 Mg/ha) for Mesquite Woodland and Scrub vegetation type",outer=TRUE,side=1)
#dev.off()

#j<-18 #17
#tmp.loads<-data[data[,evt.col]==624,17]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#lnorm.fit<-fitdist(cur.loads.vals,distr="lnorm")

#names(data)
#tmp.loads<-data[data[,evt.col]==624,]#j:(j+1)]
#summary(tmp.loads)



#############
# Graphs for Nancy
#i=15
#tally.var[i-2,2]<-length(tmp.loads[!is.na(tmp.loads[,i]),3])
#cur.loads.vals<-tmp.loads[,as.character(tally.var$fueltype)[i-2]]

#postscript(file = "758evt_duff_box.eps",height=4,width=4,onefile=FALSE,horizontal=FALSE)
#boxplot(cur.loads.vals,ylab="Duff Loading (Mg/ha)")
#dev.off()

#postscript(file = "758evt_duff_hist.eps",height=4,width=4,onefile=FALSE,horizontal=FALSE)
#hist(cur.loads.vals,xlab="Duff Loading (Mg/ha)",main=paste("Duff Loading for Black Spruce Forest\nand Woodland"))
#dev.off()

#postscript(file = "758evt_duff_qq.eps",height=4,width=4,onefile=FALSE,horizontal=FALSE)
#qqnorm(cur.loads.vals,main="Duff Loading (Mg/ha)")
#qqline(cur.loads.vals)
#dev.off()

#postscript(file = "758evt_duff_boxwozero.eps",height=4,width=4,onefile=FALSE,horizontal=FALSE)
#boxplot(cur.loads.vals[cur.loads.vals>0],main="Duff Loading >0 (Mg/ha)")
#dev.off()


############3
#i=15
#tally.var[i-2,2]<-length(tmp.loads[!is.na(tmp.loads[,i]),3])
#cur.loads.vals<-tmp.loads[,as.character(tally.var$fueltype)[i-2]]
#pdf(file=paste("Nancy_DuffGraphs",evts[k],".pdf",sep=""))
#boxplot(cur.loads.vals,ylab=paste(tally.var$fueltype[i-2],"(Mg/ha)"),main="Duff Loading for Black Spruce Forest and Woodland")
#hist(cur.loads.vals,xlab=paste(tally.var$fueltype[i-2],"(Mg/ha)"),main=paste("Duff Loading for Black Spruce Forest and Woodland"))
#qqnorm(cur.loads.vals,main="Duff Loading for Black Spruce Forest and Woodland")
#qqline(cur.loads.vals)
#boxplot(cur.loads.vals[cur.loads.vals>0],ylab=paste(tally.var$fueltype[i-2],"(Mg/ha)"),main=paste(tally.var$fueltype[i-2],">0 (Mg/ha)"))
#dev.off()


###############################

### Graphs for Nancy - evt=758, distirbution=gamma
#j<-15
#tmp.loads<-data[data[,evt.col]==758,j]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#gamma.fit<-fitdist(cur.loads.vals,distr="gamma")

#postscript(file = "758evt_duff.eps",height=4,width=8,onefile=FALSE,horizontal=FALSE)
#par(mfrow=c(1,3),oma=c(2,0,0,0))
#denscomp(gamma.fit,lwd=2,xlab="Duff Loading (Mg/ha)",legendtext="Gamma Distribution",datacol = "grey")
#qqcomp(gamma.fit,fitpch = 16,legendtext = "Gamma Distribution")
#cdfcomp(gamma.fit,xlab="Duff Loading (Mg/ha)",legendtext = "Gamma Distribution")
#mtext(text="Gamma Distribution fit to duff loading (>0 Mg/ha) for Black Spruce Forest and Woodland",outer=TRUE,side=1)
#dev.off()

#?ks.test
