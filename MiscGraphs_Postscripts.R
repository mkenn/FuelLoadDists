# Miscellaneous Graph Code
# Below are the postscripts that were used to graphs the distribution fitting of particular EVTs and fuel loading types





# outside function, pick evt, pick loading with good fit
# repeat fit object - repeat cur.loads j index (reset j to the specific loading type)
# based on fit, copy paste fit
# example
#j<-13
#tmp.loads<-data[data[,evt.col]==666,j]
#cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0]
#gamma.fit<-fitdist(cur.loads.vals,distr="gamma")

# Where fit1b is your fitdistr object
#postscript(file = "ExampleFuelFit.eps",height=4,width=8,onefile=FALSE,horizontal=FALSE)
#par(mfrow=c(1,3),oma=c(2,0,0,0))
#denscomp(gamma.fit,lwd=2,xlab="Litter loading (Mg/ha)",legendtext="Gamma distribution",datacol = "grey")
#qqcomp(gamma.fit,fitpch = 16,legendtext = "Gamma distribution")
#cdfcomp(gamma.fit,xlab="Litter loading (Mg/ha)",legendtext = "Gamma distribution")
#mtext(text="Gamma distribution fit to litter loading (>0 Mg/ha) for US Eastern Floodplain vegetation type",outer=TRUE,side=1)
#dev.off()


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
