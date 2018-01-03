##########
# global SA with independent sampling based on database distributions
##########
library(sensitivity)
source("../Functions/SensitivityAnalysis_FN.R")
source("FunctionsForGlobalSA.R")
source("../Functions/EVT_Tally_FN.R")
evtFB.map<-read.csv("EVT_Fuelbed_xwalk.csv") # FCCSID is the fuelbed number, EVT_GP is the EVT group
AllLoads.df<-read.csv("../Data/loadingsByEVTGroup_20170518.csv") # fuels loading database
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",stringsAsFactors=TRUE) # skip the header line, this is the FCCS fuelbeds file to give baseline values for the fuel loadings
fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdated.csv",stringsAsFactors = FALSE,sep=",",header=TRUE)

evt.tallies<-EVT.tally.fn(data.file = AllLoads.df,evt.col = "LFEVTGroupCd",min.tally = 100)
# cur.evt<-660 # so this function will return a list of the same length of this vector,
# cur.evt<-684 # so this function will return a list of the same length of this vector,

for(j in evt.tallies$evt.min_tally)
{
  cur.evt<-j # so this function will return a list of the same length of this vector,
  range.vals<-samo.fn(data.file=AllLoads.df,evts=cur.evt,start.col=12,#cur.cols=c(12:ncol(AllLoads.df)),
                      evt.col="LFEVTGroupCd",min.n=30,q.lower=0.1,q.upper=0.9,write.file=FALSE,file.name="SensitivityAnalysis_")
  cur.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt]#
  cur.fb<-unique(cur.fb[!is.na(cur.fb)])#&!is.na(evtFB.map$FCCSID)][1]
  # conduct analysis for all associated baseline fuelbeds
  fuelLoadFiles<-list()
  results.sa<-list()
  
  pdf(file=paste("SAResults/Batch1Try/ConsumeSABatchRun",cur.evt,".pdf",sep=""))
  list.id<-0
  for(i in 1:length(cur.fb))
  {  
    # have to screen for fb's that are actually in the all.fb file
    if(length(which(all.fbs$fuelbed_number==cur.fb[i]))>0)
    {  
      list.id<-list.id+1
      fuelLoadFiles[[list.id]]<-sobolFuelsFile.fn(range.vals,range.id=1,nreps=100,fbLoadNames.df,
                                          all.fbs,base.fb=cur.fb[i],change.units=T)
      outfilename<-"FuelLoadInputSA.csv"
      loadInFile.head<-matrix(c("GeneratorName=FCCS 3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)# didn't like today's date
      # write the file with the proper header information
      write.table(loadInFile.head,file=outfilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
      # Now append that file with the generated loadings
      write.table(fuelLoadFiles[[list.id]][[1]],file=outfilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)
    
      env.in<-read.csv("sample_consume_input.csv") #####******* Need to change units to metric!?**********
      new.env.in<-env.in[1,]
      for(m in 2:nrow(fuelLoadFiles[[list.id]][[1]]))
      {
        new.env.in<-rbind(new.env.in,env.in[1,])
      }
      new.env.in$fuelbeds<-fuelLoadFiles[[list.id]][[1]]$fuelbed_number
      envoutfilename<-"EnvInputSA.csv"
      write.table(new.env.in,file=envoutfilename,row.names=FALSE,sep=",")
    
      system.call<-paste("python consume_batch.py natural",envoutfilename,  "-f", outfilename)
      system(system.call) # tells R to execute this system call in the working directory
      
      results.sa[[list.id]]<-read.csv("consume_results.csv") # writes to this file every time, replacing previous results
      # and calculate indices
      response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions")
      par(mfrow=c(1,3),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
      sens.list<-list()
      for(k in 1:length(response.vars))
      {
        sens.list[[k]]<-tell(fuelLoadFiles[[list.id]][[2]],(results.sa[[list.id]][,response.vars[k]]-mean(results.sa[[list.id]][,response.vars[k]]))/sd(results.sa[[list.id]][,response.vars[k]]))
        rownames(sens.list[[k]]$S)<-fuelLoadFiles[[list.id]][[3]]
        plot(1:nrow(sens.list[[k]]$S),sens.list[[k]]$S$original,axes=FALSE,ylim=c(-.15,1),pch=16,
             xlab="",ylab="Sensitivity index",main=paste("EVT:",cur.evt,"FB",cur.fb[i],"output:",response.vars[k]))#las=2)
        
        axis(1,at=1:nrow(sens.list[[k]]$S),labels=rownames(sens.list[[k]]$S),las=2,cex.lab=0.9,tick = F)
        segments(x0 = 1:nrow(sens.list[[k]]$S),x1=1:nrow(sens.list[[k]]$S),y0=sens.list[[k]]$S$`min. c.i.`,y1=sens.list[[k]]$S$`max. c.i.`)
        axis(2,las=1)
        abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
        box()
      }
      # should probably save results to an R image as we go
     
    }
  }
  dev.off() 
  save(fuelLoadFiles,results.sa,file=paste("SAResults/Batch1Try/GlobalSABatchEVT",cur.evt,".RData",sep=""))
}

#######
# update range.vals with full database instead
all.fuelTypes<-names(all.fbs)[5:47]
range.vals.all<-data.frame(fueltypes=as.character(names(all.fbs)[5:47]),q10=NA,q90=NA)
for(i in 1:nrow(range.vals.all))
{
  range.vals.all[i,2:3]<-quantile(all.fbs[,i+4],probs = c(.1,.9))
}

nreps<-1000
new.X1<-new.X2<-as.data.frame(matrix(NA,ncol=43,nrow=nreps/2))
names(new.X1)<-names(new.X2)<-names(all.fbs)[5:47]
for(i in 1:nrow(range.vals.all))
{
  new.X1[,i]<-runif(nreps/2,range.vals.all[i,2],range.vals.all[i,3])
  new.X2[,i]<-runif(nreps/2,range.vals.all[i,2],range.vals.all[i,3])
}

#new.sobol<-sobolEff(X1=new.X1,X2=new.X2)
new.sobol<-sobol2007(X1=new.X1,X2=new.X2,nboot=1000,return.var=names(all.fbs)[5:47])
new.fuel.loads<-all.fbs[1,]
for(i in 2:nrow(new.sobol$X))
  new.fuel.loads<-rbind(new.fuel.loads,all.fbs[1,])
new.fuel.loads[,5:47]<-new.sobol$X
new.fuel.loads$fuelbed_number<-1000+1:nrow(new.fuel.loads)
outfilename<-"FuelLoadInputSA.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS 3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)# didn't like today's date
# write the file with the proper header information
write.table(loadInFile.head,file=outfilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# Now append that file with the generated loadings
write.table(new.fuel.loads,file=outfilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)

env.in<-read.csv("sample_consume_input.csv") #####******* Need to change units to metric!?**********
new.env.in<-env.in[1,]
for(m in 2:nrow(new.fuel.loads))
{
  new.env.in<-rbind(new.env.in,env.in[1,])
}
new.env.in$fuelbeds<-new.fuel.loads$fuelbed_number
envoutfilename<-"EnvInputSA.csv"
write.table(new.env.in,file=envoutfilename,row.names=FALSE,sep=",")

system.call<-paste("python consume_batch.py natural",envoutfilename,  "-f", outfilename)
system(system.call) # tells R to execute this system call in the working directory
results.all<-read.csv("consume_results.csv") # writes to this file every time, replacing previous results
# and calculate indices
response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions")
par(mfrow=c(1,3),mar=c(12,4,1.5,0.5),mgp=c(2,0.5,0))
sens.list.all<-list()
for(k in 1:length(response.vars))
{
  # center the response at 0 and scale by the sd
  sens.list.all[[k]]<-tell(new.sobol,(results.all[,response.vars[k]]-mean(results.all[,response.vars[k]]))/sd(results.all[,response.vars[k]]))#-mean(results.all[,response.vars[k]]))/sd(results.all[,response.vars[k]]))
  # rownames(sens.list[[k]]$S)<-fuelLoadFiles[[list.id]][[3]]
  # plot(1:nrow(sens.list.all[[k]]$S),sens.list.all[[k]]$S$original,axes=FALSE,ylim=c(-.15,1),pch=16,
  #       xlab="",ylab="Sensitivity index",main=paste("EVT:",cur.evt,"FB",cur.fb[i],"output:",response.vars[k]))#las=2)
  #  
  #  axis(1,at=1:nrow(sens.list.all[[k]]$S),labels=rownames(sens.list.all[[k]]$S),las=2,cex.lab=0.9,tick = F)
  #  segments(x0 = 1:nrow(sens.list.all[[k]]$S),x1=1:nrow(sens.list.all[[k]]$S),y0=sens.list.all[[k]]$S$`min. c.i.`,y1=sens.list.all[[k]]$S$`max. c.i.`)
  #  axis(2,las=1)
  #  abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  #  box()
}
#duff upper loading and duff upper depth come out top here, for all three outputs

save(new.sobol,sens.list.all,results.all,file="GlobalSAFCCSFBRangesNrep1000.RData")
# caveats for this analysis: unrealistic combinations obviously possible. 
pdf(file="ConsumeGlobalSAExample1.pdf")
for(k in 1:3)
{
  par(mfrow=c(1,1),mar=c(12,3.5,1.5,0.5),mgp=c(2,0.5,0))
  plot(1:nrow(sens.list.all[[k]]$T),sens.list.all[[k]]$T$original,axes=FALSE,ylim=c(-0,1),pch=16,
       xlab="",ylab="Total Sobol sensitivity index",main=paste("Output:",response.vars[k]),
       xlim=c(1.5,nrow(sens.list.all[[k]]$T)))#las=2)
  
  axis(1,at=1:nrow(sens.list.all[[k]]$T),labels=rownames(sens.list.all[[k]]$T),las=2,cex.lab=0.9,tick = F)
  segments(x0 = 1:nrow(sens.list.all[[k]]$T),x1=1:nrow(sens.list.all[[k]]$T),
           y0=sens.list.all[[k]]$T$`min. c.i.`,y1=sens.list.all[[k]]$T$`max. c.i.`)
  axis(2,las=1)
  abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  box()
}
dev.off()
# now just try with all.fbs as the "samples"
names(all.fbs)
all.X1<-all.fbs[1:170,5:47]
all.X2<-all.fbs[171:340,5:47]
sobol.all<-sobolEff(X1=all.X1,X2=all.X2)
new.fuel.loads<-all.fbs
for(i in 2:22)
  new.fuel.loads<-rbind(new.fuel.loads,all.fbs)
new.fuel.loads[,5:47]<-sobol.all$X  
new.fuel.loads$fuelbed_number<-9000+1:nrow(new.fuel.loads)
outfilename<-"FuelLoadInputSA.csv"
loadInFile.head<-matrix(c("GeneratorName=FCCS 3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),ncol=3)# didn't like today's date
# write the file with the proper header information
write.table(loadInFile.head,file=outfilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)
# Now append that file with the generated loadings
write.table(new.fuel.loads,file=outfilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)

env.in<-read.csv("sample_consume_input.csv") #####******* Need to change units to metric!?**********
new.env.in<-env.in[1,]
for(m in 2:nrow(new.fuel.loads))
{
  new.env.in<-rbind(new.env.in,env.in[1,])
}
new.env.in$fuelbeds<-new.fuel.loads$fuelbed_number
envoutfilename<-"EnvInputSA.csv"
write.table(new.env.in,file=envoutfilename,row.names=FALSE,sep=",")

system.call<-paste("python consume_batch.py natural",envoutfilename,  "-f", outfilename)
system(system.call) # tells R to execute this system call in the working directory


results.all<-read.csv("consume_results.csv") # writes to this file every time, replacing previous results
# and calculate indices
response.vars<-c("PM.Emissions","CO.Emissions","CO2.Emissions")
par(mfrow=c(1,3),mar=c(12,3,1.5,0.5),mgp=c(2,0.5,0))
sens.list.all<-list()
for(k in 1:length(response.vars))
{
  sens.list.all[[k]]<-tell(sobol.all,(results.all[,response.vars[k]]-mean(results.all[,response.vars[k]]))/sd(results.all[,response.vars[k]]))
  # rownames(sens.list[[k]]$S)<-fuelLoadFiles[[list.id]][[3]]
  # plot(1:nrow(sens.list[[k]]$S),sens.list[[k]]$S$original,axes=FALSE,ylim=c(-.15,1),pch=16,
  #      xlab="",ylab="Sensitivity index",main=paste("EVT:",cur.evt,"FB",cur.fb[i],"output:",response.vars[k]))#las=2)
  # 
  # axis(1,at=1:nrow(sens.list[[k]]$S),labels=rownames(sens.list[[k]]$S),las=2,cex.lab=0.9,tick = F)
  # segments(x0 = 1:nrow(sens.list[[k]]$S),x1=1:nrow(sens.list[[k]]$S),y0=sens.list[[k]]$S$`min. c.i.`,y1=sens.list[[k]]$S$`max. c.i.`)
  # axis(2,las=1)
  # abline(h=0,lwd=2,col="grey") # to see if CI for SI is above 0
  # box()
}
# No discrimination in sensitivity here, all negative and all very similar!
