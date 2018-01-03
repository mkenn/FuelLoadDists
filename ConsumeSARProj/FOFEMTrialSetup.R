#########
# create input file for FOFEM from random sampling of 
# database
#########

library(sensitivity)
source("../Functions/SensitivityAnalysis_FN.R")
source("FunctionsForGlobalSA.R")
source("../Functions/EVT_Tally_FN.R")
evtFB.map<-read.csv("EVT_Fuelbed_xwalk.csv") # FCCSID is the fuelbed number, EVT_GP is the EVT group
AllLoads.df<-read.csv("../Data/CurrentData/_EnglishLoadingsCrosstab.csv") # fuels loading database
all.fbs<-read.table("consume/input_data/fccs_loadings.csv",skip=1,header=TRUE,sep=",",stringsAsFactors=TRUE) # skip the header line, this is the FCCS fuelbeds file to give baseline values for the fuel loadings
fbLoadNames.df<-read.table("FCCS_DataBaseCategoryMapUpdatedWFOFEM_English.csv",stringsAsFactors = FALSE,sep=",",header=TRUE)

# FOFEM file header:
# #Stand	Litter_tpa	1hr 	10hr	10FM	100hr	Log	1000FM	%Rotten	Distribution	Duff_tpa	DuffFM	DuffDepth	Herb	Shrub	Foliage	Branch	%Burn	Region	Group	Season	Category
# How best to represent the large fuels? Log = ? 1000-hr S, or 1000-hr total? %Rotten applied to ?
# Litter_tpa, duff_tpa -- translate to Mgha. I think these are supposed to be tons/acre?

# To run:
# FOF_GUI C | S | M infile outfile runfile errorfile [H]
# Select one of the following as the first argument:
#   C = Consumed/Emissions model
# S = Consumed/Emissions/Soil model
# M = Mortality model
# Set input


fofem.system.call<-"FOF_GUI C fofemIn.txt fofemOut.txt fofemRun.txt fofemErr.txt H"

fofem.infile<-"fofemIn.txt"
fofem.outfile<-"fofemOut.txt"

evt.tallies<-EVT.tally.fn(data.file = AllLoads.df,evt.col = "LFEVTGroupCd_FINAL",min.tally = 100)
# cur.evt<-660 # so this function will return a list of the same length of this vector,
# cur.evt<-684 # so this function will return a list of the same length of this vector,

for(j in evt.tallies$evt.min_tally)
{
  cur.evt<-j # so this function will return a list of the same length of this vector,
  range.vals<-samo.fn(data.file=AllLoads.df,evts=cur.evt,start.col=13,
                      evt.col="LFEVTGroupCd_FINAL",min.n=30,q.lower=0.1,q.upper=0.9,write.file=FALSE,file.name="SensitivityAnalysis_")
  cur.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt]#
  cur.fb<-unique(cur.fb[!is.na(cur.fb)])#&!is.na(evtFB.map$FCCSID)][1]
  # conduct analysis for all associated baseline fuelbeds
  fuelLoadFiles<-list()
  results.sa<-list()
  
  pdf(file=paste("SAResults/Batch1Try/FOFEMSABatchRun",cur.evt,".pdf",sep=""))
  list.id<-0
  for(i in 1:length(cur.fb))
  {  
    # have to screen for fb's that are actually in the all.fb file
    if(length(which(all.fbs$fuelbed_number==cur.fb[i]))>0)
    {  
      list.id<-list.id+1
      fuelLoadFiles[[list.id]]<-sobolFuelsFileFOFEM.fn(range.vals,range.id=1,nreps=100,fbLoadNames.df,
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
