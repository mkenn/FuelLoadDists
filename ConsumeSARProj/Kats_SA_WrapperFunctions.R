##############
# wrapper function to conduct sensitivity analysis
# for consume and/or fofem
###############
makeCorrelatedSampleMat_Wrapper.fn<-function(data.file,fuel.ids,complete.case=TRUE,min.co.occur=30,
                                             evts,EVTCol,start.col,n.samp=1000,rankObj,fitObj,
                                             upper.quantile=NA,use.corr=TRUE,n.boot=1000,cur.evt.id)
{
  set.seed(NULL)
  sens.mats.list<-corr.sa.fn(data.file,fuel.ids=fuel.ids,complete.case=complete.case,min.co.occur=min.co.occur,
                             evts=evts,EVTCol=EVTCol,start.col=start.col,n.samp=n.samp,
                             rankObj=rankObj,fitObj=fitObj,upper.quantile=upper.quantile,
                             use.corr=use.corr)
  if(!is.na(sens.mats.list[[1]][[cur.evt.id]])[1])
  {
    cur.sobol<-sobolEff(X1=sens.mats.list[[1]][[cur.evt.id]][1:(n.samp/2),],
                        X2=sens.mats.list[[1]][[cur.evt.id]][(n.samp/2+1):n.samp,],nboot = n.boot)
    
    corr.samp.vals<-data.frame(cur.sobol$X)
    names(corr.samp.vals)<-names(sens.mats.list[[1]][[cur.evt.id]])
    
    uncor.cur.sobol<-sobolEff(X1=sens.mats.list[[2]][[cur.evt.id]][1:(n.samp/2),],
                              X2=sens.mats.list[[2]][[cur.evt.id]][(n.samp/2+1):n.samp,],nboot = n.boot)
    
    uncorr.samp.vals<-data.frame(uncor.cur.sobol$X)
    names(uncorr.samp.vals)<-names(sens.mats.list[[2]][[cur.evt.id]])
    
    return(list(sens.mats=sens.mats.list,sobol.obj=cur.sobol,uncor.sobol.obj=uncor.cur.sobol,
                corr.samp.vals.sobol=corr.samp.vals,corr.samp.vals.prcc=sens.mats.list[[1]][[cur.evt.id]],
                uncorr.samp.vals.sobol=uncorr.samp.vals,uncorr.samp.vals.prcc=sens.mats.list[[2]][[cur.evt.id]]))
  }
  else
  {
    print("Insufficient data coverage for this EVT")
    return(NA)
  }
}

#########
# now a wrapper for generating fuel inputs and calling the models
##########
ModSA_Wrapper.fn<-function(corr.samp.vals.sobol,corr.samp.vals.prcc,nreps=NA,fbLoadNames.df,
                           all.fbs,evtFB.map,cur.evt.num,base.fofem=NA,change.units=T,mod="C",phase="F",
                           infilename="FuelLoadInputSA.csv",fuel.loads=NA,
                           env.in.name="sample_consume5_input.csv",envfilename="EnvInputSA.csv",
                           #fofem.env.in.name="FofemEnvExampleIn.csv",
                           fofem.filename="FOFEM_FlamingSAInput1.csv",newwdC="consume5/apps-consume/",
                           oldwdC="../../",newwdF="fofem",oldwdF="../")
{
  base.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt.num]# identify the base fb associated with this evt group
  base.fb<-unique(base.fb[!is.na(base.fb)])[1] # placeholder, take the first in the vector
  
  sobolC.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.sobol,nreps=nreps,
                                          fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                          base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                          mod="C",phase=phase)
  print("after generate consume loads")
  sobolC.results<-call.emissions.mods(infilename=infilename,mod="C",fuel.loads=sobolC.fuel.loads,
                                      env.in.name=env.in.name,envfilename=envfilename,
                                      fofem.filename=fofem.filename,newwd=newwdC,oldwd=oldwdC)
  print("after Consume results")
  prccC.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.prcc,nreps=nreps,
                                         fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                         base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                         mod="C",phase=phase)
  print("after prcc mod call")
  prccC.results<-call.emissions.mods(infilename=infilename,mod="C",fuel.loads=prccC.fuel.loads,
                                     env.in.name=env.in.name,envfilename=envfilename,
                                     fofem.filename=fofem.filename,newwd=newwdC,oldwd=oldwdC)
  
  print("after prcc consume calc" )
    sobolF.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.sobol,nreps=nreps,
                                          fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                          base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                          mod="F",phase=phase)
  print("after generate fofem loads")
  sobolF.results<-call.emissions.mods(infilename=infilename,mod="F",fuel.loads=sobolF.fuel.loads,
                                      env.in.name=env.in.name,envfilename=envfilename,
                                      fofem.filename=fofem.filename,newwd=newwdF,oldwd=oldwdF)
  print("after fofem sobol results")
  prccF.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.prcc,nreps=nreps,
                                         fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                         base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                         mod="F",phase=phase)
  print("after prcc fofem call")
  prccF.results<-call.emissions.mods(infilename=infilename,mod="F",fuel.loads=prccF.fuel.loads,
                                     env.in.name=env.in.name,envfilename=envfilename,
                                     fofem.filename=fofem.filename,newwd=newwdF,oldwd=oldwdF)
  
  print("after prcc fofem results")
  
  
  return(list(sobolCResults=sobolC.results,prccCResults=prccC.results,
              sobolFResults=sobolF.results,prccFResults=prccF.results,
              sobolCfuelLoads=sobolC.fuel.loads,prccCfuelLoads=prccC.fuel.loads,
              sobolFfuelLoads=sobolF.fuel.loads,prccFfuelLoads=prccF.fuel.loads))
}


graphResult_Wrapper.fn<-function(analysisType, modelListType,modelResponse.vars,mats.sobol.obj = NULL, sobolResults = NULL, #def null
                                 corr.samp.vals.sobol=NULL, n.var=6,sobol.obj=NULL,x.lab="",y.lab="",y.lim=c(-0.15,1),
                                 corr.samp.vals.prcc = NULL ,prccCResults = NULL,rank=TRUE,n.boot=1000, prcc.obj = NULL, evt.vals = NULL) #main text 
{ #begin function

  if(analysisType == "sobol") { #Sobol Block
    modelListType<-list()
        for(k in 1:length(modelResponse.vars))
        { 
          modelListType[[k]]<-tell(mats.sobol.obj,sobolResults[,modelResponse.vars[k]])
          rownames(modelListType[[k]]$S)<-names( corr.samp.vals.sobol)
          my.plot.sobol(n.var = n.var,sobol.obj = modelListType[[k]],
                        #main.txt=paste("EVT:",evt.vals,x.lab,y.lab,modelResponse.vars[k])) #pass EVT...etc....
          main.txt=paste("EVT:",evt.vals,x.lab,modelResponse.vars[k])) #pass EVT...etc....
        }
        
        return(modelListType=modelListType)
  } #End Sobol Block
  
  ######################## PRCC #####################################################  
  if(analysisType == "PRCC") { #PRCC Block
      modelListType<-list()
        for(k in 1:length(modelResponse.vars))
        { 
          modelListType[[k]]<-pcc(corr.samp.vals.prcc,
                                  y=prccCResults[,modelResponse.vars[k]],
                                  rank=TRUE,nboot=n.boot)
          my.plot.prcc(n.var=n.var,prcc.obj = modelListType[[k]],
                       main.txt=paste("EVT:",evt.vals,x.lab,modelResponse.vars[k]))
          #main.txt=paste("EVT:",evt.vals,x.lab,y.lab,modelResponse.vars[k]))
        }
        return(modelListType=modelListType)
      } #End PRCC BLock

  } #End funtion
