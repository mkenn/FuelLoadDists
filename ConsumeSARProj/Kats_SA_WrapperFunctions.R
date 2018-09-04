##############
# wrapper function to conduct sensitivity analysis
# for consume and/or fofem
###############
makeCorrelatedSampleMat_Wrapper.fn<-function(data.file,fuel.ids,complete.case=TRUE,min.co.occur=30,
                                             evts,EVTCol,start.col,n.samp=1000,rankObj,fitObj,
                                             upper.quantile=NA,use.corr=TRUE,nboot=1000,cur.evt.id)
{
  set.seed(NULL)
  sens.mats.list<-corr.sa.fn(data.file,fuel.ids=fuel.ids,complete.case=complete.case,min.co.occur=min.co.occur,
                             evts=evts,EVTCol=EVTCol,start.col=start.col,n.samp=n.samp,
                             rankObj=rankObj,fitObj=fitObj,upper.quantile=upper.quantile,
                             use.corr=use.corr)
  if(!is.na(sens.mats.list[[1]][[cur.evt.id]])[1])
  {
    cur.sobol<-sobolEff(X1=sens.mats.list[[1]][[cur.evt.id]][1:(n.samp/2),],
                        X2=sens.mats.list[[1]][[cur.evt.id]][(n.samp/2+1):n.samp,],nboot = nboot)
    
    corr.samp.vals<-data.frame(cur.sobol$X)
    names(corr.samp.vals)<-names(sens.mats.list[[1]][[cur.evt.id]])
    
    uncor.cur.sobol<-sobolEff(X1=sens.mats.list[[2]][[cur.evt.id]][1:(n.samp/2),],
                              X2=sens.mats.list[[2]][[cur.evt.id]][(n.samp/2+1):n.samp,],nboot = nboot)
    
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
                           fofem.filename="FOFEM_FlamingSAInput1.csv",newwdC="consume5/apps-consume/",
                           oldwdC="../../",newwdF="fofem",oldwdF="../")
{
  base.fb<-evtFB.map$FCCSID[evtFB.map$EVT_GP==cur.evt.num]# identify the base fb associated with this evt group
  base.fb<-unique(base.fb[!is.na(base.fb)])[1]
  
  sobolC.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.sobol,nreps=nreps,
                                          fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                          base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                          mod="C",phase=phase)
  sobolC.results<-call.emissions.mods(infilename=infilename,mod="C",fuel.loads=sobolC.fuel.loads,
                                      env.in.name=env.in.name,envfilename=envfilename,
                                      fofem.filename=fofem.filename,newwd=newwdC,oldwd=oldwdC)
  prccC.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.prcc,nreps=nreps,
                                         fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                         base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                         mod="C",phase=phase)
  prccC.results<-call.emissions.mods(infilename=infilename,mod="C",fuel.loads=prccC.fuel.loads,
                                     env.in.name=env.in.name,envfilename=envfilename,
                                     fofem.filename=fofem.filename,newwd=newwdC,oldwd=oldwdC)
  
  sobolF.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.sobol,nreps=nreps,
                                          fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                          base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                          mod="F",phase=phase)
  sobolF.results<-call.emissions.mods(infilename=infilename,mod="F",fuel.loads=sobolF.fuel.loads,
                                      env.in.name=env.in.name,envfilename=envfilename,
                                      fofem.filename=fofem.filename,newwd=newwdF,oldwd=oldwdF)
  prccF.fuel.loads<-GenerateFuelInput.fn(corr.samp.vals=corr.samp.vals.prcc,nreps=nreps,
                                         fbLoadNames.df=fbLoadNames.df,all.fbs=all.fbs,
                                         base.fb=base.fb,base.fofem=base.fofem,change.units=change.units,
                                         mod="F",phase=phase)
  prccF.results<-call.emissions.mods(infilename=infilename,mod="F",fuel.loads=prccF.fuel.loads,
                                     env.in.name=env.in.name,envfilename=envfilename,
                                     fofem.filename=fofem.filename,newwd=newwdF,oldwd=oldwdF)
  
  
  
  return(list(sobolCResults=sobolC.results,prccCResults=prccC.results,
              sobolFResults=sobolF.results,prccFResults=prccF.results,
              sobolCfuelLoads=sobolC.fuel.loads,prccCfuelLoads=prccC.fuel.loads,
              sobolFfuelLoads=sobolF.fuel.loads,prccFfuelLoads=prccF.fuel.loads))
}

graphConsumeResult_Wrapper.fn<-function(type, fuels.mat, response.vars, sobolResults, corr.samp.vals.sobol, n.var=6, sobol.obj, x.lab="", y.lab="",y.lim=c(-0.15,1),
                                 main.txt, corr.samp.vals.prcc, prccCResults, prcc.obj) 
{
  switch(type, sobol = 1, PRCC = 2)
  if(type == 1) { ##meaning sobol 
    sens.consume.list<-list()
    for(k in 1:length(response.vars))
    { # here we standardize outputs to the z-scoore
      # sens.list[[k]]<-tell(sobolF,
      #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
      sens.consume.list[[k]]<-tell(fuels.mat, sobolResults)
      # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
      rownames(sens.consume.list[[k]]$S)<-names(corr.samp.vals.sobol)
      my.plot.sobol(n.var = 6,sobol.obj = sens.consume.list[[k]],
                    main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","Sensitivity index output:",responseFConsume.vars[k]))
    }
    
    return(sens.consume.list = sens.consume.list)
  }
  
  if(type == 2) { ##meaning PRCC
    consumeF.prcc<-list()
    for(k in 1:length(response.vars))
    { # here we standardize outputs to the z-scoore
      # sens.list[[k]]<-tell(sobolF,
      #                      (results.fofem.sa[,responseF.vars[k]]-mean(results.fofem.sa[,responseF.vars[k]]))/sd(results.fofem.sa[,responseF.vars[k]]))
      consumeF.prcc[[k]]<-pcc(corr.samp.vals.prcc,
                              y=prccCResults,
                              rank=TRUE,nboot=1000)
      # sens.list[[k]]<-tell(sobolF,results.fofem.sa[,responseF.vars[k]])
      my.plot.prcc(n.var=6,prcc.obj = consumeF.prcc[[k]],
                   main.txt=paste("EVT:",evt.vals[cur.evt],"Consume","Partial rank correlation coefficient output:",responseFConsume.vars[k]))
    }
    return(consumeF.prcc = consumeF.prcc)
  }
}

