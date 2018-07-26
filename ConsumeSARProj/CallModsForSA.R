########
# function to call consume or fofem 
# with a set of sample fuels
#########

call.emissions.mods<-function(infilename="FuelLoadInputSA.csv",mod="C",fuel.loads=NA,
                             env.in.name="sample_consume5_input.csv",envfilename="EnvInputSA.csv",
                             fofem.filename="FOFEM_FlamingSAInput1.csv",newwd="consume5/apps-consume/",oldwd="../../")#,outfilename="output_summary.csv")
{
  if(mod=="C")
  {
    setwd(newwd)
    loadInFile.head<-matrix(c("GeneratorName=FCCS   3.0","GeneratorVersion=3.0.0","DateCreated=07/18/2016"),
                            ncol=3)
    # didn't like today's date
    # write the file with the proper header information
#    infilename<-"FuelLoadInputSA.csv"
    write.table(loadInFile.head,file=infilename,row.names=FALSE,col.names=FALSE,sep=",",quote = FALSE)

        # Now append that file with the generated loadings
    write.table(fuel.loads,file=infilename,row.names=FALSE,append=TRUE,sep=",",quote=FALSE)
    
    # and create an environmental input file that is just repeated for all rows of the fuels file
    # This is baseline environmental conditions. Consider including in SA
    env.in<-read.csv(env.in.name) ###Note, if this file changes so should the FOFEM input file moistures
    new.env.in<-env.in[1,]
    for(m in 2:nrow(fuel.loads))
    {
      new.env.in<-rbind(new.env.in,env.in[1,])
    }
    new.env.in$fuelbeds<-fuel.loads$fuelbed_number
#    envfilename<-"EnvInputSA.csv"
    write.table(new.env.in,file=envfilename,row.names=FALSE,sep=",")
    
    # and now we call consume
    # first format the system call
    system.call<-paste("python consume_batch.py natural",envfilename,  "-f", infilename)#, "-x",outfilename)
    try1<-try(system(system.call)) # tells R to execute this system call in the working directory
    # Then we readin the results, and calculate the Sobol sensitivity indices
    
    if(try1==1)
    {
      print("Failed consume call")
      return.sa=NA
    }
    if(try1==0) # then consume was successful, and we can readin the output file
    {
      results.sa<-read.csv("consume_results.csv") # writes to this file every time, replacing previous    
    }
#    setwd("../../")
    setwd(oldwd)
  }
  if(mod=="F")
  {
    
    setwd(newwd)
#    setwd("fofem")
    write("#1k-SizeClass",file=fofem.filename) # switch header to indicate 
    write.table(fuel.loads,file=fofem.filename,append = TRUE,sep=",",row.names = FALSE,col.names = FALSE)
    system.call<-paste("FOF_GUI C",fofem.filename,"ConE-Out.txt ConE-run.txt ConE-Err.txt H", sep=" ")
    system(system.call) # tells R to execute this system call in the working directory
    
    check.fofem<-scan("ConE-Err.txt",what = character())# if there is no error recorded, then this file will be empty
    if(length(check.fofem)==0) # if there is no error recorded, then this file will be empty
      results.sa<-read.csv("ConE-Out.txt") # writes to this file every time, replacing previous results
    else
      results.sa<-NA
#    setwd("../")
    setwd(oldwd)
      
  }
  return(results.sa)
}