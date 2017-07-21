##########
# conducting SA using Consume integrated in an R Script
# Assumes Consume is in the current directory
# and assumes correct input environmental and loadings files have been generated

system.call<-paste("python consume_batch.py natural",envoutfilename,  "-f", outfilename)
# calls python to run consume_batch.py, then indicate natural (v. activity fuels), 
# then the patch and filename to the environmental input file, then
# the -f flag, and the path and filename to the loadings input file
# all filepaths assume start at working directory
system(system.call) # tells R to execture this system call in the working directory
# writes teh results file to the current directory, ALWAYS called consume_results.csv
# next step move the file to a results directory with a unique name
# The /Y flag suppresses prompt to confirm overwrite of existing file.
# be careful! 
resultsfilename<-"ConsumeResultsEVT660Try1.csv"
move.call<-paste("move /Y consume_results.csv SAResults\\",resultsfilename,sep="")
system(move.call) # need to debug. system call didn't work, but cutting and pasting to dos prompt did

results.sa<-read.csv(paste("SAResults/",resultsfilename,sep=""))
# next step is to match loading category being modified to change in desired output
# for OAT SA. This requires matching the loadings and results files, 
# which should match row to row

