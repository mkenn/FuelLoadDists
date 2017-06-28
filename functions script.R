#####Functions#####
#install fitdistrplus into console 

library(fitdistrplus)

#use loadingsByEVTGroup_20170328.csv data for data.file

data.file <- read.csv(choose.files())
#####
#EVT tally function 
#The function Will determine the number of observations per EVT
#Results will help determine which evts are used for following:
#pair correlations, higher dimensional analysis and potential distribution fitting.
#The function creates tally of all observations per evt, and graphs the tally
#min.tally is the minimum # of obs and will help determine how many evts are used
#max.tally is the maximum # of obs
#both the minimum and maximum were arbitrarily selected

EVT.tally.fn(data.file, evt.col = "LFEVTGroupCd_FINAL", write.file = TRUE)
#####
#Loading Tally Function 
#Loading Tally Function
#The function Will determine the number of observations per loading type, ignoring the evts
#this will help determine:
#holes in dataset, loadings to be used in pair correlation, sensitivity analysis and distribution fitting

loading.tally.fn(data.file, start.col = 3, write.file = TRUE)
#####
#Sensitivity Analysis Function 
#completing the sensitivity analysis of the data for specific evts for each loading type
#this analysis will help determine the sensitivity of the data to change
#EVT 624 is used as example 
#the lower and upper quantiles of 10% and 90% were arbitrarily selected
#the min of 30 was arbitrarily selected
#SAMO = sensitivity analysis of model output

samo.fn(data.file, evts = 624, start.col = 3)
#####
#Distribution Fitting Function
#This function will determine the loglikelihood of the distribution for each fuel loading type
#The function looks at four distributions: normal, lognormal, gamma, weibull
#The results are then entered into a dataframe that can compare likelihoods
#The minimum plot was arbitrarily selected
#EVT 624 is shown as example

dist.fit.fn(data.file, evts = 624, start.col = 3, write.file = TRUE)
#####
#Distribution Fitting Graphing Function
#This function focuses on graphing the data for each loading type at specific evts
#This function will automatically plot the histogram, boxplot, qqnorm and boxplot where values>0 into a pdf
#The distributions being examined are normal, lognormal, gamma, weibull
#The minimum plot value was arbitrarily chosen
#EVT 624 chosen 

dist.fit.graph.fn(data.file, evts = 624, start.col = 3)

#####
#Distribution Fitting Rank Function
#This function ranks the likelihood of the distribution fitting function
#The function reads the results of the Dist_Fitting_FN likelihoods.
#The function then ranks the LL and returns which distribution fit best.
#The file.inputname must be known in order to read the distribution fitting summary
#EVT 624 chosen
distfit.rank.fn(evts = 624, file.inputname = "DistFitSummaryEVT")
