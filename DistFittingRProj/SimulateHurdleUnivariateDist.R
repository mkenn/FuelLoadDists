#####
# Assess hurdle distribution by first
# simulating from best fit distribution
#####

test.sim<-simHurdle.fn(distr="gamma",prop0=0.25,nsamp=100,nrep=10,param1=0.6070180,param2=4.5872638)

hist(test.sim[test.sim[,1]>0,1])


