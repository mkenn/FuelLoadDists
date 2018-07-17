#########
# function to estimate correlation matrix
# on subset of fuel types (id'd as smoldering or flaming)
# this assumes a complete-case analysis for now,
# meaning we only evaluate rows for which all fuel types of
# interest are present. Alternatively, we can try some form
# of imputation on the missing data (not implemented here).
##########

corr.sa.fn<-function(data.file,fuel.ids,complete.case=TRUE,min.co.occur=10,
                     evts,EVTCol,start.col,n.samp=1000,rankObj,fitObj,
                     upper.quantile=NA,use.corr=TRUE)
{
  sens.sort.list<-list()
  sens.unCorr.list<-list()
  corr.mats.list<-list()
  R.star.list<-list()
  for(cur.evt in 1:length(evts))
  {
    
    tmp.df<-data.file[data.file[,EVTCol]==evt.vals[cur.evt],]
    tmp.loads.df<-tmp.df[,start.col:ncol(tmp.df)]
    # might need an imputation package to assess the missingness 
    # in the subset of data
    tmp2.loads.df<-tmp.loads.df[,fuel.ids]
    cur.loads<-tmp2.loads.df[complete.cases(tmp2.loads.df),]
    if(nrow(cur.loads)>min.co.occur)#&use.corr)
    {
      cur.corr<-cor(cur.loads,method="spearman")
#      try.na<-which(is.na(cur.corr[1,])) need to better id the missing correlations
# any column with length(fuel.ids)-1 NA's is tagged as 0 variance
      count.na<-rep(0,length(fuel.ids)) # we'll fill these in with any columns that need to be flagged
      try.na<-NA
      no.na<-NA
      na.id<-1
      no.na.id<-1
      for(m in 1:length(fuel.ids))
      {
        cur.len<-length(cur.corr[is.na(cur.corr[,m]),1])
        if(cur.len==length(fuel.ids)-1)
        {
          count.na[m]<-1
          try.na[na.id]<-m
          na.id<-na.id+1
        }
        else
        {
          no.na[no.na.id]<-m
          no.na.id<-no.na.id+1
        }
      }
      if(!is.na(try.na[1]))
      {
        cur.corr<-cur.corr[,-try.na]
        cur.corr<-cur.corr[-try.na,]
        new.fuel.ids<-fuel.ids[-try.na]
      }
      else
        new.fuel.ids<-fuel.ids
    
      corr.mats.list[[cur.evt]]<-cur.corr
      # now a function to return the R matrix
      R.star<-corr2sort.matrix.fn(cur.corr,n.samp,var.red=TRUE)
      R.star.list[[cur.evt]]<-R.star
      # and a function to sample the fuel types of interest
      # uses the hurdle fits to draw random values
    
      # have current loads now be based on the full data set, for
      # those cases where the continuous distribution was not fit
      tmp3.loads.df<-tmp.loads.df[,new.fuel.ids]
      sens.mat<-sampleFuelsMatrix.fn(curRank=rankObj[[cur.evt]][new.fuel.ids,],
                                     curFit=fitObj$HurdleFit[[cur.evt]][new.fuel.ids,],
                                     new.fuel.ids=new.fuel.ids,n.samp,cur.loads=tmp3.loads.df,
                                     upper.quantile=upper.quantile)
      sens.mat<-data.frame(sens.mat)
      names(sens.mat)<-names(tmp2.loads.df)
      sens.unCorr.list[[cur.evt]]<-sens.mat # to keep the uncorrelated sampled values
      # if(corr.vals)
      # {
      # apply the matrix to enforce the correlation structure
        sens.sort<-sortSensMat.fn(sens.mat,R.star,new.fuel.ids,n.samp)
        if(!is.na(try.na[1])) # then we need to fill in some columns
        {
          final.sens.sort<-matrix(NA,ncol=length(fuel.ids),nrow=n.samp)
          final.sens.sort[,no.na]<-sens.sort
          for(f in 1:length(try.na))
          {
            final.sens.sort[,try.na[f]]<-rep(cur.loads[1,try.na[f]],n.samp)
          }
        }
        else
          final.sens.sort<-sens.sort
        final.sens.sort<-data.frame(final.sens.sort)
        names(final.sens.sort)<-names(tmp2.loads.df)
        # now, insert columns in the appropriate place for those variables with no correlation
        sens.sort.list[[cur.evt]]<-final.sens.sort
      }
      else
      {
        # if(use.corr)
        # {
          sens.sort.list[[cur.evt]]<-NA
          corr.mats.list[[cur.evt]]<-NA
          R.star.list[[cur.evt]]<-NA
        # }
        # else
        # {
        #   tmp.df<-data.file[data.file[,EVTCol]==evt.vals[cur.evt],]
        #   tmp.loads.df<-tmp.df[,start.col:ncol(tmp.df)]
        #   # might need an imputation package to assess the missingness 
        #   # in the subset of data
        #   tmp2.loads.df<-tmp.loads.df[,fuel.ids]
        #   
        #   sens.sort.list[[cur.evt]]<-sampleFuelsMatrix.fn(curRank=rankObj[[cur.evt]][fuel.ids,],
        #                                  curFit=fitObj$HurdleFit[[cur.evt]][fuel.ids,],
        #                                  new.fuel.ids=fuel.ids,n.samp,cur.loads=tmp2.loads.df,
        #                                  upper.quantile=upper.quantile)
        #   
        # }
         
      }
    #}
  }
  return(list(sens.sort=sens.sort.list,sensunCorr.mat=sens.unCorr.list,corr.mats=corr.mats.list,R.star=R.star.list))
}

corr2sort.matrix.fn<-function(cur.corr,n.samp,var.red=TRUE)
{
  P.trans<-chol(cur.corr) # this returns the cholesky factorization, P' in the paper # getting not positive definite error, seems that we still need a smaller matrix; exclude those variables with ALL zeroes
# So, this isn't working because we are handling the NA's pairwise, removing them 
# for each pair. This means that the resulting correlation matrix is NOT
# positive semi-definite, which means that the factorization can't work.
  P.mat<-t(P.trans) # The transpose of the above matrix is the P matrix in the paper 
  N<-n.samp # number of samples
  a.vals<-qnorm((1:N)/(N+1))
  K<-ncol(cur.corr) # the number of variables
  R.mat<-matrix(NA,ncol=K,nrow=N)
  for(j in 1:K)
  {
    R.mat[,j]<-sample(a.vals)
  }
  
  if(var.red)
  {
    T.mat<-cor(R.mat,method="spearman")
    Q.trans<-chol(T.mat)
    Q.mat<-t(T.mat)
    S.mat<-P.mat%*%solve(T.mat)
    R.star<-R.mat%*%t(S.mat)
  }
  else
    R.star<-R.mat%*%P.trans 
  return(R.star) # this matrix helps to sort the sample matrix for SA
}

sampleFuelsMatrix.fn<-function(curRank,curFit,new.fuel.ids,n.samp,cur.loads,upper.quantile)
{
  N=n.samp
  sens.mat<-matrix(NA,ncol=length(new.fuel.ids),nrow=N)
  for(k in 1:length(new.fuel.ids))
  {
    if(!is.na(curRank$dist1.fit[k]))
    {
      tmp.distr<-switch(curRank$dist1.fit[k],
                        lnormLL="lnorm",
                        gammaLL="gamma")
      tmp.samp<-simHurdle.fn(distr=tmp.distr,prop0=curFit$prop0[k],nsamp=N,nrep=1,
                             param1=curFit[k,paste(tmp.distr,".p1",sep="")],
                             param2=curFit[k,paste(tmp.distr,".p2",sep="")],
                             upper.quantile=upper.quantile)
      sens.mat[,k]<-tmp.samp[,1]
    }
    else
    {
      tmp.loads<-cur.loads[,k]
      tmp.loads<-tmp.loads[!is.na(tmp.loads)]
      if(length(tmp.loads)>0)
      {
        cur.gt0<-tmp.loads[tmp.loads>0]
        n.gt0<-length(cur.gt0)
        n.0<-length(tmp.loads[tmp.loads==0])
        cur.prop0<-n.0/(n.gt0+n.0)
        if(cur.prop0<1)
        {
          tmp.mu<-mean(log(cur.gt0))
          tmp.sd<-sd(log(cur.gt0))
          tmp.samp<-simHurdle.fn(distr="lnorm",prop0=cur.prop0,nsamp=N,nrep=1,
                                 param1=tmp.mu,
                                 param2=tmp.sd,cur.gt0)
          sens.mat[,k]<-tmp.samp[,1]
        }
        else
        {
          sens.mat[,k]<-0
        }
      }
      else
      {
        sens.mat[,k]<-NA
      }
    }
  }
  return(sens.mat)
}

sortSensMat.fn<-function(sens.mat,R.star,fuel.ids,n.samp)
{
  N<-n.samp
  K<-ncol(sens.mat)
  X.sort<-matrix(NA,ncol=K,nrow=N)
  for(j in 1:length(fuel.ids))
  {
    tmp.sort<-sort(R.star[,j],index.return=TRUE) 
    # so tmp.sort$ix gives the row number for each rank, 
    # i.e., tmp.sort$ix[i] gives the row number of the ith ordered statistic.
    # inversely, which(tmp.sort$ix==i) gives the ordered statistic of the ith row
    tmp2.sort<-sort(sens.mat[,j],index.return=TRUE) 
    # so now we replace the ith row in X with the value of the same rank as 
    # the ith row in R.star
    for(i in 1:N)
      X.sort[i,j]<-sens.mat[tmp2.sort$ix[which(tmp.sort$ix==i)],j]
  }
  return(X.sort)
}
