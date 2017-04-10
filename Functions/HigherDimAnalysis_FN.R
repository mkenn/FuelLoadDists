# Higher Dimensional Analysis
# 
#

############################################### Higher Dimensional Analysis #####################################################
# array creation
hda.coo<-array(0,dim=c(ncol(AllLoads)-2,ncol(AllLoads)-2,ncol(AllLoads)-2))

# identify where co-occurrence>min.co
# minimum co-occurance
min.co<-10
# would creating a vector so that it wouldnt be done individually
cur.evt<-c(615,624,666,1005)

################ cooccurrence loop (looking at 3 loading types)
for(l in 1:length(cur.evt))
{
  for(i in 3:(ncol(AllLoads)-2))
  {
    for(j in (i+1):(ncol(AllLoads)-1))
    {
      for(k in (j+1):ncol(AllLoads))
      {
        tmp.loads<-AllLoads[AllLoads$LFEVTGroupCd_FINAL==cur.evt[l],]
        tmp.load1<-tmp.loads[!is.na(tmp.loads[,i]),]
        if(length(tmp.load1)>0)
        {
          tmp.load2<-tmp.load1[!is.na(tmp.load1[,j]),]
          if(length(tmp.load2)>0)
          {
            tmp.load3<-tmp.load2[!is.na(tmp.load2[,k]),]
            if(length(tmp.load3)>0)
            {
              hda.coo[i-2,j-2,k-2]<-length(tmp.load3[,1])
            }
          }
        }
      }
    }
  }
  # identifying where these cooccurances are > min
  cor.id<-which(hda.coo>min.co,arr.ind=TRUE)
  
  # write to csv file
  write.csv(cor.id,paste("HighDim_",cur.evt[l],"_Cooccur.csv",sep=""),row.names = FALSE)
}


######################### correlation
# variance-covariance matrix as a list
varco.list<-list()

# conditional statement for variance covariance matrix
# this needs to be in 3 for loops so that it looks at each spot within each tier
for(i in 1:ncol(hda.coo))
{
  for(j in 1:ncol(hda.coo))
  {
    for(k in 1:ncol(hda.coo))
    {
      if(hda.coo[i,j,k]>min.co)
      {
        #varco.list[i,j,k]<-var()
      }
    }
  }
}