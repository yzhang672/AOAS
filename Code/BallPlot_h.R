
source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load("IssuestrResults.RData")
load("allpostfans.RData")
ncluster = 4

a = c(1,7,8); BDF = NULL;
for(i in 1:length(a))
{
  j = a[i]
  print(IssuestrResults[[j]][[1]])
  kms_cluster = IssuestrResults[[j]][[3]][[2]]
  kmr_cluster = IssuestrResults[[j]][[3]][[3]]
  B = createB_general_nosort(allpostfans, kms_cluster, cc_sort)
  Bdf = as.data.frame(as.matrix(B))
  BDF = rbind(BDF,Bdf)
}

colnames(BDF) = cn_sort

for(i in 1:length(a))
{
  j = a[i]
  print(IssuestrResults[[j]][[1]])
  kms_cluster = IssuestrResults[[j]][[3]][[2]]
  kmr_cluster = IssuestrResults[[j]][[3]][[3]]
  B = createB_postcand(kmr_cluster, cc_sort, ncluster)
  Ds = diag(1/summary(as.factor(kmr_cluster)))
  Dr = diag(1/summary(as.factor(cc_sort)))
  Bn = Ds %*% B %*% Dr
  Bn = log(1000*Bn+1)
  rownames(Bn) = rownames(B); colnames(Bn) = colnames(B)
  Bdf = as.data.frame(Bn)
  BDF = rbind(BDF,Bdf)
}

ai = c(0,10,Inf)
nele = nrow(BDF)*ncol(BDF)

nscale = 1000; logTran = TRUE; sqrtTran = FALSE; 
type = "Citizen"
type = "Post"
ballGgPlot(BDF, ncluster, nscale, logTran, sqrtTran,type)

