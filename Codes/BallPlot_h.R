
library(ggplot2)

source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load("IssuestrResults.RData")
load("allpostfans.RData")
load('cc_sort.RData')


ncluster = 4

RotatePost = RotateCitizen = list()
RotatePost[[1]] = 1:4
RotatePost[[2]] = c(2,3,4,1)
RotatePost[[3]] = c(3,4,1,2)
RotateCitizen[[1]] = c(3,4,1,2)
RotateCitizen[[2]] = c(3,4,1,2)
RotateCitizen[[3]] = c(3,4,2,1)

a = c(1,7,8); BDF = NULL;
for(i in 1:length(a))
{
  j = a[i]
  print(IssuestrResults[[j]][[1]])
  kms_cluster = IssuestrResults[[j]][[3]][[2]]
  kmr_cluster = IssuestrResults[[j]][[3]][[3]]
  B = createB_general_nosort(allpostfans, kms_cluster, cc_sort)
  Brt = B
  Brt = B[RotateCitizen[[i]],]; rownames(Brt) = rownames(B);
  Bdf = as.data.frame(as.matrix(Brt))
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
  Bn = Bn[RotatePost[[i]], ]
  rownames(Bn) = rownames(B); colnames(Bn) = colnames(B)
  Bdf = as.data.frame(Bn)
  BDF = rbind(BDF,Bdf)
}

ai = c(0,10,Inf)
nele = nrow(BDF)*ncol(BDF)

nscale = 1; logTran = TRUE; sqrtTran = FALSE; eachvalue = NULL; Label = TRUE
type = "Citizen"
type = "Post"
p <- ballGgPlot(BDF, ncluster, nscale, logTran, sqrtTran,type, eachvalue, Label) + 
  theme(plot.title = element_text(hjust = 0.5))
p


type = "Each"; Label = TRUE
for(i in 1:6)
{
  eachvalue = i
  p <- ballGgPlot(BDF, ncluster, nscale, logTran, sqrtTran,type, eachvalue, Label) + 
    theme(plot.title = element_text(hjust = 0.5))
  print(p)
}








