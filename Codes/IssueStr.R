rm()
set.seed(1)

library(Matrix); library(irlba);

source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load("allpostconwords_binary.RData")
load("allfanwords_binary.RData")
load('allpostfans.RData')

ncandidate = 8; nscale = 1000; len <- 500;

GraphLap = laplacian(allpostfans,"both",regular=TRUE)
Xs = scale(allfanwords_binary,"both")
Ys = scale(allpostconwords_binary,"both")

XLYs = t(Xs) %*% (GraphLap %*% Ys)
MeanXLYs = sum(GraphLap@x)*as.matrix(colMeans(Xs))%*%t(as.matrix(colMeans(Ys)))
XLY = XLYs-MeanXLYs

thresh = 0.00115
summary(XLY@x)
quantile(XLY@x, prob = 0.99)

XLY_thresh = threshold(XLY,thresh)

#aprox = 0.99
#SubgauConst = sqrt(quantile(Xs@x, probs = aprox)*quantile(Ys@x, probs = aprox))
#dc = min(rowSums(allpostfans)) + mean(rowSums(allpostfans)); 
#dp = min(colSums(allpostfans)) + mean(colSums(allpostfans))
#gamma2 = sqrt(quantile(colSums(Xs^2), prob = aprox) * quantile(colSums(Ys^2), prob = aprox))
#alpha = 1/2021
#max(SubgauConst^2*max(sqrt(sum(GraphLap^2)*log(1/alpha)), 
#                  irlba(GraphLap,nu = 1,nv = 1)$d[1]*log(1/alpha)),
#    gamma2/sqrt(dc*dp)*sqrt(log(1/alpha)))

#consts = c(0,0.5,1,1.5,2,5,10, 285.4246, 1e6)
consts = c(0,0.5,1,1.5,2,5,10, 1e6)

TextOnly = c(rep(0,length(consts)-1),1)

consts;TextOnly

IssuestrResults = list()

for (consti in 1:length(consts))
{
  const = consts[consti]
  if(TextOnly[consti]==1){
    XWY = Xs%*%((XLY_thresh)%*%t(Ys))-Xs%*%(MeanXLYs%*%t(Ys))
    svdAC <- irlba(XWY, nu=10, nv=10, 
                   matmul = matmulACinf(XWY,Xs,Ys,XLY_thresh, 
                                        MeanXLYs, meanXLYs = TRUE))
  }else{
    svdAC <- irlba(GraphLap, nu=10, nv=10, 
                   matmul = matmulAC(GraphLap,const,Xs,Ys,XLY_thresh, 
                                     MeanXLYs, meanXLYs = TRUE))
  }
  
  svdACvalue <- svdAC$d
  #plot(svdAC$d[-1])
  
  ncluster <- 4
  v = svdAC$v[,1:ncluster]
  v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))
  gc()
  u = svdAC$u[,1:ncluster]
  u = t(apply(u,1,function(x) {
    if(sum(x^2)>0) {return(x/sqrt(sum(x^2)))}
    else{return(x)}
  }))
  
  niteration <- 1000;
  km_v = kmeans(v, centers = ncluster, nstart = niteration)
  km_u = kmeans(u,centers = ncluster, nstart = niteration)
  Bresult = createB_general(allpostfans, km_u$cluster, km_v$cluster, ncluster,sorttype="ClusterSize" )
  PostScore = rep(0,nrow(allpostfans)); FanScore = rep(0,ncol(allpostfans))
  npart = 4; 
  for (i in 1: npart)
  {
    fi = which(km_u$cluster==i)
    pi = which(km_v$cluster==i)
    FanScore[fi] =  u[fi,]%*%km_u$centers[i,]
    PostScore[pi] = v[pi,]%*%km_v$centers[i,]
  }
  
  kmcluster = Bresult[[3]]; 
  postword_signif = word_signif(allpostconwords_binary,kmcluster,poisson=TRUE,ncluster)
  posclus_sigword=list(); 
  postsigword <- data.frame(matrix(vector(), len, ncluster,
                                   dimnames=list(paste("",1:len), paste("Cluster",1:ncluster))),
                            stringsAsFactors=FALSE)
  for (i in 1:ncluster)
  {
    clusi_sig = postword_signif[i,]
    wordsig = sort(clusi_sig[which(clusi_sig>1)],decreasing = TRUE)
    posclus_sigword[[i]] = wordsig
    postwords = names(wordsig)
    postsigword[,i] <- postwords[1:min(len,length(postwords))]
  }
  
  kmscluster = Bresult[[2]]; 
  fanword_signif = word_signif(allfanwords_binary,kmscluster,poisson=TRUE,ncluster)
  fanclus_sigword=list(); 
  fansigword <- data.frame(matrix(vector(), len, ncluster,
                                  dimnames=list(paste("",1:len), paste("Cluster",1:ncluster))),
                           stringsAsFactors=FALSE)
  for (i in 1:ncluster)
  {
    clusi_sig = fanword_signif[i,]
    wordsig = sort(clusi_sig[which(clusi_sig>1)],decreasing = TRUE)
    fanwords = names(wordsig)
    fansigword[,i] <- fanwords[1:min(len,length(fanwords))]
  }
  
  IssuestrResults[[consti]] = list(c(const,TextOnly[consti]),svdACvalue,Bresult,PostScore,FanScore,postsigword,fansigword)
  print(const)
}
#save(IssuestrResults,file = "IssuestrResults.RData")

constscale = IssuestrResults[[1]][[2]][2]/IssuestrResults[[8]][[2]][2]

realconsts = consts/constscale
realconsts[7];

Case1svd = IssuestrResults[[1]][[2]]
Case2svd = IssuestrResults[[8]][[2]]

# Next time, think about first make the 1st singular values of them to be close to 1.


