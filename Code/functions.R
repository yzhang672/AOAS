library(Matrix);library(dplyr);library(RSQLite);library(SnowballC)

mycolor = c("red","blue","purple","magenta",
            "deepskyblue","orange","green", "cyan",
            "brown","yellow","skyblue")

## match candidates with their ranks in the election
cn = c("Lepen", "Joly", "Bayrou", "Hollande", 
       "Melenchon", "Dupont", "Sarkozy", "Poutou")
cn_sort = c("Hollande", "Sarkozy", "Lepen", "Melenchon",
            "Bayrou", "Joly", "Dupont", "Poutou")
cn_match = match(cn,cn_sort)


createA <- function(x,y)
{
  ux = unique(x); uy = unique(y);
  n  = length(x); nx = length(ux); ny = length(uy)
  cx = match(x,ux); cy = match(y,uy);
  A  = spMatrix(nrow = nx , ncol = ny, 
                i = cx , j = cy , x = rep(1,n))
  colnames(A) = uy
  rownames(A) = ux
  return(A)
}

laplacian <- function(AdjMat,type,regular)
{
  rs = rowSums(AdjMat); cs = colSums(AdjMat); 
  taur = 0; tauc = 0
  if(regular==TRUE)
  {taur = mean(rs); tauc = mean(cs)}
  
  if(type=="row")
  {
    GraphLap = Diagonal(length(rs), 1/(rs+taur))%*%AdjMat
  }
  if(type=="column")
  {    
    GraphLap = AdjMat%*%Diagonal(length(cs), 1/(cs+tauc))
  }
  if(type=="both")
  {
    GraphLap = Diagonal(length(rs), 1/sqrt(rs+taur))%*%
      AdjMat%*%Diagonal(length(cs), 1/sqrt(cs+tauc))
  }
  return(GraphLap)
}

scale <- function(A,type)
{
  rs = rowSums(A); cs = colSums(A)
  if (type=="both")
  {As = Diagonal(length(rs), 1/sqrt(rs))%*%
    A%*%Diagonal(length(cs), 1/sqrt(cs))}
  if (type=="column")
  {As = A%*%Diagonal(length(cs), 1/cs)}
  if (type=="row")
  {As = Diagonal(length(rs), 1/rs)%*%A}
  if (type=="none")
  {As=A}
  return(As)
}

createB_candidate <- function(cc,AdjMat,ncluster,niteration)
{
  Y = model.matrix(~as.factor(cc)-1)
  yy = solve(t(Y)%*%Y)
  ap = AdjMat%*%Y%*%yy
  apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))
  
  kmap = kmeans(apn, ncluster, nstart = niteration)
  
  Zhat = sparse.model.matrix(~as.factor(kmap$clust)-1)
  zz = solve(t(Zhat)%*%Zhat)
  B0 = zz%*%t(Zhat)%*%AdjMat%*%Y%*%yy
  
  cn = c("Lepen", "Joly", "Bayrou", "Hollande", 
         "Melenchon", "Dupont", "Sarkozy", "Poutou")
  cn_sort = c("Hollande", "Sarkozy", "Lepen", "Melenchon",
              "Bayrou", "Joly", "Dupont", "Poutou")
  sortPosClus = cn_match
  cn = cn[sortPosClus]
  
  B1=B0[,sortPosClus]; 
  
  sortfan = apply(B1,2,which.is.max)
  dupli = unique(sortfan[duplicated(sortfan)])
  
  if(ncluster<ncandidate)
  {
    sortFanClus = unique(sortfan)
  }
  
  else
  {
    if(length(dupli)==0)
    {
      indicate = 1:ncluster
      sortFanClus = c(sortfan,which(is.na(match(indicate,sortfan))))
    }
    else
    {
      duplicand = which(sortfan==dupli)
      B1dup = B1; B1dup[dupli,duplicand]=0
      sortfan_dc = apply(B1dup[,duplicand],2,which.is.max)
      
      if (ncluster == ncandidate)
      {sortFanClus = sortfan; sortFanClus[duplicand]=sortfan_dc; 
      sortFanClus[duplicand[length(duplicand)]]=dupli}
      else
      {
        sortfanclus = c(sortfan,dupli)
        sortFanClus = sortfanclus; 
        sortFanClus[duplicand]=sortfan_dc
      }
    }
    
  }
  
  B=B1[sortFanClus,];
  
  colnames(B) = cn_sort;
  rownames(B) = c(paste("Fan-group", 1:ncluster, sep=" ")); 
  
  kmClus = kmap$cluster
  for (i in 1:ncluster)
  {
    ci = which(kmap$cluster==sortFanClus[i])
    kmClus[ci]=rep(i,length(ci))
  }
  
  kmPosClus = cc
  for (i in 1:ncandidate)
  {
    ci = which(cc==sortPosClus[i])
    kmPosClus[ci]=rep(i,length(ci))
  }
  
  return(list(B,kmClus,kmPosClus))
  
}

createB_general_nosort <- function(AdjMat, kms_cluster, kmr_cluster)
{
  Zhat = model.matrix(~as.factor(kms_cluster)-1); 
  Yhat = model.matrix(~as.factor(kmr_cluster)-1); 
  zz = solve(t(Zhat)%*%Zhat); yy = solve(t(Yhat)%*%Yhat);
  B = zz%*%t(Zhat)%*%AdjMat%*%Yhat%*%yy
  
  rn = c(paste("Fc", 1:nrow(B), sep=" ")); 
  cn = c(paste("Pc", 1:ncol(B), sep=" "))
  rownames(B) = rn
  colnames(B) = cn
  return(B)
}

createB_general <- function(AdjMat, kms_cluster, kmr_cluster,ncluster,sorttype)
{
  Zhat = model.matrix(~as.factor(kms_cluster)-1); 
  Yhat = model.matrix(~as.factor(kmr_cluster)-1); 
  zz = solve(t(Zhat)%*%Zhat); yy = solve(t(Yhat)%*%Yhat);
  B0 = zz%*%t(Zhat)%*%AdjMat%*%Yhat%*%yy
  
  sortPosClus = 1:ncol(B0); sortFanClus = 1:nrow(B0); B = B0;
  
  if(sorttype == "BSize")
  {csB0 = apply(B0,2,max); csB0sort = sort(csB0,decreasing=TRUE);
  sortPosClus = match(csB0sort, csB0)
  B1=B0[,sortPosClus]; 
  B1=B1*nscale
  
  rsB1 = apply(B1,1,max)
  rsB1sort = sort(rsB1,decreasing=TRUE);
  sortFanClus = match(rsB1sort, rsB1)
  B=B1[sortFanClus,];
  }
  
  kr = length(unique(kmr_cluster)); ks = length(unique(kms_cluster))
  if(sorttype == "ClusterSize")
  {
    clussize = NULL;
    for (i in 1:kr) clussize = c(clussize,length(which(kmr_cluster==i))); 
    clussizesort = sort(clussize,decreasing=FALSE);
    sortPosClus = match(clussizesort, clussize)
    B1=B0[,sortPosClus]; 
    
    clussize = NULL;
    for (i in 1:ks) clussize = c(clussize,length(which(kms_cluster==i))); 
    clussizesort = sort(clussize,decreasing=FALSE);
    sortFanClus = match(clussizesort, clussize)
    B=B1[sortFanClus,];
  }
  
  kmsClus = kms_cluster
  for (i in 1:ks)
  {
    ci = which(kms_cluster==sortFanClus[i])
    kmsClus[ci]=rep(i,length(ci))
  }
  
  kmrClus = kmr_cluster
  for (i in 1:kr)
  {
    ci = which(kmr_cluster==sortPosClus[i])
    kmrClus[ci]=rep(i,length(ci))
  }
  
  rn = c(paste("Fc", 1:nrow(B), sep=" ")); 
  cn = c(paste("Pc", 1:ncol(B), sep=" "))
  rownames(B) = rn
  colnames(B) = cn
  return(list(B,kmsClus,kmrClus))
}


matmulAC <- function(A,const,Xs,Ys,XLY_thresh,MeanXLYs,meanXLYs)
{
  v <- apply(A,2,mean)
  function(A,x,transpose=FALSE)
  {
    if(transpose)
      return( as.matrix(t(crossprod(x,A)) - sum(x) * v))
    
    if(meanXLYs == TRUE)
      as.matrix(A %*% x + 
                  const*Xs %*% (XLY_thresh %*% crossprod(Ys,x))
                # - cbind(rep(crossprod(v,x)[1],nrow(A))) 
                - const*Xs %*% (MeanXLYs %*% crossprod(Ys,x))
      )
    else
      as.matrix(A %*% x + 
                  const*Xs %*% (XLY_thresh %*% crossprod(Ys,x))
                # - cbind(rep(crossprod(v,x)[1],nrow(A)))
      )
    
  }
}

matmulACinf <- function(XWY,Xs,Ys,XLY_thresh,MeanXLYs,meanXLYs)
{
  
  v <- apply(XWY,2,mean)
  function(XWY,x,transpose=FALSE)
  {
    if(transpose)return( as.matrix(t(crossprod(x,XWY)) - sum(x) * v))
    if(meanXLYs == TRUE)
      as.matrix(
        Xs %*% (XLY_thresh %*% crossprod(Ys,x))
        #- cbind(rep(crossprod(v,x)[1],nrow(A))) 
        - Xs %*% (MeanXLYs %*% crossprod(Ys,x))
      )
    else
      as.matrix(
        Xs %*% (XLY_thresh %*% crossprod(Ys,x))
        # - cbind(rep(crossprod(v,x)[1],nrow(A)))
      )
  }
}

expect_count <- function(X)
{
  rs = rowSums(X); cs = colSums(X); ss = sum(X)
  X_expect = rs%*%t(cs)/ss
  rownames(X_expect) = rownames(X)
  colnames(X_expect) = colnames(X)
  return(X_expect)
}

word_signif <- function(X,kmcluster,poisson,ncluster)
{
  nc = ncol(X)
  ncluster = length(unique(kmcluster))
  D = matrix(0, nrow = ncluster, ncol = nc)
  
  if (poisson == TRUE)
  {
    X_e = expect_count(X)
    
    for (k in 1:ncluster)
    {
      ck = which(kmcluster==k)
      if (length(ck)>1)
      {
        X_ck = colSums(X[ck,])
        N_ck = colSums(X_e[ck,])
      }
      else if(length(ck)==1)
      {
        X_ck = X[ck,]
        N_ck = X_e[ck,]
      }
      D[k,] = X_ck/N_ck
    }
  }
  
  else
  {
    X_ckj = matrix(0, nrow=ncluster, ncol = nc)
    for (k in 1:ncluster)
    {
      ck = which(kmcluster==k)
      if (length(ck)>1)
      {
        X_ckj[k,] = colSums(X[ck,])
      }
      else if(length(ck)==1)
      {
        X_ckj[k,] = X[ck,]
      }
    }
    X_ckj_e = expect_count(X_ckj)
    D = X_ckj/X_ckj_e
    
  }
  D = as(D, "sparseMatrix") 
  rownames(D) = as.character(1:ncluster)
  colnames(D) = colnames(X)
  return(D)
  
}


GgPlotClusSizes <- function(DataFrame, Clusters, ncluster, xlab, ylab, main){
  qplot(Clusters, data=DataFrame, geom="bar", fill=Clusters) +
    xlab(xlab) +
    ylab(ylab) +
    theme_bw() +
    ggtitle(main) +
    scale_fill_manual(values = c(mycolor[1:ncluster]),
                      breaks=as.character(1:ncluster),
                      labels=as.character(1:ncluster)) +
    theme(legend.position = "none", 
          axis.text.x = element_text(size=10, face = "bold"),
          axis.text.y = element_text(size=10, face = "bold"),
          axis.title.x = element_text(size=12, face = "bold"),
          axis.title.y = element_text(size=12, face = "bold"),
          title = element_text(size = 14, face = "bold")
    )
}
