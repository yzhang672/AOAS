library(Matrix);library(dplyr);library(RSQLite);library(SnowballC)
source("~/Dropbox/my project/frenchFacebook/code/words_del_sym.R")

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

createB_postcand <- function(kmPosClus,cc,ncluster)
{
  rn = c(paste("Pc",1:ncluster, sep=" "))
  cn = cn_sort
  
  id=1:length(kmPosClus)
  df=data.frame(kmPosClus,cc,id)
  nmatr=matrix(0,ncol=length(cn),nrow=ncluster)
  for (i in 1:ncluster)
  {
    for (j in 1:length(cn))
    {
      nmatr[i,j]=sum(df$kmPosClus==i & df$cc==j)
    }
  }
  
  rownames(nmatr)=rn
  colnames(nmatr)=cn
  return(nmatr)
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

drop2 = function(tmp){
  return(substr(tmp, start = 21, stop = nchar(tmp)))  
}

textclean <- function (x)
{
  # lower case
  x = tolower(x)
  
  x <- gsub("'"," ",x) ; x <- gsub("’"," ",x) ; 
  x <- iconv(x, to='ASCII//TRANSLIT')
  x <- gsub("/"," ",x) # we don't want those wwwstuff # if we want
  x <- gsub("'","",x) 
  # In ascii translit step, many "'" will be gernerated. 
  # Simply deleting "'" will generate many new words
  x <- gsub("[^[:alnum:][:space:]/]", " ", x) #reserve "/" for wwwyoutubecom
  
  
  # remove number
  x = gsub("[[:digit:]]", " ", x)
  
  #replace special symbols in french
  
  x = gsub("[^[:alnum:]]", " ", x)
  
  # remove extra white spaces
  x = gsub("[ \t]{2,}", " ", x)
  x = gsub("^\\s+|\\s+$", "", x)
  
  return(x)
}



freqmatr <- function(x,rname,const,count,language)
{
  
  mycorpus <- Corpus(VectorSource(x),readerControl=list(language=language))
  #tolower doesn't work for mycorpus%>% TermDocumentMatrix here
  mycorpus <- mycorpus %>% tm_map(removeWords,myStopwords)
  mydic <- mycorpus
  mycorpus <- tm_map(mycorpus, stemDocument,language = language)
  
  mytdm <- TermDocumentMatrix(mycorpus, control = list(minWordLength = 1))
  
  # make the entries 1 since there are lots of weird replications in text
  if(count == FALSE)
  {
    nv <- length(mytdm$v)
    mytdm$v <- rep(1,nv)
  }
  
  
  freqterm <- findFreqTerms(mytdm, lowfreq = (mytdm$ncol)/const, highfreq = Inf)
  
  freqc <- mytdm[freqterm,]
  freqm <- spMatrix(nrow=freqc$ncol,ncol=freqc$nrow,i=freqc$j,j=freqc$i,x=freqc$v)
  
  stemcomplete = stemCompletion(freqterm, mydic)
  stemcompl = which(stemcomplete!="")
  freqterm[stemcompl] = stemcomplete[stemcompl]
  
  colnames(freqm) <- freqterm
  rownames(freqm) <- rname
  return(freqm)
}


ReplaceSynonyms <- function(x, syn) { 
  n = length(syn)
  for(i in 1:n)
  {
    syns = syn[[i]]$syns
    word = syn[[i]]$word
    msyns = which(!is.na(match(x,syns)))
    x[msyns] = rep(word,length(msyns))
  }
  return(x)
}

wordsmat_replacesym <- function(freqm,syn = synonyms)
{
  freqterm = ReplaceSynonyms(colnames(freqm),syn)
  
  colnames(freqm) <- freqterm
  
  freqm <- t(combine_matrix(t(freqm)))
  
  cna = which(freqterm=="")
  if(length(cna>0)){freqm <- freqm[,-cna]}
  freqm@x = rep(1,length(freqm@x))
  
  return(freqm)
}



combine_matrix <- function(A,counts=FALSE) #by row
{
  fan = A %>% rownames %>% unique
  cfan = match(A %>% rownames,fan)
  nx=length(fan);ny=length(A[1,])
  B = spMatrix(nrow = nx , ncol = ny, 
               i = cfan[A@i+1]  , j = A@j+1 , x = A@x ) 
  
  if (counts==FALSE)
  {
    nx = length(B@x)
    B@x = rep(1,nx)
  }
  rownames(B) = fan
  colnames(B) = colnames(A)
  return(B)
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


balloonGgPlot = function(M, nscale, logTran, sqrtTran, xlable, ylable, main, DotColour){
  
  n = nrow(M)
  d = ncol(M)
  
  M = M[,d:1]*nscale;
  
  scaleItMean = mean(abs(M))
  
  if(logTran){
    scaleItMean = mean(log(abs(M) +1))
  }
  if(sqrtTran){
    scaleItMean = mean(sqrt(abs(M)))
  }
  
  M = as.data.frame(as.table(as.matrix(M)))
  
  DotSize = rep(0,n*d);
  
  for(i in 1:(n*d)){
    dotSize = abs(M$Freq[i])
    if(logTran) dotSize= log(dotSize + 1)
    if(sqrtTran) dotSize = sqrt(dotSize)
    DotSize[i] = dotSize
    
  }
  
  M = data.frame(M, DotSize, DotColour)
  
  s <- ggplot(M, aes(Var1, Var2)) + xlab(NULL) + ylab(NULL)+
    geom_point( aes(size = DotSize, colour = DotColour))+
    xlab(xlable) +
    ylab(ylable) +
    ggtitle(main) +
    theme_bw() +
    theme(
      #legend.position = "none",
      axis.text.x = element_text(size=10, face = "bold"),
      axis.text.y = element_text(size=10, face = "bold"),
      axis.title.x = element_text(size=12, face = "bold"),
      axis.title.y = element_text(size=12, face = "bold"),
      title = element_text(size = 14, face = "bold")
    )
  s
}


ballGgPlot = function(BDF, ncluster, nscale, logTran, sqrtTran,type){
  if(type == "Citizen") {
    start = 0; end = 3; 
    xlable = "Citizen-Clusters"; main = "Average Number of Comments"}
  if(type == "Post") {
    start = 3; end = 6;
    xlable = "Post-Clusters"; main = "Average Number of Posts"}
  
  sel = (ncluster*start+1):(ncluster*end)
  M = BDF[sel,]
  n = nrow(M)
  d = ncol(M)
  M = M[,d:1];
  M = as.data.frame(as.table(as.matrix(M)))
  Type = substr(as.character(M$Var1), 1,1)
  Hvalue = substr(as.character(M$Var1), 5,5)
  Hvalue[which(Hvalue=="")] = "0"
  M$Var1 = substr(as.character(M$Var1), 4,4)
  ci = which(Type=="F")
  DotSize = M$Freq
  DotSize[ci] = M$Freq[ci]*nscale
  if(logTran) M$Freq[ci] = log(M$Freq[ci]+1)
  if(sqrtTran) M$Freq[ci] = sqrt(M$Freq[ci])
  
  DF_new = data.frame(M, DotSize, Type, Hvalue)
  levels(DF_new$Hvalue) = c("h = 0", "h = 10", "h = infinity")
  
  
  plot_labeller <- function(variable, value) {
    names_li <- list("0" = expression(h ~ "=" ~ 0), 
                     "1" = expression(h ~ "=" ~ 10), 
                     "2" = expression(h ~ "=" ~ infinity))
    return(names_li[value])
  }
  
  
  s <- ggplot(DF_new, aes(Var1, Var2)) + 
    geom_point( aes(size = DotSize)) + 
    xlab(xlable)+ ylab("Candidate-Walls") +
    ggtitle(main) +
    theme_bw() +
    theme(
      #legend.position = "none",
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=8),
      axis.title.x = element_text(size=12, face = "bold"),
      axis.title.y = element_text(size=12, face = "bold"),
      title = element_text(size = 14, face = "bold")
    )
  s + facet_grid(~Hvalue, labeller = plot_labeller)
}