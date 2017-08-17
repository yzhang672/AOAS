set.seed(1)

library(Matrix); library(irlba)
library("ggplot2")

source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/createAdjMat.R')


ncandidate = 8; 
main = "fan clusters vs candidates"
niteration = 100; nscale = 1000

#################### REGULARIZATION ###################### 

GraphLap = laplacian(allpostfans,"both",regular=TRUE)

###### Clustering Approach: Stochastic a prior BlockModel ######

Y = model.matrix(~as.factor(cc)-1)
yy = solve(t(Y)%*%Y)
ap = allpostfans%*%Y%*%yy
apn =t(apply(ap,1,function(x) return(x/sqrt(sum(x^2)))))
svdapn = svd(apn)

singular_value = svdapn$d
label = 1:length(singular_value)
svdLapd = data.frame(label, singular_value)

p <- ggplot(data=svdLapd, mapping=aes(x=label, y=singular_value)) +
  geom_point() + 
  scale_size_area() + 
  xlab("singular values") +
  ylab("labels") +
  ggtitle("SVD of the Citizen by Candidate network")
p + theme(plot.title = element_text(hjust = 0.5))   


B0result8=createB_candidate(cc,allpostfans,ncandidate,niteration)
B0result9=createB_candidate(cc,allpostfans,ncandidate+1,niteration)

ncluster = 9
B = B0result9[[1]];  clusters = B0result9[[2]]
rownames(B) = as.character(1:nrow(B))
colnames(B) = cn_sort
colball = as.factor(rep(1:ncluster,8))
balloonGgPlot(B, 1, FALSE, FALSE, "Citizen-Clusters", "Candidate-Walls", 
              main = "Average Number of Comments", colball) +
  scale_colour_manual(values = mycolor[1:ncluster],
                      name="Citizen-clusters",
                      breaks=factor(1:ncluster),
                      labels=mycolor[1:ncluster],
                      guide = "none"
  ) + theme(plot.title = element_text(hjust = 0.5))



labels = 1:length(clusters)
Candidate = factor(clusters)
names(Candidate) = NULL
levels(Candidate) = c(cn_sort,"Split")[1:ncluster]
Citizen_cluster = factor(clusters)
df = data.frame(Candidate,labels,Citizen_cluster)
length(which(clusters==9))

xlab = "Citizen-Clusters"; ylab = "Sizes"; main = "Sizes of the Citizen-Clusters";
GgPlotClusSizes(df, Citizen_cluster, ncluster, xlab, ylab, main)+ theme(plot.title = element_text(hjust = 0.5))
  
