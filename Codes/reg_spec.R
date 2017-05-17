setwd("~/Dropbox/my project/frenchFacebook/data")
set.seed(1)

library(Matrix); library(irlba);

load('allpostfans.RData')
load('cc.RData')

ncandidate = 8; 

GraphLap = laplacian(allpostfans,"both",regular=TRUE)
svdL <- irlba(GraphLap, nu=10, nv=10)

plot(svdL$d)

ncluster <- 8
v = svdL$v[,1:ncluster]
v = t(apply(v,1,function(x) return(x/sqrt(sum(x^2)))))
u = svdL$u[,1:ncluster]
u = t(apply(u,1,function(x) return(x/sqrt(sum(x^2)))))

niteration <- 1000;
km_v = kmeans(v, centers = ncluster, nstart = niteration)
km_u = kmeans(u,centers = ncluster, nstart = niteration)

Bresult = createB_general(allpostfans, km_u$cluster, cc, ncluster,sorttype="ClusterSize" )

sortPosClus = match(cn_sort, cn)

B = Bresult[[1]][,sortPosClus];
round(B*1000)
B = B[c(6,8,1,2,7,3,5,4),]
rownames(B) = as.character(1:ncluster);
colnames(B) = cn_sort
colball = rep("black",8*ncluster)
library(ggplot2)
balloonGgPlot(B, 1, FALSE, FALSE, 
              "Citizen-Clusters", "Candidate-Walls", main = "", colball) +
  ggtitle("Average Number of Comments") +
  theme_bw() +
  theme(
    #legend.position = "none",
    axis.text.x = element_text(size=10, face = "bold"),
    axis.text.y = element_text(size=10, face = "bold"),
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=12, face = "bold"),
    title = element_text(size = 14, face = "bold")
  )+
  scale_colour_manual(values = "black",
                      name="Citizen-clusters",
                      breaks=factor(0),
                      labels="black",
                      guide = "none"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))
