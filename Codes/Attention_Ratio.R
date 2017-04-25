library(ggplot2)
source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load('allpostfans.RData'); load("cc.RData")

## match candidates with their ranks in the election
cc_sort = cc
for(i in 1:8) { ci = which(cc==i); cc_sort[ci] = cn_match[i] }

Cand_level = c("All",cn_sort)
rs = rowSums(allpostfans)

Rs = NULL
for(i in 1:8)
{ subMat = allpostfans[,which(cc_sort == i)]
  Rs = rbind(Rs,rowSums(subMat))}

rs_max = rep(apply(Rs,2,max),2)
rs_whichmax = as.character(c(rep(0,length(rs)),apply(Rs, 2, which.is.max)))
rs_sum = rep(rs,2)
DegreeType = rep("All",length(rs_sum))

rs_whichmax = as.factor(rs_whichmax)
levels(rs_whichmax) = Cand_level
rs_prop = rs_max/rs_sum
RS = data.frame(rs_whichmax,rs_sum, rs_max, rs_prop, DegreeType)

del = which(RS$rs_sum<11)
RS_select = RS[-del,]

ci = which(is.na(match(RS_select$rs_whichmax,c("Poutou"))))

g = 
  ggplot(RS_select[ci,], aes(x=rs_prop, ..density..,fill = rs_whichmax, colour = rs_whichmax, 
                             alpha = 0.2)) + 
  geom_histogram(origin = 1/10, binwidth = 1/10) + 
  #geom_density(adjust = 1) +
  theme_bw() + # background no ink
  theme(
    axis.text.x = element_text(size=6),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    title = element_text(size = 10, face = "bold"),
    legend.position="right"
  )  +
  scale_x_continuous(breaks = (1:4)/4, limits = c(0,1)+1/8) +
  ggtitle("Attention-Ratio of Citizens\n (with at least 10 Comments)") +
  xlab("Attention-Ratio") + ylab("Density") +
  scale_fill_manual(values = c("black",mycolor[1:8]),
                    name="Candidates",
                    breaks=as.character(0:8),
                    labels=Cand_level)+
  scale_colour_manual(values = c("black",mycolor[1:8]),
                      name="Candidates",
                      breaks=as.character(0:8),
                      labels=Cand_level)

g + facet_wrap(~rs_whichmax, ncol = 4) +
  theme(legend.position="none")


g = 
  ggplot(RS, aes(x = rs_sum,y = rs_prop)) + 
  stat_binhex(binwidth = c(1/5, 1/10))+
  scale_x_log10(
    breaks = 10^c(0,1,2,3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_continuous(breaks = (1:4)/4, limits = c(1/8,1.05)
  ) +
  ggtitle("Hexagonal bins of Citizens") +
  xlab("Citizen-Degree") + 
  ylab("Attention-Ratio") +
  theme_bw() +# background no ink
  theme(
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    title = element_text(size = 10, face = "bold"),
    legend.position="right"
  ) +
  scale_fill_gradient(name = "Number of Citizens\n in the hexagon", trans = "log10",
                      low = "white", high = "black", 
                      breaks = 10^c(1,2,3,4),
                      labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
g+facet_wrap(~rs_whichmax, ncol = 3) 




levels(rs_whichmax) = 0:8
citizen_favgroups = rs_whichmax[which(rs_whichmax!="0")]
citizen_favgroups = as.numeric(citizen_favgroups)-1
ncluster = 8; nscale = 1000

Bresult = createB_general_nosort(allpostfans, citizen_favgroups, cc)
sortPosClus = cn_match

B = Bresult[,sortPosClus];
round(B*1000)
rownames(B) = as.character(1:ncluster);
colnames(B) = cn_sort
colball = as.factor(rep(1:ncluster,8))

balloonGgPlot(B, nscale, TRUE, FALSE, 
              "Citizen-Groups", "Candidate-Walls", 
              main = "Average Number of Comments", colball) +
  scale_colour_manual(values = mycolor[1:ncluster],
                      name="Citizen-groups",
                      breaks=factor(1:ncluster),
                      labels=mycolor[1:ncluster],
                      guide = "none"
  ) 

clusters = citizen_favgroups
labels = 1:length(clusters)
Candidate = factor(clusters)
names(Candidate) = NULL
levels(Candidate) = Cand_level[-1]
Citizen_cluster = factor(clusters)
df = data.frame(Candidate,labels,Citizen_cluster)

xlab = "Citizen-Groups"; ylab = "Sizes"; main = "Sizes of the Citizen-Groups";
GgPlotClusSizes(df, Citizen_cluster, ncluster, xlab, ylab, main)


## Compare ###
load("~/Dropbox/my project/frenchFacebook/data/canstr_Bresult.RData")
num = 8
length(which(citizen_favgroups==B0result8[[2]] & 
               citizen_favgroups<num & 
               B0result8[[2]]<num))/
  length(citizen_favgroups[which(citizen_favgroups<num)])


length(which(B0result9[[2]]==B0result8[[2]] & 
               B0result9[[2]]<8 & 
               B0result8[[2]]<8))/
  length(citizen_favgroups[which(B0result9[[2]]<8)])
