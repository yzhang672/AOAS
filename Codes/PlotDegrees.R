library(ggplot2)
source('~/Dropbox/my project/frenchFacebook/code/text_to_facetgrid.R')
source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load('allpostfans.RData')
load("cc.RData")

cc_sort = cc
for(i in 1:8)
{
  ci = which(cc==i)
  cc_sort[ci] = cn_match[i]
}

cs = list(); rs = list()
cs[[1]] = colSums(allpostfans)
rs[[1]] = rowSums(allpostfans)

for(i in 1:8)
{
  subMat = allpostfans[,which(cc_sort == i)]
  cs[[i+1]] = colSums(subMat)
  rs[[i+1]] = rowSums(subMat)
}


Cand_level = c("All",cn_sort)
Df<- NULL
rscs = list(rs,cs)
for(i in 1:9)
{
  for(j in 1:2)
  {
    degrees <- rscs[[j]][[i]]
    degrees <- degrees[degrees>0]
    bin_max <- ceiling(log10(max(degrees)))
    bin_breaks <- c(10^(seq(0,bin_max,0.2)))-0.5
    degrees_bin <- cut(degrees, breaks = bin_breaks)
    bin_point <- c(0.5,bin_breaks[2:(length(which(bin_breaks<=max(degrees))))])
    dt <- data.frame(degrees,degrees_bin)
    dt_ecdf <- empirical_cdf(dt$degrees, ubounds = list(x=bin_point))
    Degree_type <- rep(c("Citizen Degrees","Post Degrees")[j],nrow(dt_ecdf))
    Candidate <- rep(Cand_level[i],nrow(dt_ecdf))
    Degree_points <- dt_ecdf$x
    Tail <- 1-dt_ecdf$CDF
    df <- data.frame(Candidate, Degree_type,Degree_points, Tail)
    Df <- rbind(Df,df)
  }
}

a = ggplot(Df[which(Df$Candidate=='All' & Df$Degree_type=="Citizen Degrees"),], 
           aes(Degree_points, Tail, colour = factor(Candidate))) +
  xlab("Citizen-Degrees") + ylab("Upper Tail") +
  geom_point(na.rm = TRUE) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  theme_bw() + # background no ink
  scale_colour_manual(values = c("black",mycolor[1:8]),
                      name="Candidates",
                      breaks=as.character(0:8),
                      labels=Cand_level)  +
  ggtitle("Upper Tail of Citizen-Degrees") +
  theme(
    axis.text.x = element_text(size=10, face = "bold"),
    axis.text.y = element_text(size=10, face = "bold"),
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=12, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    legend.position="none"
  )
a + annotation_logticks() 


b = ggplot(Df[which(Df$Candidate != "All" & Df$Degree_type=="Post Degrees"),], 
           aes(Degree_points, Tail, colour = Candidate)) +
  xlab("Post-Degrees") + ylab("Upper Tail") +
  geom_point(aes(shape = Candidate)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  ggtitle("Upper Tail of Post-Degrees") +
  theme_bw() +# background no ink
  theme(
    axis.text.x = element_text(size=10, face = "bold"),
    axis.text.y = element_text(size=10, face = "bold"),
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=12, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    legend.position="right"
  ) +
  scale_colour_manual(values = c(mycolor[1:8]),
                      name="Candidates",
                      breaks=cn_sort,
                      labels=cn_sort
  ) +
  scale_shape_manual( values = c(15,16,17,18,3,0,1,2),
                      name="Candidates",
                      breaks=cn_sort,
                      labels=cn_sort
  ) 
b + annotation_logticks() 


