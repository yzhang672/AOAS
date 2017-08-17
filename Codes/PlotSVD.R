library(ggplot2)

setwd("~/Dropbox/my project/shiny/FrenchElection")
load("IssuestrResults.RData")
i = 7
IssuestrResults[[i]][[1]]
svdACvalue = IssuestrResults[[i]][[2]]
label = 1:length(svdACvalue)
p <- ggplot(data=data.frame(label,svdACvalue), mapping=aes(x=label, y=svdACvalue)) +
  geom_point() + 
  theme_bw() + # background no ink
  theme(
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    title = element_text(size = 10, face = "bold"),
    legend.position="right"
  )  +
  scale_x_continuous(breaks = 1:10, limits = c(0.5,10.5)) +
  scale_size_area() + 
  ylab("Singular Values") +
  xlab("Labels") +
  ggtitle(expression(bold(paste("Singular Values of ", S, " with ", "h = 0.035"))))
p + theme(plot.title = element_text(hjust = 0.5))   



