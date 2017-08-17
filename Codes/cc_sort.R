source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data")
load("cc.RData")

cc_sort = cc
for(i in 1:8)
{
  ci = which(cc==i)
  cc_sort[ci] = cn_match[i]
}

#save(cc_sort, file = "cc_sort.RData")