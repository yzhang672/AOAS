library(Matrix)
source('~/Dropbox/my project/frenchFacebook/code/codecleaned/functions.R')
load('~/Documents/study/my project/frenchFacebook/previous work/fordesk00/mydata/tables.RData')

lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

allparent_url = c(lepe$parent_url,joly$parent_url,bayr$parent_url,
                  holl$parent_url,mele$parent_url,dupo$parent_url,
                  sark$parent_url,pout$parent_url)

allfans=c(lepe$fan_id,joly$fan_id,bayr$fan_id,holl$fan_id,
          mele$fan_id,dupo$fan_id,sark$fan_id,pout$fan_id)

allpostfans=createA(allfans,allparent_url)

#save(allpostfans, file = "~/Dropbox/my project/frenchFacebook/data/allpostfans.RData")

n=rep(0,8)
n[1]=length(unique(lepe$parent_url));n[2]=length(unique(joly$parent_url));
n[3]=length(unique(bayr$parent_url));n[4]=length(unique(holl$parent_url));
n[5]=length(unique(mele$parent_url));n[6]=length(unique(dupo$parent_url));
n[7]=length(unique(sark$parent_url));n[8]=length(unique(pout$parent_url));
cc = c()
for(i in 1:length(n)) cc = c(cc, rep(i,n[i]))

cc_sort = cc
for(i in 1:8)
{
  ci = which(cc==i)
  cc_sort[ci] = cn_match[i]
}

#save(cc_sort, file = "~/Dropbox/my project/frenchFacebook/data/cc_sort.RData")

