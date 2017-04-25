library(Matrix)
library(XML);library(RCurl);library(devtools);library(qdap)
library(stringr);library(tm);library(RWeka);

source('~/Dropbox/my project/frenchFacebook/code/code_cleaned/functions.R')
setwd("~/Dropbox/my project/frenchFacebook/data/")
load("allpostfans.RData")
load("altogether.RData")
load("allposts.RData")

allcomtext = altogether$text
allcomid = rownames(altogether)
allposturl = drop2(colnames(allpostfans))

allposttext = allposts$posttext
allpostxt = textclean(allposttext)

load('~/Documents/study/my project/frenchFacebook/previous work/fordesk00/mydata/tables.RData')
lepe = tables$lepen
joly = tables$joly
bayr = tables$bayrou
holl = tables$hollande
mele = tables$melenchon
dupo = tables$dupont
sark = tables$sarkozy
pout = tables$poutou

lepetext <- lepe$text;jolytext <- joly$text;bayrtext <- bayr$text;holltext <- holl$text
meletext <- mele$text;dupotext <- dupo$text;sarktext <- sark$text;pouttext <- pout$text

lepecomtxt <- textclean(lepetext);jolycomtxt <- textclean(jolytext)
bayrcomtxt <- textclean(bayrtext);hollcomtxt <- textclean(holltext)
melecomtxt <- textclean(meletext);dupocomtxt <- textclean(dupotext)
sarkcomtxt <- textclean(sarktext);poutcomtxt <- textclean(pouttext)

allcomtxt <- c(lepecomtxt,jolycomtxt,bayrcomtxt,hollcomtxt,
               melecomtxt,dupocomtxt,sarkcomtxt,poutcomtxt)

#################################################################

allfanid = altogether$fan_id
allcomwords_binary <- freqmatr(allcomtxt,allfanid,1000,count=FALSE, language = "fr")
allcomwords_binary <- wordsmat_replacesym(allcomwords_binary)
allfanwords_binary = combine_matrix(allcomwords_binary,counts=FALSE)

allpostid = c(allposts$posturl,drop2(altogether$parent_url))
allcontentxt = c(allpostxt,allcomtxt)
allcontentwords_binary <- freqmatr(allcontentxt,allpostid,1000,count=FALSE, language = "fr")
allcontentwords_binary <- wordsmat_replacesym(allcontentwords_binary)
allpostconwords_binary = combine_matrix(allcontentwords_binary,counts=FALSE)

