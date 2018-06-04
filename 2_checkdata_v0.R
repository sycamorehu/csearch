## header ======================================================================
# purpose: check data issues
# author: Zoey Hu
# create date: 06/01/2018 
# last update: //2018 
#
# notation:
#          - *: things to be consider while proceed to code; pitfalls
#          - %: techniques
#          - -: notes, numbers


library(tictoc)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(tidyr)
library(sqldf)
library(stargazer)
library(gtable)

#setwd("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch")
setwd("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
# sc <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/search_clean.csv")
dt <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/data/raw/tmp_jinyongliu_dxj_query_1.csv")
# dt <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/data/raw/tmp_jinyongliu_dxj_query_1.csv")
usr <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/user.csv")
# usr <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/user.csv")
rm(list = setdiff(ls(), c("sc", "usr", "dt")))


## check with sample ===========================================================

uidsp <- setnames(data.table(sample(usr[,uid], 100)), "uid")
spdt <- dt[uidsp, on = "uid"] 
names(sc)
sp <- sc[uidsp, on = "uid"][, c(1:2, 71, 74, 76, 6:8, 29:56)][order(uid, query_order)]
fwrite(spdt, "check_sampledt.csv")
fwrite(sp, "check_sample.csv")