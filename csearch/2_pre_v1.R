## header ======================================================================
# purpose: for first-year prensentation, focus on price
# author: Zoey Hu
# create date: 04/25/2018 
# last update: //2018 
#
# input file: search_clean.csv
# output file: 

library(tictoc)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(tidyr)

#setwd("/home/zhiying/Dropbox\ (GaTech)/csearch")
setwd("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
#sc <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/search_clean.csv")
dt <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/
            data/raw/tmp_jinyongliu_dxj_query_1.csv")
load("2prev1.RData")

## sample and explore ==========================================================

user <- unique(dt[, uid])
setkey(dt, uid)
spdt <- dt[user[sample(100)]]
fwrite(spdt, "sample_dt.csv")

## not every qid has complete rank_in_qid
sp1dt <- dt[uid == "405996808fe0a07160376bfea7befe7b"]
sp1sc <- sc[uid == "405996808fe0a07160376bfea7befe7b"]

## price: what users set in dataset and what available on App is different
aaprice <- setorder(sc[, .N, by = price], -N)
sp2dt <- dt[price_qry %like% "254以下"]
sp2dt2 <- dt[price_qry %like% "￥300以下￥1050以上￥200以下"]
# get csv file
aaprice_qry <- setorder(sc[, .N, by = price_qry], -N)
fwrite(aaprice_qry, "aaprice_qry.csv")
# how many price ranges in one query
aaprice_num <- setorder(sc[, .N, by = price_num], -N)
sum(aaprice_num[price_num > 1,N])/sum(aaprice_num[, N])  # 3548, 0.19%
#-- by user
aaprice_num_uid <- unique(sc[, .(uid, price_num)])[, .N, 
                                                   by = price_num][order(-N)]
sum(aaprice_num_uid[price_num > 1,N])/sum(aaprice_num_uid[, N])  # 1511, 0.3357%
#-- by user and queries
aaprice_num_uidqid <- (sc[price_num > 1, .(uid, qid_new)])[, .N, 
                                                   by = uid][order(-N)]

sum(aaprice_num_uidqid[N == 1, N])  # 583 users
    
## sort
aasort_qry <- setorder(sc[, .N, by = sort_qry], -N)
names(sc)



## time distribution ===========================================================

#*-- histogram: datenum, sessionnum, querynum ----------------------------------
p <- ggplot(usr, aes(pricenum)) + 
  xlim(-1, 10) + 
  xlab("number of price ranges")
g <- p + geom_histogram(aes(pricenum, ..density..),
                   binwidth = 1, 
                   fill = "#CF5B43")
g
ggsave("pricenum_bar.png", g)


#*-- plot 2x2 ------------------------------------------------------------------
fct <- function(var){
  # get unique var list
  test <- unique(usr[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click")
              )][order(-sbook)]
  test3 <- test2[, c(1, 4:5)]
  setkey(test3, uid)
  # merge
  test4 <- test[test3]
  # aggregate
  test5 <<- test4[V2 != "" & !is.na(V2), .N, by = .(V2, ibook, iclick)][order(-N)]
  test5 <- test5[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "not click", N])]
  test5 <- test5[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "click", N])]
  test5 <<- test5[ibook == "book" & iclick == "click", 
                  N2 := N/sum(test5[ibook == "book" & iclick == "click", N])]
  test6 <<- test5
  # setkey(test6, V2)
  # test6 <<- test6[aametro22[2:101, 1]]
  #plot
  p <<- qplot(V2, N2, data = test6, facets = ibook ~ iclick,
              ylab = "percentage",
              xlab = "number of days spent on searching",
              alpha = I(0.8),
              colour = "#CF5B43") +
    theme(axis.text=element_text(size=8, family="PingFang SC")) +
    theme(legend.position="none")
  # p <<- qplot(x = reorder(V2, N), log(N2), data = test6,
  #             facets = ibook ~ iclick,
  #             ylab = "percentage",
  #             xlab = var,
  #             alpha = I(0.1)) +
  #       coord_flip() +
  #       theme(axis.text=element_text(size=5, family="PingFang SC"))
  p
}

fct("keywordnum")
sum(test6[,N2])
ggsave("datenum_22.png", p)

fct <- function(var){
  # get unique var list
  test <- unique(usr[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click")
              )][order(-sbook)]
  test3 <- test2[, c(1, 4:5)]
  setkey(test3, uid)
  # merge
  test4 <- test[test3]
  # aggregate
  test5 <<- test4[V2 != "" & !is.na(V2), .N, by = .(V2, ibook, iclick)][order(-N)]
  test5 <- test5[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "not click", N])]
  test5 <- test5[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "click", N])]
  test5 <<- test5[ibook == "book" & iclick == "click", 
                  N2 := N/sum(test5[ibook == "book" & iclick == "click", N])]
  test6 <<- test5
  # setkey(test6, V2)
  # test6 <<- test6[aametro22[2:101, 1]]
  #plot
  p <<- ggplot(test6, aes(V2, N2)) + facet_grid(ibook ~ iclick) +
              ylab("percentage") +
              xlab("number of days spent on searching") + 
    geom_bar(stat = "identity", alpha = I(0.8), fill = "#CF5B43") +
    theme(axis.text=element_text(size=8, family="PingFang SC")) +
    theme(legend.position="none")
  # p <<- qplot(x = reorder(V2, N), log(N2), data = test6,
  #             facets = ibook ~ iclick,
  #             ylab = "percentage",
  #             xlab = var,
  #             alpha = I(0.1)) +
  #       coord_flip() +
  #       theme(axis.text=element_text(size=5, family="PingFang SC"))
  p
}

## save ========================================================================
save.image("2prev1.RData")
fwrite(sc, "search_clean.csv")