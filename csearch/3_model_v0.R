## header ======================================================================
# purpose: for 744 marketing models
# author: Zoey Hu
# create date: 04/28/2018 
# last update: //2018 
#
# input file: search_clean.csv / user.csv
# output file: 

library(tictoc)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape)
library(tidyr)
library(sqldf)

#setwd("/home/zhiying/Dropbox\ (GaTech)/csearch")
setwd("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
usr <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/user.csv")


## prepare data ================================================================
names(usr)
test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
            by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                              iclick = ifelse(sclick > 0, "click", "not click")
            )][order(-sbook)]
test3 <- test2[, c(1, 4:5)]

setkey(usr, uid)
usr <- usr[test3]
usr2 <- usr[ibook == "book", ibook := 1]
usr2 <- usr[ibook == "not book", ibook := 0]
usr2 <- usr[iclick == "click", iclick := 1]
usr2 <- usr[iclick == "not click", iclick := 0]
usr2[, ibook := as.numeric(ibook)]
usr2[, iclick := as.numeric(iclick)]


## model =======================================================================
summary(usr)
sapply(usr, sd)

names(usr2)
mlogit <- glm(ibook ~ pricenum + starnum + keywordnum + specnum + brand1num + 
                brand2num + distnum + filterscorenum + com2num + facnum + 
                filterquantitynum + sta2num + sta3num + distr2num + metro11num + 
                metro22num + hotd1num + hotd2num + sortnum + bedtype2num + 
                breakfast2num + paytype2num, data = usr2,
              family = "binomial"(link = "logit"))

summary(mlogit)

mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)


## save ========================================================================
fwrite(usr, "user.csv")