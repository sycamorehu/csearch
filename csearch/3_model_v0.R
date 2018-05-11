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
library(stargazer)

#setwd("/home/zhiying/Dropbox\ (GaTech)/csearch")
setwd("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
usr <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/user.csv")


## prepare data ================================================================
names(usr)  # done in the clean part
usr2 <- usr
setnames(usr2, c("uid","pricenum","starnum","keywordnum","htlstylenum",
                "brand1num","brand2num","distnum","ratingscorenum",
                "commercialareanum","facilitynum","reviewnum",
                "airporttrains2num","airporttrains1num","distrnum",
                "metro1num", "metro2num","hotd2num","hotd1num",
                "sortnum", "bedtypenum","breakfastnum","paytypenum",
                "datenum", "sessionnum","querynum","ifbook","ifclick",
                "type","criterianum","advancedays", "staydays","priceN","starN",
                "keywordN","htlstyleN","brand1N","brand2N",
                "distN","ratingscoreN","commericalareaN","facilityN",
                "reviewN","airporttrainsN","distrN","metro1N",
                "metro2N","hotd2N","hotd1N","sortN","bedtypeN",
                "breakfastN","paytypeN"))

names(usr2)



## model book ==================================================================

stargazer(usr2[,33:53])

#*-- model 1: book, with nums --------------------------------------------------

blogit1 <- glm(ifbook ~ pricenum + starnum + keywordnum + htlstylenum + 
                 brand1num + brand2num + distnum + ratingscorenum + 
                 commercialareanum + facilitynum + reviewnum + 
                 airporttrains2num + airporttrains1num + distrnum + metro1num + 
                 metro2num + hotd2num + hotd1num + sortnum + bedtypenum + 
                 breakfastnum + paytypenum, data = usr2,
              family = "binomial"(link = "logit"))

step(blogit1)

blogit1_2 <- glm(formula = ifbook ~ pricenum + starnum + keywordnum + 
                   htlstylenum + brand1num + brand2num + distnum + 
                   airporttrains1num + distrnum +  metro1num + metro2num + 
                   sortnum + bedtypenum + breakfastnum + paytypenum, 
                 family = binomial(link = "logit"), data = usr2)


#*-- model 2: book, with nums, other variables ---------------------------------

blogit2 <- glm(ifbook ~ pricenum + starnum + keywordnum + htlstylenum + 
                 brand1num + brand2num + distnum + ratingscorenum + 
                 commercialareanum + facilitynum + reviewnum + 
                 airporttrains2num + airporttrains1num + distrnum + metro1num + 
                 metro2num + hotd2num + hotd1num + sortnum + bedtypenum + 
                 breakfastnum + paytypenum + datenum + sessionnum +  
                 advancedays + staydays, data = usr2,
              family = "binomial"(link = "logit"))
step(blogit2)
names(usr2)

blogit2_2 <- glm(formula = ifbook ~ pricenum + starnum + keywordnum + 
                   htlstylenum +  brand1num + brand2num + distnum + 
                   airporttrains1num + distrnum + metro1num + metro2num + 
                   sortnum + bedtypenum + breakfastnum + paytypenum + 
                   datenum + sessionnum + querynum + criterianum + 
                   advancedays + staydays, family = binomial(link = "logit"), 
                 data = usr2)

stargazer(blogit1, blogit2, single.row = TRUE)
stargazer(blogit1_2, blogit2_2, single.row = TRUE, type = "text")


#*-- model 3: bool, with bool value --------------------------------------------

blogit3 <- glm(ifbook ~ priceN + starN + keywordN + htlstyleN + brand1N + 
                  brand2N + distN + ratingscoreN + commericalareaN + 
                  facilityN + reviewN + airporttrainsN + distrN + 
                  metro1N + metro2N + hotd2N + hotd1N + sortN + 
                  bedtypeN + breakfastN + paytypeN, data = usr2,
               family = "binomial"(link = "logit"))
step(blogit3)
blogit3_2 <- glm(formula = ifbook ~ priceN + starN + keywordN + htlstyleN + 
                   brand1N + brand2N + distN + ratingscoreN + commericalareaN + 
                   facilityN + reviewN + airporttrainsN + distrN + metro1N + 
                   metro2N + sortN + bedtypeN + breakfastN + paytypeN, 
                 family = binomial(link = "logit"), data = usr2)


#*-- model 4 -------------------------------------------------------------------

blogit4 <- glm(ifbook ~ priceN + starN + keywordN + htlstyleN + brand1N + 
                 brand2N + distN + ratingscoreN + commericalareaN + 
                 facilityN + reviewN + airporttrainsN + distrN + 
                 metro1N + metro2N + hotd2N + hotd1N + sortN + 
                 bedtypeN + breakfastN + paytypeN + datenum + sessionnum + 
                 advancedays + staydays, data = usr,
                family = "binomial"(link = "logit"))
step(blogit4)
blogit4_2 <- glm(formula = ifbook ~ priceN + starN + keywordN + htlstyleN + 
                   brand1N + brand2N + distN + ratingscoreN + commericalareaN + 
                   facilityN +  reviewN + airporttrainsN + distrN + metro1N + 
                   metro2N + sortN + bedtypeN + breakfastN + paytypeN + 
                   datenum + sessionnum + advancedays + staydays, 
                 family = binomial(link = "logit"), 
                 data = usr)


stargazer(blogit3, blogit4, single.row = TRUE)
stargazer(blogit3_2, blogit4_2, single.row = TRUE, type = "text")



## model click =================================================================

stargazer(usr2[,33:53])

#*-- model 1: click, with nums --------------------------------------------------

clogit1 <- glm(ifclick ~ pricenum + starnum + keywordnum + htlstylenum + 
                 brand1num + brand2num + distnum + ratingscorenum + 
                 commercialareanum + facilitynum + reviewnum + 
                 airporttrains2num + airporttrains1num + distrnum + metro1num + 
                 metro2num + hotd2num + hotd1num + sortnum + bedtypenum + 
                 breakfastnum + paytypenum, data = usr2,
               family = "binomial"(link = "logit"))



#*-- model 2: click, with nums, other variables ---------------------------------

clogit2 <- glm(ifclick ~ pricenum + starnum + keywordnum + htlstylenum + 
                 brand1num + brand2num + distnum + ratingscorenum + 
                 commercialareanum + facilitynum + reviewnum + 
                 airporttrains2num + airporttrains1num + distrnum + metro1num + 
                 metro2num + hotd2num + hotd1num + sortnum + bedtypenum + 
                 breakfastnum + paytypenum + datenum + sessionnum + 
                 advancedays + staydays, data = usr2,
               family = "binomial"(link = "logit"))


stargazer(blogit1, blogit2, clogit1, clogit2, single.row = TRUE)
stargazer(blogit1, blogit2, clogit1, clogit2, single.row = TRUE, type = "text")


#*-- model 3: click, with bool value --------------------------------------------

clogit3 <- glm(ifclick ~ priceN + starN + keywordN + htlstyleN + brand1N + 
                 brand2N + distN + ratingscoreN + commericalareaN + 
                 facilityN + reviewN + airporttrainsN + distrN + 
                 metro1N + metro2N + hotd2N + hotd1N + sortN + 
                 bedtypeN + breakfastN + paytypeN, data = usr2,
               family = "binomial"(link = "logit"))
step(clogit3)


#*-- model 4 -------------------------------------------------------------------

clogit4 <- glm(ifclick ~ priceN + starN + keywordN + htlstyleN + brand1N + 
                 brand2N + distN + ratingscoreN + commericalareaN + 
                 facilityN + reviewN + airporttrainsN + distrN + 
                 metro1N + metro2N + hotd2N + hotd1N + sortN + 
                 bedtypeN + breakfastN + paytypeN + datenum + sessionnum + 
                 advancedays + staydays, data = usr,
               family = "binomial"(link = "logit"))
summary(clogit4)

stargazer(blogit3, blogit4, clogit3, clogit4, single.row = TRUE)
stargazer(blogit3, blogit4, clogit3, clogit4, single.row = TRUE, type = "text")


## save ========================================================================
fwrite(usr, "user.csv")
fwrite(sc, "search_clean.csv")
