## header ======================================================================
# purpose: 1. translate Chinese to English; 
#          2. sep some col values; 
#          3. set proper new variables
# author: Zoey Hu
# create date: 04/14/2018 
# last update: //2018 
#
# input file: raw data 
#            "tmp_jinyongliu_dxj_query_1.csv.csv"
# output file: 
# 

library(tictoc)
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)

setwd("~/a/ctrip")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters



## load data ===================================================================

dt <- fread("~/a/ctrip/data/raw/tmp_jinyongliu_dxj_query_1.csv", 
                        colClasses = list(character = c(21, 34:38)))



## separate into two tables ====================================================

#*-- search table: 1823430 x 27 ------------------------------------------------
sc <- unique(dt[, c(1:2, 7:31, 43)])  # remove s, h, osversion
setkey(sc, qid_new)

#*-- hotel table: 8648511 x 16 -------------------------------------------------
htl <- dt[, c(1:3, 5:6, 32:42)]



## clean search table ==========================================================

#*-- starttime, rank -----------------------------------------------------------
## prepare starttime
startt <- unique(dt[, .(qid_new, starttime)])
startt[, rank := frank(starttime), by = qid_new]
setkey(startt, qid_new)
startt2 <- aa[rank == 1]

## match
sc <- sc[startt2]
sc[, rank := NULL]

## rank
sc[, rank := frank(starttime), by = qid_new]


#*-- checkin, checkout ---------------------------------------------------------
sc$checkin <- ymd(sc$checkin)
sc$checkout <- ymd(sc$checkout)


#*-- price_qry -----------------------------------------------------------------

# Hypothesis: people with proper options(e.g. 2 not 6) has highest cr

## test on aprice_qry
aprice_qry[, price := gsub("¥", "￥", price_qry)]
aprice_qry[, count := stringr::str_count(price, "￥")]

## clean price
sc[, price := gsub("¥", "￥", price_qry)]
sc[, price := gsub("999", "", price)]
sc[price == "不限", price := ""]
sc[price == "-", price := ""]
sc[price == "999", price := ""]
sc[price == "1", price := "￥0-150"]
sc[price == "2", price := "￥150-300"]
sc[price == "3", price := "￥300-450"]
sc[price == "4", price := "￥450-600"]
sc[price == "5", price := "￥600-1000"]
sc[price == "6", price := "￥1000以上"]

## price_num
sc[, price_num := stringr::str_count(price, "￥")]

## split price options
sc[, price1 := stringr::str_match_all(price, "([0-9]+)")]
sc[, price2 := lapply(price1, as.numeric)]

sc[, price_min := lapply(price2, min)]
sc[price == "", price_min := ""]
sc[, price_min := lapply(price_min, as.numeric)]
sc[price %like% "以下" , price_min := 0]

sc[, price_max := lapply(price2, max)]
sc[price == "", price_max := ""]
sc[, price_max := lapply(price_max, as.numeric)]
sc[price %like% "以上", price_max := as.numeric(price_max) + 500]

## price_range
sc[, price_range := as.numeric(price_max) - as.numeric(price_min)]


#*-- star_qry ------------------------------------------------------------------
sc[star_qry %like% "二星及以下/经济", star2 := 1]
sc[star_qry %like% "三星/舒适", star3 := 1]
sc[star_qry %like% "四星/高档", star4 := 1]
sc[star_qry %like% "五星/豪华", star5 := 1]
sc[star_qry %like% "快捷连锁", star2 := 1]
sc[star_qry %like% "三钻/舒适", star3 := 1]
sc[star_qry %like% "四钻/高档" , star4 := 1]
sc[star_qry %like% "五钻/豪华" , star5 := 1]
sc[star_qry %like% "1" , star1 := 1]
sc[star_qry %like% "2" , star2 := 1]
sc[star_qry %like% "3" , star3 := 1]
sc[star_qry %like% "4" , star4 := 1]
sc[star_qry %like% "5" , star5 := 1]

sc[, star_num := sum(star1, star2, star3, star4, star5, 
                     na.rm = TRUE), by = 1:nrow(sc)]


#*-- keyword_qry ---------------------------------------------------------------
sc[keyword_qry == "999", keyword := ""]
sc$keyword <- gsub("&", "", sc$keyword_qry)
sc$keyword <- gsub("[/*]", "", sc$keyword)
sc[, keyword_len := nchar(keyword), by = 1:nrow(sc)]



#*-- click and book ------------------------------------------------------------
## click and book
clickbook <- htl[, .(click = sum(click_bool), 
                            book = sum(booking_bool)), by = .(uid, qid_new)]
clickbook[, .N, by = .(click, book)]  # check
setkey(clickbook, qid_new)
sc <- sc[clickbook]

## click rate and conversion rate
uclickbook <- clickbook[, .(click = sum(click), book = sum(book)), by = uid]
ctr <- uclickbook[, .N, by = click]
cr <- uclickbook[, .N, by = book]
(sum(ctr$N) - ctr[1, 2])/sum(ctr$N)  # 0.3983098
(sum(cr$N) - cr[1, 2])/sum(cr$N)  # 0.04

## ifclick and ifbook
sc[, ifclick := sapply(click, function(x) if (x > 0) 1 else 0)]
sc[, ifbook := sapply(book, function(x) if (x > 0) 1 else 0)]
sc[click > 1 & book > 0][1:100, 47:50]  # check

