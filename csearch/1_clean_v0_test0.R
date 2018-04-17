## header ======================================================================
# purpose: to test "1_clean_v0"
# author: Zoey Hu
# create date: 04/15/2018 
# last update: //2018 
#
# - test unique, dplyr distinct and sqldf: 51.7, 102.5, 1495.9
# - test qid, search table: starttime, hotelnum, osversion
# - check starttime
# - check hotelnum
# - check price split

library(sqldf)



## separate into two tables ====================================================

#*-- dt.unique, dplyr.distinct, sqldf ------------------------------------------
tic("total:")  # total:: 1650.133 sec elapsed
tic("unique dt:")  # unique dt:: 51.658 sec elapsed
sc <- unique(dt[, c(1:4, 7:31, 43:44)])
toc()

tic("dplyr distinct")  # dplyr distinct: 102.503 sec elapsed
sc2 <- dplyr::distinct(dt[, c(1:4, 7:31, 43:44)])
toc()

tic("sqldf")  # sqldf: 1495.935 sec elapsed
sc3 <- sqldf("
             select distinct uid,qid_new,starttime,hotelnum,price_qry,star_qry,
             keyword_qry,cityid_qry,checkin,checkout,spec_qry,
             brand1_qry,brand2_qry,zone_qry,bookable_qry,dist_qry,
             fac_qry,filter_score,filter_quantity,com,sta,distr,
             metro1,metro2,hotd,sort_qry,bedtype,breakfast,paytype,
             os,osversion
             from dt
             ")
toc()
toc()


#*-- qid, search table ---------------------------------------------------------
# qid: 1823430
sc <- unique(dt[, c(1:4, 7:31, 43:44)])  # 3582487
sc2 <- unique(dt[, c(1:2, 4, 7:31, 43:44)])  # remove starttime: 1884119
sc3 <- unique(dt[, c(1:2, 7:31, 43:44)])  # remove s, hotelnum: 1823431
sc4 <- unique(dt[, c(1:2, 7:10, 12:31, 43:44)])  # remove s, h, checkin/out
sc5 <- unique(dt[, c(1:2, 7:31, 43)])  # remove s, h, osversion: 1823430
# sol: match starttime(first), hotelnum(first|average? check), remove osversion



## clean search table ==========================================================

#*-- check starttime -----------------------------------------------------------
startt <- unique(dt[, .(qid_new, starttime)])
startt[, rank := frank(starttime), by = qid_new]
setkey(startt, qid_new, rank)
a <- startt[1:100]
setkey(dt, qid_new)
b <- dt[a]
fwrite(b,"./data/check_startnqry.csv" )
# search criteria stays the same


#*-- check hotelnum ------------------------------------------------------------
qidhtlnum <- setorder(dt[, .(qid_new, hotelnum)][, .N, by = qid_new], -N)
a <- qidhtlnum[1:100]
setkey(a, qid_new)
b <- dt[a]
fwrite(b,"./data/check_htlnumnqry.csv" ) 
# from 12049 to 971 without changes in search criteria, any criteria missing?
# sol: max(hotelnum)


#*-- check price ---------------------------------------------------------------
a <- data.table(c("￥200以下￥200-300", "¥ 300-500", "￥1100以上￥650-1100", 
       "￥600-1000￥300-400￥200-300￥400-600"))
a[, num := stringr::str_match_all(V1, "([0-9]+)")]
a[, un := lapply(num, as.numeric)]
a[, min := lapply(un, min)] 
a[, max := lapply(un, max)]
a[V1 %like% "以下" , min := 0]
a[V1 %like% "以上", max := as.numeric(max) + 500]



