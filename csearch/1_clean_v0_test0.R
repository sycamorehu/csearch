## header ======================================================================
# purpose: to test "1_clean_v0"
# author: Zoey Hu
# create date: 04/15/2018 
# last update: 04/19/2018 
#
# - test unique, dplyr distinct and sqldf: 51.7, 102.5, 1495.9
# - test qid, search table: starttime, hotelnum, osversion
# - check starttime
# - check hotelnum
# - check price split
# - check brand1 and brand2
# - test loop for metro11
# - test collape into a user table

library(sqldf)

sc <- fread()



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



#*-- brand1 -------------------------------------------------------------------
aaabrand12 <- sc[brand2_qry %like% "高端连锁" | 
                   brand2_qry %like% "中端连锁" | 
                   brand2_qry %like% "快捷连锁"][, .N, 
                                             by = .(brand1_qry, brand2_qry)] [order(-N)]
aaabrand122 <- sc[!grepl("快捷", brand1_qry) & 
                    !grepl("中端", brand1_qry) & 
                    !grepl("高端", brand1_qry)][, .N, 
                                              by = .(brand1_qry,brand2_qry)][order(-N)]
aaabrand012 <- sc[, .N, by = .(brand1, brand1_qry, brand2_qry)][order(-N)]

## check brand1_qry "-999" or "酒店类型", brand2_qry "快捷连锁" etc


#*-- brand2 --------------------------------------------------------------------

## check those with only numbers, extract and count commas

unique(sc[str_match(brand2_qry, "([0-9]+)")[, 2] != "" & 
     brand2_qry != "-999" & 
     !grepl("7天", brand2_qry) &
     !grepl("速8", brand2_qry) & 
     !grepl("维也纳3好", brand2_qry) &
     !grepl("99旅馆", brand2_qry) &
     !grepl("99优选", brand2_qry) &
     !grepl("24K国际连锁", brand2_qry) &
     !grepl("都市118", brand2_qry) &
     !grepl("莫泰168", brand2_qry)&
     !grepl("景悦99", brand2_qry)&
     !grepl("A8连锁", brand2_qry), brand2_qry])

#*-- metro1 -------------------------------------------------------------------
aaametro11 <- sc[, .N, by = .(metro2, metro1, metro11)][order(metro2, -metro11)]
View(aaametro11)

for (i in 2:nrow(aaametro11)){
  if (aaametro11$metro2[i] == aaametro11$metro2[i-1] & 
      aaametro11$metro11[i-1] != "" & 
      aaametro11$metro11[i] == "") aaametro11$metro11[i] <- aaametro11$metro11[i-1] 
}


## user table ==================================================================

tic("data.table: ")  # 17.091
priceop <- sc[, .(uid, price)][, .(price2 = paste(price, collapse = ",")), by = uid]
priceop[, ":=" (price3= strsplit(price2, split = ","), pricenum = "")] 

for (i in 1:100){
  print(i)
  aa <- as.data.frame(priceop[i, price3],col.names = "price")$price
  aa <- aa[which(aa!="")]
  priceop$pricenum[i] <- length(unique(aa))
}
toc()


tic("dplyr: ")  # 20.458
pricenum0 <- subset(sc, select = c("uid", "price")) %>% group_by(uid) %>% summarise(price = paste(price, collapse = ",")) 
pricetest <- strsplit(pricenum0$price,split=",")
pricenum0$pricenum <- 999

for (i in 1:100){
  print(i)
  a <- as.data.frame(pricetest[i],col.names = "price")$price
  a <- a[which(a!="")]
  pricenum0$pricenum[i] <- length(unique(a))
}
toc()

## user clps

user <- data.table(unique(sc[, uid]))
setnames(user, "uid")
setkey(user, uid)

clps <- function(col){
  opt <<- sc[, .(uid, get(col))]
  setnames(opt, c("uid", col))
  opt <<- opt[, .(opt2 = paste(get(col), collapse = ",")), by = uid]
  opt[, opt3 := strsplit(opt2, split = ",")] 
  setnames(opt, c("uid", "opt2", paste0(col, "clps")))
  user <- user[opt[, 1:2], on = "uid"]
}

for (i in names(sc)[29:30]){
  clps(i)
}


## user opt

op <- function(x){
  opt <<- sc[, .(uid, get(x))]
  setnames(opt, c("uid", x))
  opt <<- opt[, .(opt2 = paste(get(x), collapse = ",")), by = uid]
  opt[, ":=" (opt3= strsplit(opt2, split = ","), optnum = "")] 
  
  for (i in 1:nrow(opt)){
    print(i)
    aa <- as.data.frame(opt[i, opt3],col.names = x)[,1]
    aa <- aa[which(aa!="")]
    opt$optnum[i] <- length(unique(aa))
  }
  
  opt <- opt[, c(1, 4)]
  setnames(opt, c("uid", paste0(x,"num")))
  return(opt)
}

