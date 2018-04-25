## header ======================================================================
# purpose: to translate columns with Chinese values into English
# author: Zoey Hu
# create date: 04/12/2018 
# last update: 04/13/2018 
#
# input file: raw data 
#            "tmp_jinyongliu_dxj_query_1.csv.csv"
# 
# they're changing the search area: 20180407 and 20180415 different
# archive: https://web.archive.org/web/20171006185044/http://hotels.ctrip.com:80
# would really like to know sort; or 9 a default?
# spec: number vs spec
# dist: under which condition? zoom map?



library(tictoc)
library(data.table)
library(lubridate)

setwd("~/a/ctrip")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters



## load data ===================================================================

dt <- fread("/home/zhiying/Dropbox (GaTech)/csearch/data/raw/tmp_jinyongliu_dxj_query_1.csv", 
            colClasses = list(character = c(21, 34:38)))



## explore data ================================================================

#*-- starttime -----------------------------------------------------------------
starttime <- setorder(dt[, .N, by = as_date(starttime)], -N)  # 180401 to 180407

#*-- cityid --------------------------------------------------------------------
cityid <- setorder(dt[, .N, by = cityid_qry], -N)  # cityid = 2

#*-- hotelid -------------------------------------------------------------------
hotelid <- setorder(dt[, .N, by = hotelid], -N)  # 47250 hotels

#*-- uid, qid ------------------------------------------------------------------
uid <- setorder(dt[, .N, by = uid], -N)  # 404813
qid <- setorder(dt[, .N, by = qid_new], -N)  # qid_new as token, 1823430
uidqid<- setorder(dt[, .N, by = .(uid, qid_new)], -N)
uidnqid <- setorder(unique(dt[,1:2])[, .N, by = uid], -N)

#*-- rank ----------------------------------------------------------------------
rank <- setorder(dt[, .N, by = rank_in_qid], -N)
hist(dt$rank_in_qid, xlim = c(0,200), breaks = 100) # no limit as 10, 20, 30

#*-- hotelnum ------------------------------------------------------------------
hotelnum <- setorder(dt[, .N, by = hotelnum], -N) # highest not 1, but 219
hist(dt$hotelnum)

#*-- all other -----------------------------------------------------------------
colnames(dt)

for (i in colnames(dt)[7:44]){
  print(i)
  assign(paste("a", i, sep = ""), setorder(dt[, .N, by = i], -N))
}

# price: can choose multiple "￥600-1000￥300-400￥200以下￥200-300￥400-600", 
#        can set as "￥236以下"
#        some are highly mixed: "￥300以下￥1050以上￥200以下"

# keyword: what does "&*" mean? 
#          "迪士尼&*&*&*&*"
#          "迪士尼"
#          "*&人民广场&*&*&*"
#          "*&*&复旦大学(邯郸校区)&*&*"
#          "上海迪士尼度假区&上海迪士尼度假区&*&*&*"
           
# check-in  # "2018-03-29" to "2019-04-06"
max(ymd(na.omit(acheckin$checkin[acheckin$checkin!=-999])))
min(ymd(na.omit(acheckin$checkin[acheckin$checkin!=-999])))

# check-out  # "2018-03-30" to "2019-05-01"
max(ymd(na.omit(acheckout$checkout[acheckout$checkout!=-999])))
min(ymd(na.omit(acheckout$checkout[acheckout$checkout!=-999])))
           
# spec  # have to retrieve all the combinations; can we get a separate one?
#         "豪宅农家乐购物便捷美食林休闲度假老洋房精品酒店"
#         "休闲度假浦江观景精品酒店"

# brand 1  # "酒店类型 41320"?
#            "Q+7天莫泰快捷连锁如家快捷汉庭锦江之星"
          
# zone, bookable  # all -999

# dist_qry  # diff "401米内(地图范围)" and "754米内"
#             user distance? 

# com  #"北外滩地区人民广场地区南外滩地区淮海路商业区新天地地区"

# sta  # can tell train station from airport

# ismobshare, isreturn

# fh_price, starttime_click, starttime_book
hist(dt$fh_price, breaks = 1000, xlim = c(0, 3000))
