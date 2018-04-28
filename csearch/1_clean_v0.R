## header ======================================================================
# purpose: 1. translate Chinese to English; 
#          2. sep some col values; 
#          3. set proper new variables
# author: Zoey Hu
# create date: 04/14/2018 
# last update: 04/25/2018 
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
library(ggplot2)
library(reshape)
library(tidyr)

#setwd("/home/zhiying/Dropbox\ (GaTech)/csearch")
setwd("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
#sc <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/search_clean.csv")
load("tmp.RData")
usr <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/user.csv")



## separate into two tables ====================================================

#*-- search table: 1823430 x 27 ------------------------------------------------
# need to add hotelnum
sc <- unique(dt[, c(1:2, 7:31, 43)])  # remove s, h, osversion
setkey(sc, qid_new)

#*-- hotel table: 8648511 x 16 -------------------------------------------------
htl <- dt[, c(1:3, 5:6, 32:42)]



## clean search table ==========================================================

#*-- extract values and frequencies --------------------------------------------
names(sc)
for (i in colnames(sc)[3:28]){
  print(i)
  assign(paste("a", i, sep = ""), setorder(dt[, .N, by = i], -N))
}

#*-- price ---------------------------------------------------------------------
names(sc)
View(aprice_qry)
sc[, price := price_qry]
sc[price == "1", price := "￥0-150"]
sc[price == "2", price := "￥150-300"]
sc[price == "3", price := "￥300-450"]
sc[price == "4", price := "￥450-600"]
sc[price == "5", price := "￥600-1000"]
sc[price == "6", price := "￥1000以上"]
sc[price == "不限" | price == "-999", price := ""]
sc[, price := gsub("以上", "andAbove", price)]
sc[, price := gsub("以下", "andBelow", price)]
sc[, price := gsub("￥", "¥", price)]
sc[, price := gsub(" ", "", price)]
## check
unique(sc[price == "", price_qry])
assign(paste("aa", "price", sep = ""), setorder(sc[, .N, by = price], -N))
View(aaprice)
aaaprice <- sc[, .N, by = .(price_qry, price)]
View(aaaprice)
## usage 
tmp <- unique(sc[, .(uid, price)])
tmp1 <- tmp[price != ""][, .N, by = price]
sum(tmp1$N)/404813  # 21.26% (86080)
sum(tmp1$N)

#*-- star ----------------------------------------------------------------------
names(sc)
View(astar_qry)
sc[, star := ""]
sc[star_qry %like% "1", star := "0-2,"]
sc[star_qry %like% "2", star := paste(star, "0-2,")]
sc[star_qry %like% "快捷连锁", star := paste(star, "0-2,")]
sc[star_qry %like% "二星及以下/经济", star := paste(star, "0-2,")]
sc[star_qry %like% "3", star := paste(star, "3,")]
sc[star_qry %like% "三星/舒适", star := paste(star, "3,")]
sc[star_qry %like% "三钻/舒适", star := paste(star, "3,")]
sc[star_qry %like% "4", star := paste(star, "4,")]
sc[star_qry %like% "四星/高档", star := paste(star, "4,")]
sc[star_qry %like% "四钻/高档", star := paste(star, "4,")]
sc[star_qry %like% "5", star := paste(star, "5,")]
sc[star_qry %like% "五钻/豪华", star := paste(star, "5,")]
sc[star_qry %like% "五星/豪华", star := paste(star, "5,")]
sc[, star := gsub("0-2, 0-2", "0-2", star)]
sc[, star := substr(star, 1, nchar(star)-1)]
sc[, star := gsub(" ", "", star)]
unique(sc[star == "", star_qry])
assign(paste("aa", "star", sep = ""), setorder(sc[, .N, by = star], -N))
View(aastar)
aaastar <- sc[, .N, by = .(star_qry, star)]
View(aaastar)
## usage 
tmp <- unique(sc[, .(uid, star)])
tmp1 <- tmp[star != ""][, .N, by = star]
sum(tmp1$N)/404813  # 28.15% (113945)
sum(tmp1$N)

#*-- keyword -------------------------------------------------------------------
names(sc)
View(akeyword_qry)
sc[, keyword := keyword_qry]
sc[keyword == "-999", keyword := ""]
sc[, keyword := gsub("&", "", keyword)]
sc[, keyword := gsub("[/*]", "", keyword)]
unique(sc[keyword == "", keword_qry])
assign(paste("aa", "keyword", sep = ""), setorder(sc[, .N, by = keyword], -N))
View(aakeyword)
aaakeyword <- sc[, .N, by = .(keyword_qry, keyword)]
View(aaakeyword)
## usage 
tmp <- unique(sc[, .(uid, keyword)])
tmp1 <- tmp[keyword != ""][, .N, by = keyword]
sum(tmp1$N)/404813  # 60.98% (246857)
sum(tmp1$N)

#*-- spec ----------------------------------------------------------------------
names(sc)
View(aspec_qry)
sc[, spec := ""]
sc[spec_qry %like% "客栈", spec := "inn,"]
sc[spec_qry %like% "公寓", spec := paste(spec, "apartment,")]
sc[spec_qry %like% "别墅", spec := paste(spec, "villa,")]
sc[spec_qry %like% "青年旅", spec := paste(spec, "youth hostel,")]
sc[spec_qry %like% "农家乐", spec := paste(spec, "agritainment,")]
sc[spec_qry %like% "民居", spec := paste(spec, "homestay,")]
sc[spec_qry %like% "老洋房", spec := paste(spec, "old-style villa,")]
sc[spec_qry %like% "浦江观景", spec := paste(spec, "river view,")]
sc[spec_qry %like% "米其林餐厅", spec := paste(spec, "michelin restaurants,")]
sc[spec_qry %like% "度假", spec := paste(spec, "vocation,")]
sc[spec_qry %like% "豪宅", spec := paste(spec, "mansion,")]
sc[spec_qry %like% "海滨风光", spec := paste(spec, "coast view,")]
sc[spec_qry %like% "亲子", spec := paste(spec, "family,")]
sc[spec_qry %like% "情侣", spec := paste(spec, "romantic,")]
sc[spec_qry %like% "商务出行", spec := paste(spec, "business,")]
sc[spec_qry %like% "精品酒店", spec := paste(spec, "selection,")]
sc[spec_qry %like% "设计师酒店", spec := paste(spec, "designer hotel,")]
sc[spec_qry %like% "温泉", spec := paste(spec, "hot spring,")]
sc[spec_qry %like% "购物便捷", spec := paste(spec, "shopping mall,")]
sc[spec_qry %like% "老饭店", spec := paste(spec, "old-style hotel,")]
sc[spec_qry %like% "美食林", spec := paste(spec, "genuine food,")]
sc[spec_qry %like% "全日房", spec := paste(spec, "all-day room,")]
sc[spec_qry %like% "钟点房", spec := paste(spec, "hour room,")]
sc[str_match(spec_qry, "([0-9]+)")[, 2] != "" & 
     spec_qry != "-999" & spec_qry != "0", spec := spec_qry]

sc[nchar(spec) != 1, spec := substr(spec, 1, nchar(spec)-1)]
unique(sc[spec == "", spec_qry])
assign(paste("aa", "spec", sep = ""), setorder(sc[, .N, by = spec], -N))
View(aaspec)
aaaspec <- sc[, .N, by = .(spec_qry, spec)]
View(aaaspec)

## usage 
tmp <- unique(sc[, .(uid, spec)])
tmp1 <- tmp[spec != ""][, .N, by = spec]
sum(tmp1$N)/404813  # 6.25% (25289)
sum(tmp1$N)

#*-- brand1 --------------------------------------------------------------------
sc[, brand1 := ""]
sc[brand2_qry %like% "全部高端连锁" |
     brand2_qry %like% "CitiGO" |
     brand2_qry %like% "JW万豪" |
     brand2_qry %like% "W酒店" |
     brand2_qry %like% "万丽" |
     brand2_qry %like% "万怡" |
     brand2_qry %like% "万枫" |
     brand2_qry %like% "万豪" |
     brand2_qry %like% "万达" |
     brand2_qry %like% "万达瑞华" |
     brand2_qry %like% "丽思卡尔顿" |
     brand2_qry %like% "丽笙" |
     brand2_qry %like% "中南海滨" |
     brand2_qry %like% "书香世家" |
     brand2_qry %like% "云栖" |
     brand2_qry %like% "亚朵" |
     brand2_qry %like% "假日" |
     brand2_qry %like% "凯宾斯基" |
     brand2_qry %like% "凯悦嘉寓酒店" |
     brand2_qry %like% "凯悦嘉轩酒店" |
     brand2_qry %like% "凯悦酒店" |
     brand2_qry %like% "千禧" |
     brand2_qry %like% "半岛" |
     brand2_qry %like% "华尔道夫" |
     brand2_qry %like% "华美达" |
     brand2_qry %like% "卓美亚" |
     brand2_qry %like% "名致" |
     brand2_qry %like% "君亭" |
     brand2_qry %like% "君亭酒店" |
     brand2_qry %like% "君悦酒店" |
     brand2_qry %like% "和颐" |
     brand2_qry %like% "喜来登" |
     brand2_qry %like% "喜达屋" |
     brand2_qry %like% "嘉里" |
     (brand2_qry %like% "四季" & !grepl("云上四季", brand2_qry))|
     brand2_qry %like% "大仓" |
     brand2_qry %like% "奥克伍德" |
     brand2_qry %like% "威斯汀" |
     brand2_qry %like% "安缦" |
     brand2_qry %like% "安达仕" |
     brand2_qry %like% "安达仕酒店" |
     brand2_qry %like% "宝丽嘉" |
     brand2_qry %like% "富豪" |
     brand2_qry %like% "希尔顿" |
     brand2_qry %like% "希尔顿花园酒店" |
     brand2_qry %like% "帝盛" |
     brand2_qry %like% "康德思" |
     brand2_qry %like% "康铂" |
     brand2_qry %like% "开元" |
     brand2_qry %like% "开元名都" |
     brand2_qry %like% "开元大酒店" |
     brand2_qry %like% "御庭" |
     brand2_qry %like% "悦榕庄" |
     brand2_qry %like% "文华东方" |
     brand2_qry %like% "斯维登" |
     brand2_qry %like% "新世界" |
     brand2_qry %like% "日航" |
     brand2_qry %like% "星河湾" |
     brand2_qry %like% "朗廷" |
     brand2_qry %like% "柏悦酒店" |
     brand2_qry %like% "柏曼" |
     brand2_qry %like% "桔子水晶" |
     brand2_qry %like% "欢墅" |
     brand2_qry %like% "洲际" |
     brand2_qry %like% "温德姆" |
     brand2_qry %like% "温德姆至尊豪廷" |
     brand2_qry %like% "漫心" |
     brand2_qry %like% "瑞吉" |
     brand2_qry %like% "瑞士" |
     brand2_qry %like% "皇冠假日" |
     brand2_qry %like% "盛捷" |
     brand2_qry %like% "睿柏" |
     brand2_qry %like% "福朋" |
     brand2_qry %like% "禧玥" |
     brand2_qry %like% "粤海国际" |
     brand2_qry %like% "素柏" |
     brand2_qry %like% "索菲特" |
     brand2_qry %like% "美利亚" |
     brand2_qry %like% "美居" |
     brand2_qry %like% "美爵" |
     brand2_qry %like% "美豪" |
     brand2_qry %like% "臻品之选" |
     brand2_qry %like% "艾美" |
     brand2_qry %like% "英迪格" |
     brand2_qry %like% "诺富特" |
     brand2_qry %like% "豪华精选" |
     brand2_qry %like% "豪生" |
     brand2_qry %like% "贝尔特" |
     brand2_qry %like% "费尔蒙" |
     brand2_qry %like% "辉盛庭" |
     brand2_qry %like% "辉盛阁" |
     brand2_qry %like% "远洲" |
     brand2_qry %like% "迪士尼" |
     brand2_qry %like% "逸林希尔顿" |
     brand2_qry %like% "金陵" |
     brand2_qry %like% "铂尔曼" |
     (brand2_qry %like% "锦江"  & !grepl("锦江之星", brand2_qry))|
     brand2_qry %like% "锦江都城" |
     brand2_qry %like% "隐居" |
     brand2_qry %like% "雅诗阁" |
     brand2_qry %like% "雅阁" |
     brand2_qry %like% "雅高(Accor)" |
     brand2_qry %like% "首旅" |
     brand2_qry %like% "香格里拉"|
     brand2_qry %like% "万达"|
     brand2_qry %like% "瑰丽(Rose Wood)"|
     brand2_qry %like% "钓鱼台美高梅"|
     brand2_qry %like% "粤海"|
     brand2_qry %like% "长荣(Evergreen)"|
     brand2_qry %like% "萨维尔"|
     brand2_qry %like% "瑞华", brand1 := "High,"]

sc[brand2_qry %like% "全部快捷连锁" |
     brand2_qry %like% "Q+" |
     brand2_qry %like% "如家快捷" |
     brand2_qry %like% "海友" |
     brand2_qry %like% "驿居" |
     brand2_qry %like% "怡莱" |
     brand2_qry %like% "汉庭" |
     brand2_qry %like% "云上四季" |
     brand2_qry %like% "莫泰" |
     brand2_qry %like% "派柏" |
     brand2_qry %like% "如家商旅" |
     (brand2_qry %like% "如家" & !grepl("如家精选", brand2_qry)) |
     brand2_qry %like% "金广快捷" |
     brand2_qry %like% "锦江之星" |
     brand2_qry %like% "百时快捷" |
     brand2_qry %like% "IU酒店" |
     brand2_qry %like% "7天优品" |
     brand2_qry %like% "7天" |
     brand2_qry %like% "戴斯" |
     brand2_qry %like% "派酒店" |
     (brand2_qry %like% "维也纳" & !grepl("维也纳国际", brand2_qry)) |
     brand2_qry %like% "城市便捷" |
     brand2_qry %like% "精途" |
     brand2_qry %like% "易佰" |
     brand2_qry %like% "华驿酒店" |
     brand2_qry %like% "维也纳3好" |
     brand2_qry %like% "布丁" |
     brand2_qry %like% "游多多" |
     brand2_qry %like% "速8" |
     brand2_qry %like% "宜必思" |
     brand2_qry %like% "宜必思尚品" |
     brand2_qry %like% "24K国际连锁" |
     brand2_qry %like% "都市118" |
     brand2_qry %like% "A8连锁" |
     brand2_qry %like% "希岸" |
     brand2_qry %like% "吉泰" |
     brand2_qry %like% "星程" |
     brand2_qry %like% "格林" |
     brand2_qry %like% "喆啡" |
     brand2_qry %like% "华住" |
     brand2_qry %like% "智尚" |
     brand2_qry %like% "浦江之星", brand1 := paste("Budge,", brand1)]

sc[brand2_qry %like% "全部中端连锁" |
        brand2_qry %like% "凤梧" |
        brand2_qry %like% "中油阳光" |
        brand2_qry %like% "格兰云天" |
        brand2_qry %like% "纽宾凯" |
        brand2_qry %like% "都市花园" |
        brand2_qry %like% "尊茂" |
        brand2_qry %like% "衡山" |
        brand2_qry %like% "蓝海大饭店" |
        brand2_qry %like% "骏怡" |
        brand2_qry %like% "南苑e家连锁" |
        brand2_qry %like% "城市之家" |
        brand2_qry %like% "尚客优" |
        brand2_qry %like% "锐思特" |
        brand2_qry %like% "清沐" |
        brand2_qry %like% "99旅馆" |
        brand2_qry %like% "99优选" |
        brand2_qry %like% "智选假日" |
        brand2_qry %like% "安榭" |
        brand2_qry %like% "维也纳国际" |
        brand2_qry %like% "全季" |
        brand2_qry %like% "麗枫" |
        brand2_qry %like% "如家精选" |
        brand2_qry %like% "桔子精选" |
        (brand2_qry %like% "桔子" & !grepl("桔子水晶", brand2_qry)) |
        brand2_qry %like% "源涞" |
        brand2_qry %like% "景悦" |
        brand2_qry %like% "YUNIK" |
        brand2_qry %like% "凯莱"|
        brand2_qry %like% "曼居", brand1 := paste(brand1, "Mid,")]
sc[, brand1 := substr(brand1, 1, nchar(brand1) - 1)]
sc[, brand1 := gsub("Budge", "Budget", brand1)]
sc[brand1 == "Budget,", brand1 := "Budget"]

unique(sc[brand1 == "", brand1_qry])
assign(paste("aa", "brand1", sep = ""), setorder(sc[, .N, by = brand1], -N))
View(aabrand1)

## compare with brand2
aabrand12 <- sc[, .N, by = .(brand1, brand2)]
View(aabrand12)
aaabrand1 <- sc[, .N, by = .(brand1_qry, brand1)][order(-N)]
View(aaabrand1)

## usage 
tmp <- unique(sc[, .(uid, brand1)])
tmp1 <- tmp[brand1 != ""][, .N, by = brand1]
sum(tmp1$N)/404813  # 7.01% (28362)
sum(tmp1$N)


#*-- brand2 --------------------------------------------------------------------
names(sc)
View(abrand2_qry)
sc[, brand2 := ""]
## encode according to list
sc[brand2_qry %like% "24K", brand2 := paste(brand2, "24K,")]
sc[brand2_qry %like% "7天" & 
     !grepl("7天优品", brand2_qry), brand2 := paste(brand2, "7天,")]
sc[brand2_qry %like% "7天优品", brand2 := paste(brand2, "7天优品,")]
sc[brand2_qry %like% "99优选", brand2 := paste(brand2, "99优选,")]
sc[brand2_qry %like% "99旅馆", brand2 := paste(brand2, "99旅馆,")]
sc[brand2_qry %like% "A8连锁", brand2 := paste(brand2, "A8连锁,")]
sc[brand2_qry %like% "CitiGO", brand2 := paste(brand2, "CitiGO,")]
sc[brand2_qry %like% "IU酒店", brand2 := paste(brand2, "IU酒店,")]
sc[brand2_qry %like% "JW万豪", brand2 := paste(brand2, "JW万豪,")]
sc[brand2_qry %like% "Q+", brand2 := paste(brand2, "Q+,")]
sc[brand2_qry %like% "W酒店", brand2 := paste(brand2, "W酒店,")]
sc[brand2_qry %like% "YUNIK", brand2 := paste(brand2, "YUNIK,")]
sc[brand2_qry %like% "万丽", brand2 := paste(brand2, "万丽,")]
sc[brand2_qry %like% "万怡", brand2 := paste(brand2, "万怡,")]
sc[brand2_qry %like% "万枫", brand2 := paste(brand2, "万枫,")]
sc[brand2_qry %like% "万豪", brand2 := paste(brand2, "万豪,")]
sc[brand2_qry %like% "万达瑞华", brand2 := paste(brand2, "万达瑞华,")]
sc[brand2_qry %like% "丽思卡尔顿", brand2 := paste(brand2, "丽思卡尔顿,")]
sc[brand2_qry %like% "丽笙", brand2 := paste(brand2, "丽笙,")]
sc[brand2_qry %like% "中南海滨", brand2 := paste(brand2, "中南海滨,")]
sc[brand2_qry %like% "中油阳光", brand2 := paste(brand2, "中油阳光,")]
sc[brand2_qry %like% "书香世家", brand2 := paste(brand2, "书香世家,")]
sc[brand2_qry %like% "云上四季", brand2 := paste(brand2, "云上四季,")]
sc[brand2_qry %like% "云栖", brand2 := paste(brand2, "云栖,")]
sc[brand2_qry %like% "亚朵", brand2 := paste(brand2, "亚朵,")]
sc[brand2_qry %like% "假日", brand2 := paste(brand2, "假日,")]
sc[brand2_qry %like% "全季", brand2 := paste(brand2, "全季,")]
sc[brand2_qry %like% "全部高端连锁", brand2 := paste(brand2, "全部高端连锁,")]
sc[brand2_qry %like% "全部中端连锁", brand2 := paste(brand2, "全部中端连锁,")]
sc[brand2_qry %like% "全部快捷连锁", brand2 := paste(brand2, "全部快捷连锁,")]
sc[brand2_qry %like% "凤梧", brand2 := paste(brand2, "凤梧,")]
sc[brand2_qry %like% "凯宾斯基", brand2 := paste(brand2, "凯宾斯基,")]
sc[brand2_qry %like% "凯悦" & 
     !grepl("凯悦嘉寓酒店", brand2_qry) & 
     !grepl("凯悦嘉轩酒店", brand2_qry), brand2 := paste(brand2, "凯悦,")]
sc[brand2_qry %like% "凯悦嘉寓酒店", brand2 := paste(brand2, "凯悦嘉寓酒店,")]
sc[brand2_qry %like% "凯悦嘉轩酒店", brand2 := paste(brand2, "凯悦嘉轩酒店,")]
sc[brand2_qry %like% "凯莱", brand2 := paste(brand2, "凯莱,")]
sc[brand2_qry %like% "千禧", brand2 := paste(brand2, "千禧,")]
sc[brand2_qry %like% "半岛", brand2 := paste(brand2, "半岛,")]
sc[brand2_qry %like% "华住", brand2 := paste(brand2, "华住,")]
sc[brand2_qry %like% "华尔道夫", brand2 := paste(brand2, "华尔道夫,")]
sc[brand2_qry %like% "华美达", brand2 := paste(brand2, "华美达,")]
sc[brand2_qry %like% "华驿酒店", brand2 := paste(brand2, "华驿酒店,")]
sc[brand2_qry %like% "卓美亚", brand2 := paste(brand2, "卓美亚,")]
sc[brand2_qry %like% "南苑e家连锁", brand2 := paste(brand2, "南苑e家连锁,")]
sc[brand2_qry %like% "吉泰", brand2 := paste(brand2, "吉泰,")]
sc[brand2_qry %like% "名致", brand2 := paste(brand2, "名致,")]
sc[brand2_qry %like% "君亭", brand2 := paste(brand2, "君亭,")]
sc[brand2_qry %like% "君悦酒店", brand2 := paste(brand2, "君悦酒店,")]
sc[brand2_qry %like% "和颐", brand2 := paste(brand2, "和颐,")]
sc[brand2_qry %like% "喆啡", brand2 := paste(brand2, "喆啡,")]
sc[brand2_qry %like% "喜来登", brand2 := paste(brand2, "喜来登,")]
sc[brand2_qry %like% "喜达屋", brand2 := paste(brand2, "喜达屋,")]
sc[brand2_qry %like% "嘉里", brand2 := paste(brand2, "嘉里,")]
sc[brand2_qry %like% "四季" & 
     !grepl("云上四季", brand2_qry), brand2 := paste(brand2, "四季,")]
sc[brand2_qry %like% "城市之家", brand2 := paste(brand2, "城市之家,")]
sc[brand2_qry %like% "城市便捷", brand2 := paste(brand2, "城市便捷,")]
sc[brand2_qry %like% "大仓", brand2 := paste(brand2, "大仓,")]
sc[brand2_qry %like% "奥克伍德", brand2 := paste(brand2, "奥克伍德,")]
sc[brand2_qry %like% "如家" & 
     !grepl("如家商旅", brand2_qry) & 
     !grepl("如家快捷", brand2_qry) & 
     !grepl("如家精选", brand2_qry), brand2 := paste(brand2, "如家,")]
sc[brand2_qry %like% "如家商旅", brand2 := paste(brand2, "如家商旅,")]
sc[brand2_qry %like% "如家快捷", brand2 := paste(brand2, "如家快捷,")]
sc[brand2_qry %like% "如家精选", brand2 := paste(brand2, "如家精选,")]
sc[brand2_qry %like% "威斯汀", brand2 := paste(brand2, "威斯汀,")]
sc[brand2_qry %like% "安榭", brand2 := paste(brand2, "安榭,")]
sc[brand2_qry %like% "安缦", brand2 := paste(brand2, "安缦,")]
sc[brand2_qry %like% "安达仕", brand2 := paste(brand2, "安达仕,")]
sc[brand2_qry %like% "宜必思", brand2 := paste(brand2, "宜必思,")]
sc[brand2_qry %like% "宝丽嘉", brand2 := paste(brand2, "宝丽嘉,")]
sc[brand2_qry %like% "富豪", brand2 := paste(brand2, "富豪,")]
sc[brand2_qry %like% "尊茂", brand2 := paste(brand2, "尊茂,")]
sc[brand2_qry %like% "尚客优", brand2 := paste(brand2, "尚客优,")]
sc[brand2_qry %like% "布丁", brand2 := paste(brand2, "布丁,")]
sc[brand2_qry %like% "希尔顿" & 
     !grepl("希尔顿花园酒店", brand2_qry), brand2 := paste(brand2, "希尔顿,")]
sc[brand2_qry %like% "希尔顿花园酒店", brand2 := paste(brand2, "希尔顿花园酒店,")]
sc[brand2_qry %like% "希岸", brand2 := paste(brand2, "希岸,")]
sc[brand2_qry %like% "帝盛", brand2 := paste(brand2, "帝盛,")]
sc[brand2_qry %like% "康德思", brand2 := paste(brand2, "康德思,")]
sc[brand2_qry %like% "康铂", brand2 := paste(brand2, "康铂,")]
sc[brand2_qry %like% "开元" & 
     !grepl("开元名都", brand2_qry) & 
     !grepl("开元大酒店", brand2_qry), brand2 := paste(brand2, "开元,")]
sc[brand2_qry %like% "开元名都", brand2 := paste(brand2, "开元名都,")]
sc[brand2_qry %like% "开元大酒店", brand2 := paste(brand2, "开元大酒店,")]
sc[brand2_qry %like% "御庭", brand2 := paste(brand2, "御庭,")]
sc[brand2_qry %like% "怡莱", brand2 := paste(brand2, "怡莱,")]
sc[brand2_qry %like% "悦榕庄", brand2 := paste(brand2, "悦榕庄,")]
sc[brand2_qry %like% "戴斯", brand2 := paste(brand2, "戴斯,")]
sc[brand2_qry %like% "文华东方", brand2 := paste(brand2, "文华东方,")]
sc[brand2_qry %like% "斯维登", brand2 := paste(brand2, "斯维登,")]
sc[brand2_qry %like% "新世界", brand2 := paste(brand2, "新世界,")]
sc[brand2_qry %like% "日航", brand2 := paste(brand2, "日航,")]
sc[brand2_qry %like% "易佰", brand2 := paste(brand2, "易佰,")]
sc[brand2_qry %like% "星河湾", brand2 := paste(brand2, "星河湾,")]
sc[brand2_qry %like% "星程", brand2 := paste(brand2, "星程,")]
sc[brand2_qry %like% "景悦", brand2 := paste(brand2, "景悦,")]
sc[brand2_qry %like% "智选假日", brand2 := paste(brand2, "智选假日,")]
sc[brand2_qry %like% "朗廷", brand2 := paste(brand2, "朗廷,")]
sc[brand2_qry %like% "柏悦酒店", brand2 := paste(brand2, "柏悦酒店,")]
sc[brand2_qry %like% "柏曼", brand2 := paste(brand2, "柏曼,")]
sc[brand2_qry %like% "格兰云天", brand2 := paste(brand2, "格兰云天,")]
sc[brand2_qry %like% "格林豪泰", brand2 := paste(brand2, "格林豪泰,")]
sc[brand2_qry %like% "格林联盟", brand2 := paste(brand2, "格林联盟,")]
sc[brand2_qry %like% "格林东方", brand2 := paste(brand2, "格林东方,")]
sc[brand2_qry %like% "桔子" & 
     !grepl("桔子精选", brand2_qry) & 
     !grepl("桔子水晶", brand2_qry), brand2 := paste(brand2, "桔子,")]
sc[brand2_qry %like% "桔子水晶", brand2 := paste(brand2, "桔子水晶,")]
sc[brand2_qry %like% "桔子精选", brand2 := paste(brand2, "桔子精选,")]
sc[brand2_qry %like% "欢墅", brand2 := paste(brand2, "欢墅,")]
sc[brand2_qry %like% "汉庭", brand2 := paste(brand2, "汉庭,")]
sc[brand2_qry %like% "洲际", brand2 := paste(brand2, "洲际,")]
sc[brand2_qry %like% "派柏", brand2 := paste(brand2, "派柏,")]
sc[brand2_qry %like% "派酒店", brand2 := paste(brand2, "派酒店,")]
sc[brand2_qry %like% "浦江之星", brand2 := paste(brand2, "浦江之星,")]
sc[brand2_qry %like% "海友", brand2 := paste(brand2, "海友,")]
sc[brand2_qry %like% "清沐", brand2 := paste(brand2, "清沐,")]
sc[brand2_qry %like% "温德姆" & 
     !grepl("温德姆至尊豪廷", brand2_qry), brand2 := paste(brand2, "温德姆,")]
sc[brand2_qry %like% "温德姆至尊豪廷", brand2 := paste(brand2, "温德姆至尊豪廷,")]
sc[brand2_qry %like% "游多多", brand2 := paste(brand2, "游多多,")]
sc[brand2_qry %like% "源涞", brand2 := paste(brand2, "源涞,")]
sc[brand2_qry %like% "漫心", brand2 := paste(brand2, "漫心,")]
sc[brand2_qry %like% "瑞吉", brand2 := paste(brand2, "瑞吉,")]
sc[brand2_qry %like% "瑞士", brand2 := paste(brand2, "瑞士,")]
sc[brand2_qry %like% "百时快捷", brand2 := paste(brand2, "百时快捷,")]
sc[brand2_qry %like% "皇冠假日", brand2 := paste(brand2, "皇冠假日,")]
sc[brand2_qry %like% "盛捷", brand2 := paste(brand2, "盛捷,")]
sc[brand2_qry %like% "睿柏", brand2 := paste(brand2, "睿柏,")]
sc[brand2_qry %like% "福朋", brand2 := paste(brand2, "福朋,")]
sc[brand2_qry %like% "禧玥", brand2 := paste(brand2, "禧玥,")]
sc[brand2_qry %like% "粤海国际", brand2 := paste(brand2, "粤海国际,")]
sc[brand2_qry %like% "精途", brand2 := paste(brand2, "精途,")]
sc[brand2_qry %like% "素柏", brand2 := paste(brand2, "素柏,")]
sc[brand2_qry %like% "索菲特", brand2 := paste(brand2, "索菲特,")]
sc[brand2_qry %like% "纽宾凯", brand2 := paste(brand2, "纽宾凯,")]
sc[brand2_qry %like% "维也纳" & 
     !grepl("维也纳3好", brand2_qry) & 
     !grepl("维也纳国际", brand2_qry), brand2 := paste(brand2, "维也纳,")]
sc[brand2_qry %like% "维也纳3好", brand2 := paste(brand2, "维也纳3好,")]
sc[brand2_qry %like% "维也纳国际", brand2 := paste(brand2, "维也纳国际,")]
sc[brand2_qry %like% "美利亚", brand2 := paste(brand2, "美利亚,")]
sc[brand2_qry %like% "美居", brand2 := paste(brand2, "美居,")]
sc[brand2_qry %like% "美爵", brand2 := paste(brand2, "美爵,")]
sc[brand2_qry %like% "美豪", brand2 := paste(brand2, "美豪,")]
sc[brand2_qry %like% "臻品之选", brand2 := paste(brand2, "臻品之选,")]
sc[brand2_qry %like% "艾美", brand2 := paste(brand2, "艾美,")]
sc[brand2_qry %like% "英迪格", brand2 := paste(brand2, "英迪格,")]
sc[brand2_qry %like% "莫泰", brand2 := paste(brand2, "莫泰,")]
sc[brand2_qry %like% "蓝海大饭店", brand2 := paste(brand2, "蓝海大饭店,")]
sc[brand2_qry %like% "衡山", brand2 := paste(brand2, "衡山,")]
sc[brand2_qry %like% "诺富特", brand2 := paste(brand2, "诺富特,")]
sc[brand2_qry %like% "豪华精选", brand2 := paste(brand2, "豪华精选,")]
sc[brand2_qry %like% "豪生", brand2 := paste(brand2, "豪生,")]
sc[brand2_qry %like% "贝尔特", brand2 := paste(brand2, "贝尔特,")]
sc[brand2_qry %like% "费尔蒙", brand2 := paste(brand2, "费尔蒙,")]
sc[brand2_qry %like% "辉盛" & 
     !grepl("辉盛阁", brand2_qry), brand2 := paste(brand2, "辉盛,")]
sc[brand2_qry %like% "辉盛阁", brand2 := paste(brand2, "辉盛阁,")]
sc[brand2_qry %like% "远洲", brand2 := paste(brand2, "远洲,")]
sc[brand2_qry %like% "迪士尼", brand2 := paste(brand2, "迪士尼,")]
sc[brand2_qry %like% "速8", brand2 := paste(brand2, "速8,")]
sc[brand2_qry %like% "逸林希尔顿", brand2 := paste(brand2, "逸林希尔顿,")]
sc[brand2_qry %like% "都市118", brand2 := paste(brand2, "都市118,")]
sc[brand2_qry %like% "都市花园", brand2 := paste(brand2, "都市花园,")]
sc[brand2_qry %like% "金广快捷", brand2 := paste(brand2, "金广快捷,")]
sc[brand2_qry %like% "金陵", brand2 := paste(brand2, "金陵,")]
sc[brand2_qry %like% "铂尔曼", brand2 := paste(brand2, "铂尔曼,")]
sc[brand2_qry %like% "锐思特", brand2 := paste(brand2, "锐思特,")]
sc[brand2_qry %like% "锦江" & 
     !grepl("锦江之星", brand2_qry) & 
     !grepl("锦江都城", brand2_qry), brand2 := paste(brand2, "锦江,")]
sc[brand2_qry %like% "锦江之星", brand2 := paste(brand2, "锦江之星,")]
sc[brand2_qry %like% "锦江都城", brand2 := paste(brand2, "锦江都城,")]
sc[brand2_qry %like% "隐居", brand2 := paste(brand2, "隐居,")]
sc[brand2_qry %like% "雅诗阁", brand2 := paste(brand2, "雅诗阁,")]
sc[brand2_qry %like% "雅阁", brand2 := paste(brand2, "雅阁,")]
sc[brand2_qry %like% "雅高", brand2 := paste(brand2, "雅高,")]
sc[brand2_qry %like% "首旅", brand2 := paste(brand2, "首旅,")]
sc[brand2_qry %like% "香格里拉", brand2 := paste(brand2, "香格里拉,")]
sc[brand2_qry %like% "驿居", brand2 := paste(brand2, "驿居,")]
sc[brand2_qry %like% "骏怡", brand2 := paste(brand2, "骏怡,")]
sc[brand2_qry %like% "麗枫", brand2 := paste(brand2, "麗枫,")]
sc[brand2_qry %like% "智尚", brand2 := paste(brand2, "智尚,")]
sc[brand2_qry %like% "曼居", brand2 := paste(brand2, "曼居,")]
sc[brand2_qry %like% "瑰丽", brand2 := paste(brand2, "瑰丽(Rose Wood),")]
sc[brand2_qry %like% "钓鱼台美高梅", brand2 := paste(brand2, "钓鱼台美高梅")]
sc[brand2_qry %like% "粤海", brand2 := paste(brand2, "粤海,")]
sc[brand2_qry %like% "长荣", brand2 := paste(brand2, "长荣(Evergreen),")]
sc[brand2_qry %like% "萨维尔", brand2 := paste(brand2, "萨维尔,")]
sc[brand2_qry %like% "瑞华", brand2 := paste(brand2, "瑞华,")]
sc[brand2_qry %like% "万达", brand2 := paste(brand2, "万达,")]

## those with only numbers
sc[str_match(brand2_qry, "([0-9]+)")[, 2] != "" & 
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
     !grepl("A8连锁", brand2_qry), brand2 := brand2_qry]  

sc[nchar(brand2) != 1, brand2 := substr(brand2, 1, nchar(brand2)-1)]
assign(paste("aa", "brand2", sep = ""), setorder(sc[, .N, by = brand2], -N))
View(aabrand2)
unique(sc[brand2 == "", brand2_qry])
aaabrand2 <- sc[, .N, by = .(brand2_qry, brand2)]
View(aaabrand2)
## usage 
tmp <- unique(sc[, .(uid, brand2)])
tmp1 <- tmp[brand2 != ""][, .N, by = brand2]
sum(tmp1$N)/404813  # 8.79% (35601)
sum(tmp1$N)


#*-- dist ----------------------------------------------------------------------
names(sc)
View(adist_qry)

sc[, dist := dist_qry]
sc[dist_qry == "-999", dist := ""]
sc[dist_qry == "4公里内", dist := ""]
sc[dist_qry == "4", dist := ""]
sc[dist_qry == "1", dist := "1公里内"]
sc[dist_qry == "2", dist := "2公里内"]
sc[dist_qry == "3", dist := "3公里内"]
sc[dist_qry == "5", dist := "5公里内"]
sc[dist_qry == "6", dist := "6公里内"]
sc[dist_qry %like% "公里内", dist := 1000*as.numeric(str_match(dist_qry,
                                                            "([0-9]+)")[, 2])]
sc[, dist := gsub("米内", "", dist)]
sc[, dist := gsub("米以内", "", dist)]
sc[, dist := gsub("(地图范围)", "", dist)]
sc[, dist := gsub("[/()]", "", dist)]
sc[, dist := gsub("[/（）]", "", dist)]
sc[dist == "0", dist := ""]
assign(paste("aa", "dist", sep = ""), setorder(sc[, .N, by = dist], -N))
View(aadist)
aaadist <- sc[, .N, by = .(dist_qry, dist)]
View(aaadist)

unique(sc[dist == "", dist_qry])
aaadist <- sc[, .N, by = .(dist_qry, dist)][order(-N)]
View(aaadist)
## usage 
tmp <- unique(sc[, .(uid, dist)])
tmp1 <- tmp[dist != ""][, .N, by = dist]
sum(tmp1$N)/404813  # 15.01% (60780)
sum(tmp1$N)


#-- replace from dt
aa <- unique(dt[, .(qid_new, dist_qry)])[, .N, by = dist_qry]
sum(aa[, N])
aaa <- sc[, .N, by = dist_qry]
sum(aaa[, N])
View(aaa)

aa <- unique(dt[, .(qid_new, dist_qry)])
setkey(sc, qid_new)
sc <- sc[aa]
names(sc)
sc[, dist_qry := i.dist_qry]
sc[, i.dist_qry := NULL]

#*-- filter_score --------------------------------------------------------------
names(sc)
View(afilter_score)
sc[, filterscore := filter_score]
sc[filter_score == "-999", filterscore := ""]
sc[filter_score == "3", filterscore := "3.0分以上"]
sc[filter_score == "4", filterscore := "4.0分以上"]
sc[filter_score == "4.5分以上4.0分以上", filter_score := "4.5分以上"]
sc[, filterscore := gsub(" ", "", filterscore)]
sc[filterscore %like% "分以上", filterscore := paste("above", filterscore)]
sc[, filterscore := gsub("分以上", "", filterscore)]
assign(paste("aa", "filterscore", sep = ""), setorder(sc[, .N, by = filterscore], -N))
View(aafilterscore)
aaafilterscore <- sc[, .N, by = .(filterscore, filter_score)]
View(aaafilterscore)

unique(sc[filterscore == "", filter_score])
aaafilterscore <- sc[, .N, by = .(filter_score, filterscore)][order(-N)]
View(aaafilterscore)
## usage 
tmp <- unique(sc[, .(uid, filterscore)])
tmp1 <- tmp[filterscore != ""][, .N, by = filterscore]
sum(tmp1$N)/404813  # 6.40% (259043)
sum(tmp1$N)

#*-- com -----------------------------------------------------------------------
names(sc)
View(acom)
sc[, com2 := ""]
sc[com %like% "八万人体育场地区", com2 := "八万人体育场地区,"]
sc[com %like% "北外滩地区", com2 := paste(com2, "北外滩地区,")]
sc[com %like% "曹杨、真如地区", com2 := paste(com2, "曹杨、真如地区,")]
sc[com %like% "漕河泾开发区", com2 := paste(com2, "漕河泾开发区,")]
sc[com %like% "崇明、长兴岛、横沙岛", com2 := paste(com2, "崇明、长兴岛、横沙岛,")]
sc[com %like% "打浦桥地区", com2 := paste(com2, "打浦桥地区,")]
sc[com %like% "大宁国际商业区", com2 := paste(com2, "大宁国际商业区,")]
sc[com %like% "滴水湖临港地区", com2 := paste(com2, "滴水湖临港地区,")]
sc[com %like% "迪士尼度假区" |
     com %like% "迪士尼", com2 := paste(com2, "迪士尼度假区,")]
sc[com %like% "长风公园地区", com2 := paste(com2, "长风公园地区,")]
sc[com %like% "长寿路商业区", com2 := paste(com2, "长寿路商业区,")]
sc[com %like% "奉贤开发区", com2 := paste(com2, "奉贤开发区,")]
sc[com %like% "顾村公园地区", com2 := paste(com2, "顾村公园地区,")]
sc[com %like% "光大会展中心", com2 := paste(com2, "光大会展中心,")]
sc[com %like% "国家会展中心", com2 := paste(com2, "国家会展中心,")]
sc[com %like% "衡山路地区", com2 := paste(com2, "衡山路地区,")]
sc[com %like% "虹口足球场地区", com2 := paste(com2, "虹口足球场地区,")]
sc[com %like% "虹桥地区" |
     com %like% "虹桥", com2 := paste(com2, "虹桥地区,")]
sc[com %like% "虹桥枢纽地区", com2 := paste(com2, "虹桥枢纽地区,")]
sc[com %like% "虹桥枢纽周边区", com2 := paste(com2, "虹桥枢纽周边区,")]
sc[com %like% "淮海路商业区", com2 := paste(com2, "淮海路商业区,")]
sc[com %like% "江桥工业区", com2 := paste(com2, "江桥工业区,")]
sc[com %like% "江湾、五角场商业区", com2 := paste(com2, "江湾、五角场商业区,")]
sc[com %like% "交大、闵行经济开发区", com2 := paste(com2, "交大、闵行经济开发区,")]
sc[com %like% "金山城市沙滩地区", com2 := paste(com2, "金山城市沙滩地区,")]
sc[com %like% "金山枫泾古镇地区", com2 := paste(com2, "金山枫泾古镇地区,")]
sc[com %like% "静安寺地区" |
     com %like% "静安寺区域" |
     hotd == "静安寺", com2 := paste(com2, "静安寺地区,")]
sc[com %like% "马陆、嘉定城区", com2 := paste(com2, "马陆、嘉定城区,")]
sc[com %like% "南汇及野生动物园", com2 := paste(com2, "南汇及野生动物园,")]
sc[com %like% "南京西路商业区", com2 := paste(com2, "南京西路商业区,")]
sc[com %like% "南外滩地区", com2 := paste(com2, "南外滩地区,")]
sc[com %like% "南翔、安亭汽车城", com2 := paste(com2, "南翔、安亭汽车城,")]
sc[com %like% "彭浦地区", com2 := paste(com2, "彭浦地区,")]
sc[com %like% "浦东机场核心区" |
     com %like% "浦东机场核心", com2 := paste(com2, "浦东机场核心区,")]
sc[com %like% "浦东金桥地区", com2 := paste(com2, "浦东金桥地区,")]
sc[com %like% "浦东陆家嘴金融贸易区" |
     com %like% "陆家嘴" |
     hotd == "陆家嘴", com2 := paste(com2, "浦东陆家嘴金融贸易区,")]
sc[com %like% "浦东世博园区", com2 := paste(com2, "浦东世博园区,")]
sc[com %like% "浦东外高桥地区", com2 := paste(com2, "浦东外高桥地区,")]
sc[com %like% "浦东新国际博览中心" |
     com %like% "新国际博览中心", com2 := paste(com2, "浦东新国际博览中心,")]
sc[com %like% "浦东张江地区", com2 := paste(com2, "浦东张江地区,")]
sc[com %like% "浦江镇地区", com2 := paste(com2, "浦江镇地区,")]
sc[com %like% "七宝古镇", com2 := paste(com2, "七宝古镇,")]
sc[com %like% "七浦路商业区", com2 := paste(com2, "七浦路商业区,")]
sc[com %like% "青浦城区", com2 := paste(com2, "青浦城区,")]
sc[com %like% "人民广场地区" |
     com %like% "人民广场", com2 := paste(com2, "人民广场地区,")]
sc[com %like% "上海火车南站地区", com2 := paste(com2, "上海火车南站地区,")]
sc[com %like% "上海火车站地区", com2 := paste(com2, "上海火车站地区,")]
sc[com %like% "佘山、松江大学城", com2 := paste(com2, "佘山、松江大学城,")]
sc[com %like% "莘庄工业区", com2 := paste(com2, "莘庄工业区,")]
sc[com %like% "四川北路商业区", com2 := paste(com2, "四川北路商业区,")]
sc[com %like% "外滩地区" | 
     com %like% "外滩", com2 := paste(com2, "外滩地区,")]
sc[com %like% "吴淞口国际邮轮港", com2 := paste(com2, "吴淞口国际邮轮港,")]
sc[com %like% "新场古镇", com2 := paste(com2, "新场古镇,")]
sc[com %like% "新天地地区", com2 := paste(com2, "新天地地区,")]
sc[com %like% "徐家汇地区", com2 := paste(com2, "徐家汇地区,")]
sc[com %like% "豫园地区", com2 := paste(com2, "豫园地区,")]
sc[com %like% "中山公园商业区", com2 := paste(com2, "中山公园商业区,")]
sc[com %like% "周浦康桥地区", com2 := paste(com2, "周浦康桥地区,")]
sc[com %like% "朱家角、东方绿舟", com2 := paste(com2, "朱家角、东方绿舟,")]
sc[com2 == "-999", com2 := ""]
sc[str_match(com, "([0-9]+)")[, 2] != "" & 
     com != "-999", com2 := com] 
sc[nchar(com2) != 1, com2 := substr(com2, 1, nchar(com2) - 1)]

assign(paste("aa", "com2", sep = ""), setorder(sc[, .N, by = com2], -N))
View(aacom2)

unique(sc[com2 == "", com])
aaacom2 <- sc[, .N, by = .(com, com2)][order(-N)]
View(aaacom2)
## usage 
tmp <- unique(sc[, .(uid, com2)])
tmp1 <- tmp[com2 != ""][, .N, by = com2]
sum(tmp1$N)/404813  # 15.48% (62661)
sum(tmp1$N)

#*-- fac -----------------------------------------------------------------------
names(sc)
View(afac_qry)
sc[, fac := ""]
sc[fac_qry %like% "免费wifi上网" |
     fac_qry %like% "免费WiFi上网", fac := paste(fac, "free wifi,")]
sc[fac_qry %like% "免费有线宽带", fac := paste(fac, "free Internet,")]
sc[fac_qry %like% "免费停车", fac := paste(fac, "free parking,")]
sc[fac_qry %like% "停车场", fac := paste(fac, "parking,")]
sc[fac_qry %like% "代客泊车", fac := paste(fac, "valet parking,")]
sc[fac_qry %like% "接送服务", fac := paste(fac, "airport picking-up service,")]
sc[fac_qry %like% "新开业/新装修", fac := paste(fac, "newly open,")]
sc[fac_qry %like% "穿梭机场班车", fac := paste(fac, "shuttle,")]
sc[fac_qry %like% "增值税专用发票", fac := paste(fac, "value-added tax invoice,")]
sc[fac_qry %like% "游泳池", fac := paste(fac, "swimming pool,")]
sc[fac_qry %like% "健身房", fac := paste(fac, "gym,")]
sc[fac_qry %like% "SPA", fac := paste(fac, "spa,")]
sc[fac_qry %like% "酒吧", fac := paste(fac, "bar,")]
sc[fac_qry %like% "餐厅", fac := paste(fac, "restaurants,")]
sc[fac_qry %like% "棋牌室", fac := paste(fac, "cards room,")]
sc[fac_qry %like% "高尔夫球场", fac := paste(fac, "golf course,")]
sc[fac_qry %like% "会议设施", fac := paste(fac, "meeting room,")]
sc[fac_qry %like% "商务中心", fac := paste(fac, "business center,")]
sc[fac_qry %like% "行李寄存", fac := paste(fac, "luggage storage,")]
sc[fac_qry %like% "叫醒服务", fac := paste(fac, "wake-up servece,")]
sc[fac_qry %like% "吸烟区", fac := paste(fac, "smoking area,")]
sc[fac_qry %like% "洗衣服务", fac := paste(fac, "laundry service,")]
sc[fac_qry %like% "允许携带宠物", fac := paste(fac, "pet allowed,")]
sc[fac_qry %like% "洗衣机", fac := paste(fac, "washing machine,")]
sc[fac_qry %like% "厨房", fac := paste(fac, "kitchen,")]
sc[fac_qry %like% "携程行李寄送", fac := paste(fac, "luggage delivery,")]
sc[, fac := substr(fac, 1, nchar(fac) - 1)]

assign(paste("aa", "fac", sep = ""), setorder(sc[, .N, by = fac], -N))
View(aafac)

unique(sc[fac == "", fac_qry])
aaafac <- sc[, .N, by = .(fac_qry, fac)][order(-N)]
View(aaafac)


#*-- filter_quantity -----------------------------------------------------------
names(sc)
View(afilter_quantity)
sc[, filterquantity := ""]
sc[filter_quantity == "50条以上" |
     filter_quantity == "50", filterquantity := "more than 50 reviews"]
sc[filter_quantity == "20条以上", filterquantity := "more than 20 reviews"]
sc[filter_quantity == "10条以上", filterquantity := "more than 10 reviews"]
sc[filter_quantity == "5条以上", filterquantity := "more than 5 reviews"]

unique(sc[sta2 == "", sta])
assign(paste("aa", "sta2", sep = ""), setorder(sc[, .N, by = sta2], -N))
View(aasta2)
aaasta2 <- sc[, .N, by = .(sta, sta2)]
View(aaasta2)
## usage 
tmp <- unique(sc[, .(uid, sta2)])
tmp1 <- tmp[sta2 != ""][, .N, by = sta2]
sum(tmp1$N)/404813  # 0.03% (129)
sum(tmp1$N)

#*-- sta2 ----------------------------------------------------------------------
names(sc)
View(asta)
sc[, sta2 := ""]
sc[sta == "1274799" | 
     sta == "上海浦东国际机场" |
     hotd == "上海浦东国际机场", sta2 := "PVG Airport"]
sc[sta == "11274819" | 
     sta == "上海虹桥国际机场" |
     hotd == "上海虹桥国际机场", sta2 := "SHA Airport"]
sc[sta == "1646717" | 
     sta == "虹桥火车站" |
     hotd == "虹桥火车站", sta2 := "HQ Train Station"]
sc[sta == "11974396" | 
     sta == "上海火车站" |
     hotd == "上海火车站", sta2 := "SH Train Station"]
sc[sta == "2467534" | 
     sta == "上海南站" |
     hotd == "上海南站", sta2 := "SH South Train Station"]
sc[sta == "2467535" | sta == "上海站北广场", sta2 := "SH Train Station SquareN"]
sc[sta == "上海西火车站", sta2 := "SH West Train Station"]
sc[sta == "松江南火车站", sta2 := "SJ South Train Station"]
sc[sta == "松江站", sta2 := "SJ Train Station"]
sc[sta == "金山北火车站", sta2 := "JS North Train Station"]
sc[sta == "上海长途客运南站", sta2 := "Bus Station South"]
sc[sta == "上海长途汽车客运总站", sta2 := "Bus Station SH"]
sc[sta == "上海长途汽车站", sta2 := "Bus Station Intercity"]
sc[sta == "青浦客运站", sta2 := "Bus Station QP"]
sc[sta == "上海虹桥客运西站", sta2 := "Bus Station HQ West"]
sc[sta == "上海南浦大桥长途旅游客运站", sta2 := "Bus Station NP"]
sc[sta == "上海长途客运东站", sta2 := "Bus Station Ease"]
sc[sta == "上海中山交通客运有限公司", sta2 := "Bus Station ZS"]
sc[sta == "吴淞长途汽车站", sta2 := "Bus Station WS"]
sc[sta == "上海长途客运北站", sta2 := "Bus Station Intercity North"]
sc[sta == "上海交投沪太路综合客运交通枢纽站", sta2 := "Bus Station HT"]
sc[sta == "巴士南汇长途总站", sta2 := "Bus Station NH"]
sc[sta == "上海旅游集散中心总站", sta2 := "Bus Station Travel"]
sc[sta == "太平洋长途汽车站", sta2 := "Bus Station TPY"]
sc[sta == "沪铁长途客运站", sta2 := "Bus Station HT"]
sc[sta == "中环共和新路综合客运交通枢纽", sta2 := "Bus Station GH"]
sc[sta == "高桥长途客运站", sta2 := "Bus Station GQ"]
sc[sta == "上海浦东国际机场长途客运站", sta2 := "Bus Station PVG"]
sc[sta == "北区汽车站", sta2 := "Bus Station North"]

unique(sc[sta2 == "", sta])
assign(paste("aa", "sta2", sep = ""), setorder(sc[, .N, by = sta2], -N))
View(aasta2)
aaasta2 <- sc[, .N, by = .(sta, sta2)]
View(aaasta2)
## usage 
tmp <- unique(sc[, .(uid, sta2)])
tmp1 <- tmp[sta2 != ""][, .N, by = sta2]
sum(tmp1$N)/404813  # 4.84% (19607)
sum(tmp1$N)

#*-- sta3 ----------------------------------------------------------------------
names(sc)
View(asta)
sc[, sta3 := ""]
sc[sta2 %like% "Airport", sta3 := "Airport"]
sc[sta2 %like% "Train Station", sta3 := "Train Station"]
sc[sta2 %like% "Bus Station", sta3 := "Bus Station"]

unique(sc[sta3 == "", sta2])
assign(paste("aa", "sta3", sep = ""), setorder(sc[, .N, by = sta3], -N))
View(aasta3)
aaasta3 <- sc[, .N, by = .(sta, sta2, sta3)]
View(aaasta3)
## usage 
tmp <- unique(sc[, .(uid, sta3)])
tmp1 <- tmp[sta3 != ""][, .N, by = sta3]
sum(tmp1$N)/404813  # 0.03% (129)
sum(tmp1$N)


#*-- distr ---------------------------------------------------------------------
names(sc)
View(adistr)
sc[, distr2 := ""]
sc[distr %like% "静安区" | distr == "112", distr2 := "Jingan,"]
sc[distr %like% "徐汇区" | distr == "113", distr2 := paste(distr2, "Xuhui,")]
sc[distr %like% "长宁区" | distr == "114", distr2 := paste(distr2, "Changning,")]
sc[distr %like% "黄浦区" | distr == "115", distr2 := paste(distr2, "Huangpu,")]
sc[distr %like% "虹口区" | distr == "116", distr2 := paste(distr2, "Hongkou,")]
sc[distr %like% "宝山区" | distr == "117", distr2 := paste(distr2, "Baoshan,")]
sc[distr %like% "浦东新区" | distr == "119", distr2 := paste(distr2, "Pudong,")]
sc[distr %like% "普陀区" | distr == "120", distr2 := paste(distr2, "Putuo,")]
sc[distr %like% "杨浦区" | distr == "121", distr2 := paste(distr2, "Yangpu,")]
sc[distr %like% "闵行区" | distr == "122", distr2 := paste(distr2, "Minhang,")]
sc[distr %like% "嘉定区" | distr == "123", distr2 := paste(distr2, "Jiading,")]
sc[distr %like% "松江区" | distr == "125", distr2 := paste(distr2, "Songjiang,")]
sc[distr %like% "金山区" | distr == "126", distr2 := paste(distr2, "Jinshan,")]
sc[distr %like% "青浦区" | distr == "128", distr2 := paste(distr2, "Qingpu,")]
sc[distr %like% "奉贤区" | distr == "133", distr2 := paste(distr2, "Fengxian,")]
sc[distr %like% "崇明区" | distr == "134", distr2 := paste(distr2, "Chongming,")]
sc[nchar(distr2) != 1, distr2 := substr(distr2, 1, nchar(distr2) - 1)]
unique(sc[distr2 == "", distr])
assign(paste("aa", "distr2", sep = ""), setorder(sc[, .N, by = distr2], -N))
View(aadistr2)
aaadistr2 <- sc[, .N, by = .(distr, distr2)]
View(aaadistr2)
## usage 
tmp <- unique(sc[, .(uid, distr2)])
tmp1 <- tmp[distr2 != ""][, .N, by = distr2]
sum(tmp1$N)/404813  # 9.84% (39819)
sum(tmp1$N)


#*-- metro1 --------------------------------------------------------------------
names(sc)
View(ametro1)
## extract info from metro2
sc[, metro11 := str_match(metro2, 
  "((\\()([0-9]*)(号线)*(磁悬浮)*(\\)))(.)*((\\()([0-9]*)(号线)*(磁悬浮)*(\\)))*")[, 2]]
sc[, metro11 := gsub("\\(", "", metro11)]
sc[, metro11 := gsub("\\)", "", metro11)]
sc$metro11[is.na(sc$metro11)]   <- "" 
## translate from metro1
sc[metro1 == "2号线" | metro11 == "2号线" | metro1 == "2", metro11 := "Line 2"]
sc[metro1 == "11号线" | metro11 == "11号线", metro11 := "Line 11"]
sc[metro1 == "1号线" | metro11 == "1号线" | metro1 == "1", metro11 := "Line 1"]
sc[metro1 == "10号线" | metro11 == "10号线" | metro1 == "10", metro11 := "Line 10"]
sc[metro1 == "9号线" | metro11 == "9号线", metro11 := "Line 9"]
sc[metro1 == "7号线" | metro11 == "7号线" | metro1 == "7", metro11 := "Line 7"]
sc[metro1 == "8号线" | metro11 == "8号线", metro11 := "Line 8"]
sc[metro1 == "4号线" | metro11 == "4号线", metro11 := "Line 4"]
sc[metro1 == "3号线" | metro11 == "3号线", metro11 := "Line 3"]
sc[metro1 == "13号线" | metro11 == "13号线" | metro1 == "13", metro11 := "Line 13"]
sc[metro1 == "12号线" | metro11 == "12号线", metro11 := "Line 12"]
sc[metro1 == "6号线" | metro11 == "6号线" | metro1 == "6", metro11 := "Line 6"]
sc[metro1 == "17号线" | metro11 == "17号线" | metro1 == "17", metro11 := "Line 17"]
sc[metro1 == "16号线" | metro11 == "16号线", metro11 := "Line 16"]
sc[metro1 == "5号线" | metro11 == "5号线" | metro1 == "5", metro11 := "Line 5"]
sc[metro1 == "磁悬浮" | metro11 == "磁悬浮", metro11 := "Line Maglev"]
sc[metro1 == "浦江线" | metro11 == "浦江线", metro11 := "Line Pujiang"]

## fix metro1
sc <- sc[order(metro2, -metro11)]
for (i in 1743680:nrow(sc)){
  print(i)
  if (sc$metro2[i] == sc$metro2[i-1] & 
      sc$metro11[i-1] != "" & 
      sc$metro11[i] == "") sc$metro11[i] <- sc$metro11[i-1] 
}

unique(sc[metro11 == "", metro1])
assign(paste("aa", "metro11", sep = ""), setorder(sc[, .N, by = metro11], -N))
View(aametro11)
aaametro11 <- sc[, .N, by = .(metro2, metro1, metro11)][order(metro2, -metro11)]
View(aaametro11)

## usage 
tmp <- unique(sc[, .(uid, metro11)])
tmp1 <- tmp[metro11 != ""][, .N, by = metro11]
sum(tmp1$N)/404813  # 6.13 % (24821)
sum(tmp1$N)


#*-- metro2 --------------------------------------------------------------------
names(sc)
View(ametro2)
sc[, metro22 := metro2]
sc[metro1 == "-999", metro22 := ""]
sc[metro2 == "-999", metro22 := ""]

unique(sc[metro22 == "", metro2])
assign(paste("aa", "metro22", sep = ""), setorder(sc[, .N, by = metro22], -N))
View(aametro22)
aaametro22 <- sc[, .N, by = .(metro1, metro2, metro22)][order(-N)]
View(aaametro22)
## usage 
tmp <- unique(sc[, .(uid, metro22)])
tmp1 <- tmp[metro22 != ""][, .N, by = metro22]
sum(tmp1$N)/404813  # 6.53% (26432)
sum(tmp1$N)


#*-- hotd1, hot2 ---------------------------------------------------------------
names(sc)
View(ahotd)
sc[, hotd2 := ""]
sc[hotd == "景点", hotd2 := "scenic spot"]
sc[hotd == "会展中心", hotd2 := "convention center"]
sc[hotd == "大学周边", hotd2 := "around university"]
sc[hotd == "景区-游园", hotd2 := "scenic area"]
sc[hotd == "医院周边", hotd2 := "around hospital"]

sc[, hotd1 := hotd]
sc[hotd == "-999", hotd1 := ""]
sc[hotd == "nolimit", hotd1 := ""]
sc[hotd == "虹桥火车站", hotd1 := ""]
sc[hotd == "上海浦东国际机场", hotd1 := ""]
sc[hotd == "上海火车站", hotd1 := ""]
sc[hotd == "上海虹桥国际机场", hotd1 := ""]
sc[hotd == "上海南站", hotd1 := ""]
sc[hotd == "陆家嘴", hotd1 := ""]
sc[hotd == "静安寺", hotd1 := ""]
sc[hotd == "景点", hotd1 := ""]
sc[hotd == "会展中心", hotd1 := ""]
sc[hotd == "大学周边", hotd1 := ""]
sc[hotd == "景区-游园", hotd1 := ""]
sc[hotd == "医院周边", hotd1 := ""]
sc[hotd == "武汉站", hotd1 := ""]

sc[hotd == "七宝", hotd1 := "Qibao"]
sc[hotd == "打浦桥", hotd1 := "Dapuqiao"]
sc[hotd == "松江大学城", hotd1 := "Songjiang University Area"]
sc[hotd == "上海迪士尼度假区", hotd1 := "Disneyland"]
sc[hotd == "外滩", hotd1 := "Bund"]
sc[hotd == "上海儿童医学中心", hotd1 := "Children's Medical Center"]
sc[hotd == "南京路步行街", hotd1 := "Nanjing Road"]
sc[hotd == "上海世博展览馆", hotd1 := "World Expo Center"]
sc[hotd == "新天地", hotd1 := "Xintiandi"]
sc[hotd == "上海新国际博览中心", hotd1 := "New International Expo Center"]
sc[hotd == "人民广场", hotd1 := "People's Square"]
sc[hotd == "中山公园", hotd1 := "Zhongshan Park"]
sc[hotd == "上海科技馆", hotd1 := "Science and Tech Museum"]
sc[hotd == "上海城隍庙", hotd1 := "City God Temple"]


unique(sc[hotd1 == "", hotd])
assign(paste("aa", "hotd1", sep = ""), setorder(sc[, .N, by = hotd1], -N))
View(aahotd1)
aaahotd1 <- sc[, .N, by = .(hotd, hotd1)]
View(aaahotd1)
## usage 
tmp <- unique(sc[, .(uid, hotd1)])
tmp1 <- tmp[hotd1 != ""][, .N, by = hotd1]
sum(tmp1$N)/404813  # 0.09% (356)
sum(tmp1$N)

unique(sc[hotd2 == "", hotd])
assign(paste("aa", "hotd2", sep = ""), setorder(sc[, .N, by = hotd2], -N))
View(aahotd2)
aaahotd2 <- sc[, .N, by = .(hotd, hotd2)]
View(aaahotd2)
## usage 
tmp <- unique(sc[, .(uid, hotd2)])
tmp1 <- tmp[hotd2 != ""][, .N, by = hotd2]
sum(tmp1$N)/404813  # 0.02% (73)
sum(tmp1$N)


#*-- sort_qry ------------------------------------------------------------------
names(sc)
View(asort_qry)
sc[, sort := sort_qry]
sc[sort_qry == "0", sort := ""]
sc[sort_qry == "6", sort := ""]
sc$sort[is.na(sc$sort)] <- ""

unique(sc[sort == "", sort_qry])
assign(paste("aa", "sort", sep = ""), setorder(sc[, .N, by = sort], -N))
View(aasort)
aaasort <- sc[, .N, by = .(sort_qry, sort)]
View(aaasort)
## usage 
tmp <- unique(sc[, .(uid, sort)])
tmp1 <- tmp[sort != ""][, .N, by = sort]
sum(tmp1$N)/404813  # 54.90% (222237)
sum(tmp1$N)

#*-- bedtype -------------------------------------------------------------------
names(sc)
View(abedtype)
sc[, bedtype2 := ""]
sc[bedtype == "双床", bedtype2 := "2"]
sc[bedtype == "多张床", bedtype2 := "3"]
sc[bedtype == "大床", bedtype2 := "1"]
sc[bedtype == "三人/家庭", bedtype2 := "3"]
sc[bedtype == "单人床", bedtype2 := "1 "]
sc[bedtype == "三人/家庭房", bedtype2 := "3"]
sc[bedtype == "2", bedtype2 := "2"]
sc[bedtype == "3", bedtype2 := "3"]
sc[bedtype == "1", bedtype2 := "1"]
sc[bedtype == "多张床三人/家庭", bedtype2 := "3"]
sc[bedtype == "大床双床", bedtype2 := "2"]
sc[bedtype == "大床三人/家庭", bedtype2 := "3"]

unique(sc[bedtype2 == "", bedtype])
assign(paste("aa", "bedtype2", sep = ""), setorder(sc[, .N, by = bedtype2], -N))
View(aabedtype2)
aaabedtype2 <- sc[, .N, by = .(bedtype, bedtype2)]
View(aaabedtype2)
## usage 
tmp <- unique(sc[, .(uid, bedtype2)])
tmp1 <- tmp[bedtype2 != ""][, .N, by = bedtype2]
sum(tmp1$N)/404813  # 1.93% (7804)
sum(tmp1$N)


#*-- breakfast -----------------------------------------------------------------
names(sc)
View(abreakfast)
sc[, breakfast2 := ""]
sc[breakfast == "含早餐", breakfast2 := "breakfast provied"]
sc[breakfast == "双份早餐", breakfast2 := "2"]
sc[breakfast == "含早", breakfast2 := "breakfast provided"]
sc[breakfast == "单份早餐", breakfast2 := "1"]
sc[breakfast == "1", breakfast2 := "1"]
sc[breakfast == "3", breakfast2 := "3"]
sc[breakfast == "双早", breakfast2 := "2"]
sc[breakfast == "含早餐双份早餐", breakfast2 := "2"]
sc[breakfast == "含早餐单份早餐", breakfast2 := "1"]
sc[breakfast == "2", breakfast2 := "2"]

unique(sc[breakfast2 == "", breakfast])
assign(paste("aa", "breakfast2", sep = ""), setorder(sc[, .N, by = breakfast2], -N))
View(aabreakfast2)
aaabreakfast2 <- sc[, .N, by = .(breakfast, breakfast2)]
View(aaabreakfast2)
## usage 
tmp <- unique(sc[, .(uid, breakfast2)])
tmp1 <- tmp[breakfast2 != ""][, .N, by = breakfast2]
sum(tmp1$N)/404813  # 2.20% (8897)
sum(tmp1$N)


#*-- paytype -------------------------------------------------------------------
names(sc)
View(apaytype)
sc[, paytype2 := ""]
sc[paytype == "到店付款", paytype2 := "postpay"]
sc[paytype == "在线付款", paytype2 := "prepay"]
sc[paytype == "闪住", paytype2 := "postpay_noDeposit"]

unique(sc[paytype2 == "", paytype])
assign(paste("aa", "paytype2", sep = ""), setorder(sc[, .N, by = paytype2], -N))
View(aapaytype2)
aaapaytype2 <- sc[, .N, by = .(paytype, paytype2)]
View(aaapaytype2)
## usage 
tmp <- unique(sc[, .(uid, paytype2)])
tmp1 <- tmp[paytype2 != ""][, .N, by = paytype2]
sum(tmp1$N)/404813  # 0.32% (1283)
sum(tmp1$N)


#*-- os ------------------------------------------------------------------------
names(sc)
View(aos)
sc[, os2 := ""]
sc[os == "e84e30b9390cdb64db6db2c9ab87846d", os2 := "Android"]
sc[os == "1bdf605991920db11cbdf8508204c4eb", os2 := "iOS"]

assign(paste("aa", "os2", sep = ""), setorder(sc[, .N, by = os2], -N))
View(aaos2)


#*-- NULL to blanks ------------------------------------------------------------
sc$dist[is.na(sc$dist)] <- ""
sc$sort[is.na(sc$sort)] <- ""
sc$bedtype2[is.na(sc$bedtype2)] <- ""
sc$price_min[is.na(sc$price_min)] <- ""
sc$price_max[is.na(sc$price_max)] <- ""
sc$price_range[is.na(sc$price_range)] <- ""



## create variables ============================================================

#*-- starttime, rank -----------------------------------------------------------
## prepare starttime
startt <- unique(dt[, .(qid_new, starttime)])
startt[, rank := frank(starttime), by = qid_new]
setkey(startt, qid_new)
startt2 <- startt[rank == 1]

## match
setkey(sc, qid_new)
sc <- sc[startt2]
names(sc)

## rank
sc[, frank := frank(starttime), by = list(uid)]
sc[, query_order := rank]
sc[, rank := NULL]

#*-- click and book ------------------------------------------------------------
## click and book
clickbook <- htl[, .(click = sum(click_bool), 
                     book = sum(booking_bool)), by = .(uid, qid_new)]
clickbook[, .N, by = .(click, book)]  # check
setkey(clickbook, qid_new)
sc <- sc[clickbook]
sc[, i.uid := NULL]

## click rate and conversion rate
uclickbook <- clickbook[, .(click = sum(click), book = sum(book)), by = uid]
ctr <- uclickbook[, .N, by = click]
cr <- uclickbook[, .N, by = book]
(sum(ctr$N) - ctr[1, 2])/sum(ctr$N)  # 0.3983098
(sum(cr$N) - cr[1, 2])/sum(cr$N)  # 0.04

## ifclick and ifbook
sc[, ifclick := sapply(click, function(x) if (x > 0) 1 else 0)]
sc[, ifbook := sapply(book, function(x) if (x > 0) 1 else 0)]
sc[click > 1 & book > 0][1:100, 54:57]  # check
names(sc)

#*-- checkin, checkout, advance days -------------------------------------------
sc[, checkin:= lubridate::ymd(checkin)]
sc[, checkout:= lubridate::ymd(checkout)]
## check
str(sc[, .(checkin, checkout)])
## advancedays
sc[, advancedays := as.numeric(difftime(checkin, date(starttime), units = "days"))]
## check
sc[1:100, .(starttime, checkin, advancedays)]

#*-- price ---------------------------------------------------------------------
# Hypothesis: people with proper options(e.g. 2 not 6) has highest cr

## price_num
sc[, price_num := stringr::str_count(price, "¥")]

## split price options
sc[, price1 := stringr::str_match_all(price, "([0-9]+)")]
sc[, price2 := lapply(price1, as.numeric)]

sc[, price_min := lapply(price2, min)]
sc[price == "", price_min := ""]
sc[, price_min := lapply(price_min, as.numeric)]
sc[price %like% "Below" , price_min := 0]
#sc[, price_min := as.numeric(price_min)]

sc[, price_max := lapply(price2, max)]
sc[price == "", price_max := ""]
sc[, price_max := lapply(price_max, as.numeric)]
sc[price %like% "Above", price_max := as.numeric(price_max) + 500]
#sc[, price_max := as.numeric(price_max)]

## price_range, price_mean
sc[, price_range := as.numeric(price_max) - as.numeric(price_min)]
sc$price_range[is.na(sc$price_range)] <- ""
sc$price_min[is.na(sc$price_min)] <- ""
sc$price_max[is.na(sc$price_max)] <- ""
sc[, price1 := NULL]
sc[, price2 := NULL]

## price_mean
sc[, price_mean := (as.numeric(price_max) + as.numeric(price_min)) / 2]
sc$price_mean[is.na(sc$price_mean)] <- ""

## check
sc[1:10, .(price, price_min)]
sc[1:100, .(price, price_min, price_max, price_num, price_mean, price_range)]
View(bprice_num)



#*-- star ----------------------------------------------------------------------
names(sc)
sc[, star_num := (stringr::str_count(star, ",") + 1)]
sc[star == "", star_num := 0]

bstar <- sc[, .N, by = .(star, star_num)][order(-N)]
View(bstar)

#*-- keyword -------------------------------------------------------------------
names(sc)
View(aakeyword)
sc[, keyword_len := nchar(keyword), by = 1:nrow(sc)]

bkeyword <- sc[, .N, by = .(keyword, keyword_len)][order(-N)]
View(bkeyword)

#*-- spec ----------------------------------------------------------------------
names(sc)
View(aaspec)
sc[, spec_num := (stringr::str_count(spec, ",") + 1)]
sc[spec == "", spec_num := 0]

bspec<- sc[, .N, by = .(spec, spec_num)][order(-N)]
View(bspec)

#*-- brand1  -------------------------------------------------------------------
names(sc)
View(aabrand1)
sc[, brand1_num := (stringr::str_count(brand1, ",") + 1)]
sc[brand1 == "", brand1_num := 0]

bbrand1<- sc[, .N, by = .(brand1, brand1_num)][order(-N)]
View(bbrand1)

#*-- brand2  -------------------------------------------------------------------
names(sc)
View(aabrand2)
sc[, brand2_num := (stringr::str_count(brand2, ",") + 1)]
sc[brand2 == "", brand2_num := 0]

bbrand2<- sc[, .N, by = .(brand2, brand2_num)][order(-N)]
View(bbrand2)

#*-- com  ----------------------------------------------------------------------
names(sc)
View(aacom2)
sc[, com2_num := (stringr::str_count(com2, ",") + 1)]
sc[com2 == "", com2_num := 0]

bcom2<- sc[, .N, by = .(com2, com2_num)][order(-N)]
View(bcom2)

#*-- fac  ----------------------------------------------------------------------
names(sc)
View(aafac)
sc[, fac_num := (stringr::str_count(fac, ",") + 1)]
sc[fac == "", fac_num := 0]

bfac<- sc[, .N, by = .(fac, fac_num)][order(-N)]
View(bfac)

#*-- distr  --------------------------------------------------------------------
names(sc)
View(aadistr2)
sc[, distr2_num := (stringr::str_count(distr2, ",") + 1)]
sc[distr2 == "", distr2_num := 0]

bdistr2<- sc[, .N, by = .(distr2, distr2_num)][order(-N)]
View(bdistr2)

#*-- criteria_num  -------------------------------------------------------------
sc[, priceN := ifelse(price != "", 1, 0)]
sc[, starN := ifelse(star != "", 1, 0)]
sc[, keywordN := ifelse(keyword != "", 1, 0)]
sc[, specN := ifelse(spec != "", 1, 0)]
sc[, brand1N := ifelse(brand1 != "", 1, 0)]
sc[, brand2N := ifelse(brand2 != "", 1, 0)]
sc[, distN := ifelse(dist != "", 1, 0)]
sc[, filterscoreN := ifelse(filterscore != "", 1, 0)]
sc[, com2N := ifelse(com2 != "", 1, 0)]
sc[, facN := ifelse(fac != "", 1, 0)]
sc[, filterquantityN := ifelse(filterquantity != "", 1, 0)]
sc[, sta2N := ifelse(sta2 != "", 1, 0)]
sc[, distr2N := ifelse(distr2 != "", 1, 0)]
sc[, metro11N := ifelse(metro11 != "", 1, 0)]
sc[, metro22N := ifelse(metro22 != "", 1, 0)]
sc[, hotd1N := ifelse(hotd1 != "", 1, 0)]
sc[, hotd2N := ifelse(hotd2 != "", 1, 0)]
sc[, sortN := ifelse(sort != "", 1, 0)]
sc[, bedtype2N := ifelse(bedtype2 != "", 1, 0)]
sc[, breakfast2N := ifelse(breakfast2 != "", 1, 0)]
sc[, paytype2N := ifelse(paytype2 != "", 1, 0)]
sc[, criteria_num := sum(priceN, starN, keywordN, specN, brand1N, brand2N, 
                         distN, filterscoreN, com2N, facN, filterquantityN, 
                         sta2N, distr2N, metro11N, metro22N, hotd1N, hotd2N, 
                         sortN, bedtype2N, breakfast2N, paytype2N), 
                      by = 1:nrow(sc)]
sc[, c("priceN", "starN", "keywordN", "specN", "brand1N", "brand2N", "distN", 
       "filterscoreN", "com2N", "facN", "filterquantityN", "sta2N", "distr2N", 
       "metro11N", "metro22N", "hotd1N", "hotd2N", "sortN", "bedtype2N", 
       "breakfast2N", "paytype2N") := NULL]

## check via a sample
sp <- sc[sample(1000)]
fwrite(sp, "check_spcrinum.csv", sep = "\t")


#*-- search date, search hour --------------------------------------------------
sc[, date := date(starttime)]
sc[, hour := hour(starttime)]
# check
sc[1:10, .(starttime, date, hour)]


#*-- self-defined sessions -----------------------------------------------------
sc <- sc[order(uid, starttime)]
# decide whether interval >= 30 min
sc[, session_sep30 := 1]
for (i in 2:nrow(sc)){
  print(i)
  if ((as.numeric(difftime(sc[i, starttime], 
                           sc[i-1, starttime], 
                           units = "mins")) <= 30 &
       sc[i, uid] == sc[i-1, uid])){ 
    sc[i, session_sep30 := sc[i-1, session_sep30]]
  } else if ((as.numeric(difftime(sc[i, starttime], 
                                  sc[i-1, starttime], 
                                  units = "mins")) > 30 &
              sc[i, uid] == sc[i-1, uid])){
    sc[i, session_sep30 := sc[i-1, session_sep30] + 1]
  } else {
    sc[i, session_sep30 := 1]
  }
}

#*-- query within sessions -----------------------------------------------------

## test
aa <- sc[1:100, .(uid, starttime, query_order, 
                  session_sep30)][order(uid, query_order)]
View(aa)
aa[, squery_order := frank(query_order), by = list(uid, session_sep30)]

## apply
sc[, squery_order := frank(query_order), by = list(uid, session_sep30)]


## user table ==================================================================

#*-- unique options ------------------------------------------------------------

user <- data.table(unique(sc[, uid]))
setnames(user, "uid")
setkey(user, uid)

op <- function(x){
  opt <<- sc[, .(uid, get(x))]
  setnames(opt, c("uid", x))
  opt <<- opt[, .(opt2 = paste(get(x), collapse = ",")), by = uid]
  opt[, ":=" (opt3= strsplit(opt2, split = ","), optnum = "")] 
  
  for (i in 1:nrow(opt)){
    print(paste(x, i))
    aa <- as.data.frame(opt[i, opt3],col.names = x)[,1]
    aa <- aa[which(aa!="")]
    opt[i, optnum := length(unique(aa))]
  }
  
  opt <- opt[, c(1, 4)]
  setnames(opt, c("uid", paste0(x,"num")))
  return(opt)
}

names(sc)
for (i in names(sc)[48:48]){
#  assign(paste("aa", i, sep = ""), setorder(sc[, .N, by = get(i)], -N))
  vb2 <- op(i)
#  user <- user[vb2, on = "uid"]
}

usr <- fread("user.csv")

# test usr table
names(usr)
str(usr)
setkey(sc, uid)
aa <- sc[usr[100:200]][, c(1, 29:50, 75:96)]
View(aa)

#-- dist, bedtype, sort fix
aa <- setkey(sc[, .(uid, sort_qry, sort)], uid)[vb2]
setkey(usr, uid)
usr <- usr[vb2]
usr[, bedtype2num := i.bedtype2num]
usr[, i.bedtype2num := NULL]
names(usr)

#*-- date, session, query ------------------------------------------------------

setkey(usr, uid)
## datenum
udate <- setnames(unique(sc[, .(uid, date)])[, .N, by = uid], c("uid", "datenum"))
usr <- usr[udate]
## sessionnum
usession <- setnames(
  sc[, .(uid, session_sep30)][, .(session = max(session_sep30)), by = uid], 
  c("uid", "sessionnum"))
usr <- usr[usession]
## querynum
uquery <- setnames(unique(sc[, .(uid, qid_new)])[, .N, by = uid], 
                   c("uid", "querynum"))
usr <- usr[uquery]


names(usr)




## save ========================================================================
rm(list=setdiff(ls(), c("dt", "sc", "usr")))
save.image("1cleanv0.RData")
fwrite(sc, "search_clean.csv")
fwrite(usr, "user.csv")
