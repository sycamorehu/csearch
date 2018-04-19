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
sc[price == "￥200以下￥950以上", 
   price := "below¥200above¥950"]
sc[price == "￥100以上￥200以下", 
   price := "above¥100below¥200"]
sc[price == "￥1000以上￥500以下", 
   price := "above¥1000below¥500"]
sc[price == "￥326以下￥600-950￥950以上", 
   price := "below¥326¥600-950above¥950"]
sc[price == "￥1000以上￥200以下", 
   price := "above¥1000below¥200"]
sc[price == "￥1000以上￥900以下", 
   price := "above¥1000below¥900"]
sc[price == "￥1000以上￥400以下", 
   price := "above¥1000below¥400"]
sc[price == "￥1000以上￥500以下￥200-300", 
   price := "above¥1000below¥500¥200-300"]
sc[price == "￥950以上￥850以下", 
   price := "above¥950below¥850"]
sc[price == "￥600-950￥700以下￥950以上￥450-600", 
   price := "¥600-950below¥700above¥950¥450-600"]
sc[price == "￥200以下￥150以上", 
   price := "below¥200above¥150"]
sc[price == "￥1000以上￥200-300￥250以下", 
   price := "above¥1000¥200-300below¥250"]
sc[price == "￥1000以上￥700以下", 
   price := "above¥1000below¥700"]
sc[price == "￥333以下￥1000以上", 
   price := "below¥333above¥1000"]
sc[price == "￥250以上￥200以下", 
   price := "above¥250below¥200"]
sc[price == "￥600-1000￥1000以上￥300-400￥200以下￥200-300￥400-600", 
   price := "¥600-1000above¥1000¥300-400below¥200¥200-300¥400-600"]
sc[price == "￥50以下￥1000以上", 
   price := "below¥50above¥1000"]
sc[price == "￥600-1000￥1000以上￥600以下", 
   price := "¥600-1000above¥1000below¥600"]
sc[price == "￥200以下￥450以上", 
   price := "below¥200above¥450"]
sc[price == "￥1050以上￥200以下", 
   price := "above¥1050below¥200"]
sc[price == "￥400以下￥1050以上￥200以下", 
   price := "below¥400above¥1050below¥200"]
sc[price == "￥300以下￥1050以上￥200以下", 
   price := "below¥300above¥1050below¥200"]
sc[price == "￥200以下￥300以上", 
   price := "below¥200above¥300"]
sc[price == "￥200以下￥1200以上", 
   price := "below¥200above¥1200"]
sc[price == "￥1000以上￥300-400￥200以下", 
   price := "above¥1000¥300-400below¥200"]
sc[price == "￥1000以上￥800以下", 
   price := "above¥1000below¥800"]
sc[price == "￥50以下￥1150以上", 
   price := "below¥50above¥1150"]
sc[price == "￥550以上￥200以下", 
   price := "above¥550below¥200"]
sc[price == "￥600-1000￥1000以上￥200以下", 
   price := "¥600-1000above¥1000below¥200"]
sc[price %like% "以上" , price := paste("above", price)]
sc[price %like% "以下" , price := paste("below", price)]
sc[, price := gsub("以上", "", price)]
sc[, price := gsub("以下", "", price)]
sc[, price := gsub("￥", "¥", price)]
sc[, price := gsub(" ", "", price)]
## check
assign(paste("aa", "price", sep = ""), setorder(sc[, .N, by = price], -N))
View(aaprice)

#*-- star ----------------------------------------------------------------------
names(sc)
View(astar_qry)
sc[, star := ""]
sc[star_qry %like% "1", star := "0-2"]
sc[star_qry %like% "2", star := paste(star, "0-2")]
sc[star_qry %like% "快捷连锁", star := paste(star, "0-2")]
sc[star_qry %like% "二星及以下/经济", star := paste(star, "0-2")]
sc[star_qry %like% "3", star := paste(star, "3")]
sc[star_qry %like% "三星/舒适", star := paste(star, "3")]
sc[star_qry %like% "三钻/舒适", star := paste(star, "3")]
sc[star_qry %like% "4", star := paste(star, "4")]
sc[star_qry %like% "四星/高档", star := paste(star, "4")]
sc[star_qry %like% "四钻/高档", star := paste(star, "4")]
sc[star_qry %like% "5", star := paste(star, "5")]
sc[star_qry %like% "五钻/豪华", star := paste(star, "5")]
sc[star_qry %like% "五星/豪华", star := paste(star, "5")]
sc[, star := gsub("0-2 0-2", "0-2", star)]
assign(paste("aa", "star", sep = ""), setorder(sc[, .N, by = star], -N))
View(aastar)

#*-- keyword -------------------------------------------------------------------
names(sc)
View(akeyword_qry)
sc[, keyword := keyword_qry]
sc[keyword == "-999", keyword := ""]
sc[, keyword := gsub("&", "", keyword)]
sc[, keyword := gsub("[/*]", "", keyword)]
assign(paste("aa", "keyword", sep = ""), setorder(sc[, .N, by = keyword], -N))
View(aakeyword)


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
sc[spec_qry %like% "浪漫情侣", spec := paste(spec, "romantic,")]
sc[spec_qry %like% "商务出行", spec := paste(spec, "business,")]
sc[spec_qry %like% "精品酒店", spec := paste(spec, "selection,")]
sc[spec_qry %like% "设计师酒店", spec := paste(spec, "designer hotel,")]
sc[spec_qry %like% "温泉酒店", spec := paste(spec, "hot spring,")]
sc[spec_qry %like% "购物便捷", spec := paste(spec, "shopping mall,")]
sc[spec_qry %like% "老饭店", spec := paste(spec, "old-style hotel,")]
sc[spec_qry %like% "美食林", spec := paste(spec, "genuine food,")]
sc[spec_qry %like% "全日房", spec := paste(spec, "all-day room,")]
sc[spec_qry %like% "钟点房", spec := paste(spec, "hour room,")]
sc[, spec := substr(spec, 1, nchar(spec)-1)]
assign(paste("aa", "spec", sep = ""), setorder(sc[, .N, by = spec], -N))
View(aaspec)


#*-- brand11 --------------------------------------------------------------------
names(sc)
View(abrand2_qry)
sc[, brand11 := ""]
## check
aaabrand12 <- sc[brand2_qry %like% "高端连锁" | 
                 brand2_qry %like% "中端连锁" | 
                 brand2_qry %like% "快捷连锁"][, .N, 
                   by = .(brand1_qry, brand2_qry)] [order(-N)]
aaabrand122 <- sc[!grepl("快捷", brand1_qry) & 
                  !grepl("中端", brand1_qry) & 
                  !grepl("高端", brand1_qry)][, .N, 
                    by = .(brand1_qry,brand2_qry)][order(-N)]
aaabrand012 <- sc[, .N, by = .(brand1, brand1_qry, brand2_qry)][order(-N)]

## encode from brand1_qry
sc[brand1_qry %like% "快捷" & 
   brand1_qry %like% "中端" & 
   brand1_qry %like% "高端", brand11 := "快捷连锁,中端连锁,高端连锁"]
sc[!grepl("快捷", brand1_qry) & 
   !grepl("中端", brand1_qry) & 
   !grepl("高端", brand1_qry), brand11 := ""]
sc[brand1_qry %like% "快捷" & 
   brand1_qry %like% "中端" & 
   !grepl("高端", brand1_qry), brand11 := "快捷连锁,中端连锁"]
sc[brand1_qry %like% "快捷" & 
   !grepl("中端", brand1_qry) & 
   brand1_qry %like% "高端", brand11 := "快捷连锁,高端连锁"]
sc[!grepl("快捷", brand1_qry) & 
   brand1_qry %like% "中端" & 
   brand1_qry %like% "高端", brand11 := "中端连锁,高端连锁"]
sc[brand1_qry %like% "快捷" & 
   !grepl("中端", brand1_qry) & 
   !grepl("高端", brand1_qry), brand11 := "快捷连锁"]
sc[!grepl("快捷", brand1_qry) & 
   brand1_qry %like% "中端" & 
   !grepl("高端", brand1_qry), brand11 := "中端连锁"]
sc[!grepl("快捷", brand1_qry) & 
   !grepl("中端", brand1_qry) & 
   brand1_qry %like% "高端", brand11 := "高端连锁"]

## encode part from brand2_qry
sc[!grepl("快捷", brand1_qry) & !grepl("中端", brand1_qry) & 
     !grepl("高端", brand1_qry) & (brand2_qry == "全季" |
                                   brand2_qry == "如家快捷" |
                                   brand2_qry == "锦江之星" |
                                   brand2_qry == "格林豪泰" |
                                   brand2_qry == "汉庭" |
                                   brand2_qry == "莫泰"|
                                   brand2_qry == "7天"|
                                   brand2_qry == "速8"|
                                   brand2_qry == "维也纳"|
                                   brand2_qry == "海友"), brand11 := "快捷连锁"]
sc[!grepl("快捷", brand1_qry) & !grepl("中端", brand1_qry) & 
     !grepl("高端", brand1_qry) & (brand2_qry == "如家精选" |
                                   brand2_qry == "桔子精选" |
                                   brand2_qry == "维也纳国际" |
                                   brand2_qry == "麗枫" |
                                 brand2_qry == "智选假日"), brand11 := "中端连锁"]
sc[!grepl("快捷", brand1_qry) & !grepl("中端", brand1_qry) & 
     !grepl("高端", brand1_qry) & (brand2_qry == "桔子水晶" |
                                   brand2_qry == "喜来登" |
                                   brand2_qry == "万豪" |
                                   brand2_qry == "万豪(Marriott)" |
                                   brand2_qry == "假日" |
                                   brand2_qry == "皇冠假日" |
                                   brand2_qry == "和颐"|
                                   brand2_qry == "洲际"|
                                   brand2_qry == "洲际(IHG)"|
                                   brand2_qry == "亚朵"|
                                   brand2_qry == "锦江" |
                                   brand2_qry == "希尔顿"|
                                   brand2_qry == "希尔顿(Hilton)"|
                                   brand2_qry == "凯悦(Hyatt)"|
                                   brand2_qry == "万丽"|
                                   brand2_qry == "万怡"|
                                   brand2_qry == "华美达"|
                                   brand2_qry == "W酒店"), brand11 := "高端连锁"]
                                 
## encode from brand2_qry
sc[brand2_qry %like% "快捷连锁" & (brand1_qry == "-999" |
                                 brand1_qry == "酒店类型"), brand11 := "快捷连锁"]
sc[brand2_qry %like% "中端连锁" & (brand1_qry == "-999" |
                                 brand1_qry == "酒店类型"), brand11 := "中端连锁"]
sc[brand2_qry %like% "高端连锁" & (brand1_qry == "-999" |
                                 brand1_qry == "酒店类型"), brand11 := "高端连锁"]

assign(paste("aa", "brand11", sep = ""), setorder(sc[, .N, by = brand11], -N))
View(aabrand11)
## usage 
tmp <- unique(sc[, .(uid, brand11)])
tmp1 <- tmp[brand11 != ""][, .N, by = brand11 ]
sum(tmp1$N)/404813  # 11.18% (45249)

#*-- brand12 -------------------------------------------------------------------
sc[, brand12 := ""]
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
     brand2_qry %like% "香格里拉", brand12 := "高端连锁"]

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
     brand2_qry %like% "格林豪泰" |
     brand2_qry %like% "喆啡" |
     brand2_qry %like% "华住" |
     brand2_qry %like% "浦江之星", brand12 := paste("快捷连锁", brand12)]

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
        brand2_qry %like% "桔子" |
        brand2_qry %like% "源涞" |
        brand2_qry %like% "景悦" |
        brand2_qry %like% "YUNIK" |
        brand2_qry %like% "凯莱", brand12 := paste(brand12, "中端连锁")]

assign(paste("aa", "brand12", sep = ""), setorder(sc[, .N, by = brand12], -N))
View(aabrand12)

## compare with brand11
aabrand1112 <- sc[, .N, by = .(brand11, brand12)]
View(aabrand1on)


#*-- brand1 --------------------------------------------------------------------
sc[,brand1 := ""]
sc[brand11 %like% "快捷连锁" | 
     brand12 %like% "快捷连锁", brand1 := "快捷连锁,"]
sc[brand11 %like% "中端连锁" | 
     brand12 %like% "中端连锁", brand1 := paste(brand1, "中端连锁,")]
sc[brand11 %like% "高端连锁" | 
     brand12 %like% "高端连锁", brand1 := paste(brand1, "高端连锁,")]
sc[, brand1 := substr(brand1, 1, nchar(brand1)-1)]
aabrand1 <- sc[, .N, by = brand1]
View(aabrand1)

## compare with brand2
aabrand12 <- sc[, .N, by = .(brand12, brand2)]
View(aabrand12)

#*-- brand2 --------------------------------------------------------------------
names(sc)
View(abrand2_qry)
sc[, brand2 := ""]
sc[brand2_qry == "快捷连锁", brand2 := ""]
sc[brand2_qry == "高端连锁", brand2 := ""]
sc[brand2_qry == "快捷连锁高端连锁", brand2 := ""]
sc[brand2_qry == "快捷连锁高端连锁", brand2 := ""]
sc[brand2_qry == "-999", brand2 := ""]

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

sc[, brand2 := substr(brand2, 1, nchar(brand2)-1)]
assign(paste("aa", "brand2", sep = ""), setorder(sc[, .N, by = brand2], -N))
View(aabrand2)

## usage 
tmp <- unique(sc[, .(uid, brand2)])
tmp1 <- tmp[brand2 != ""][, .N, by = brand2]
sum(tmp1$N)/404813  # 9.23% (37378)





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


#*-- com -----------------------------------------------------------------------
names(sc)
View(acom)
sc[, com2 := com]
sc[com2 == "-999", com2 := ""]
sc[grepl("\\d", com2), com2 := ""]
assign(paste("aa", "com2", sep = ""), setorder(sc[, .N, by = com2], -N))
View(aacom2)

## create variables ============================================================

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

