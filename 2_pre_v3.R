## header ======================================================================
# purpose: for first-year, price and star learning
# author: Zoey Hu
# create date: 05/10/2018 
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## load data ===================================================================

sc <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/search_clean.csv")
# sc <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/search_clean.csv")
dt <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/data/raw/tmp_jinyongliu_dxj_query_1.csv")
# dt <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/data/raw/tmp_jinyongliu_dxj_query_1.csv")
usr <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/project/csearch/user.csv")
# usr <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/user.csv")
rm(list = setdiff(ls(), c("sc", "usr", "dt")))


## price =======================================================================
# first get user table with labels:
# --booking price higher/lower than price criteria
# --start with high/low/mid price criteria
# --start with high/low/mid price range sizes
# then combine with price table

# 268921 queries, 60413 (14.9%) users set price ranges
# *Only contain those queries with price ranges. If they jump to a query
# *without price ranges, then that query would be left out.
price <- sc[price != "", .(uid, qid_new, session, query_order, squery_order, 
                           starttime, price, price_min, price_max, 
                           price_range, price_mean, price_num, ifclick, 
                           ifbook)][order(uid, query_order)]

#* prepare data ----------------------------------------------------------------

## 1. first price range ## 

## 1.1 identify query with first price range ##
# % .(qstart = ...) with the dot in front, no need to setnames.
price2 <- price[, .(qstart = min(query_order)), by = uid]
setkey(price2, uid)
# % price left outer join price2
price <- price2[price]
rm(price2)

## 1.2 get first price range user table ##
# - 60413 users, solid
first_range <- price[qstart == query_order, .(uid, price, price_min, price_max, 
                                              price_mean, price_range)]



## 2. label whether booking price higher or lower than price criteria ## 

## 2.1 get booking price ##

# * include all queries for they might not book in the query with price criteria
# - 16791 orders
# - check: length(unique(book_price[, uid]))  # 16169 users, solid
book_price <- dt[booking_bool == 1, .(uid, fh_price)][order(uid)]

## 2.2 get book_low, book_high ##
# % add multiple variables
# book.price[, ":=" (book_low = min(fh_price), 
#                   book_high = max(fh_price)), by = uid]
# % get only new values
book_price <- book_price[, .(book_low = min(fh_price), 
                             book_high = max(fh_price)), by = uid]
book_price[, book_price := (book_high + book_low)/2]

## 2.3 match booking price and first price range, label users ##
# - check length(unique(first.range.test[!is.na(book_low), uid])) : 3535
first_range <- book_price[first_range, on = "uid"]
first_range[, type_pp := ifelse(book_low < price_min, "lower", 
                           ifelse(book_high > price_max, "higher",
                                  ifelse(!is.na(book_low), "within", "no")))]
# - check type_pp: first_range[, .N, by = type_pp]

## 3. label starting with high/low/mid price criteria ##
# label: if > 3Q then high, <1Q then low, else mid
summary(price[, .(price_mean, price_min, price_max, price_range)])
first_range[, type_price := ifelse(price_max > 550, "high",
                                   ifelse(price_min < 150, "low", "mid"))]
# - check type_price: first_range[, .N, by = type_price]
first_range[, type_psize := ifelse(price_range > 315, "wide",
                                   ifelse(price_range < 150, "narrow", "mid"))]
# - check type_psize: first_range[, .N, by = type_psize]


## 4. combine user label table with price table ## 
setkey(first_range, uid)
price <- first_range[, .(uid, book_high, book_low, book_price, 
                              type_pp, type_price, type_psize)][price]


#*-- plot ----------------------------------------------------------------------

## 1. type_pp ##

## version 1: aggregate by queries ##
prc410 <- price[!is.na(type_pp) & 
                  session < 5 & 
                  squery_order < 11 & 
                  squery_order != 5.5]
prc410agg <- prc410[, .(price_mean = mean(price_mean), 
                       price_min = mean(price_min),
                       price_max = mean(price_max),
                       price_range = mean(price_range),
                       price_book = mean(book_price)), 
                   by = .(squery_order, type_pp)][order(squery_order)]
setnames(prc410agg, "squery_order", "query")


p <- ggplot(prc410agg) + 
  geom_line(aes(query, price_mean), colour = "#EDBDB2") + 
  #  geom_line(aes(query, price_min), colour = "#EDBDB2") + 
  #  geom_line(aes(query, price_max), colour = "#EDBDB2") + 
  #  geom_line(aes(query, price_book), colour = "blue") +
  geom_point(aes(query, price_mean, size = price_range), colour = "#CF5B43") +
  scale_size(range = c(0, 8)) +
  facet_grid(. ~ type_pp) +
  xlab("query") + ylab("average price") +
  ggtitle("Within-Session Learning: price purchased higher/lower than/within ranges") +
  labs(color='') +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggsave("type_pp.png", p)



## version 2: aggregate by type_pp ##
prc410 <- price[!is.na(type_pp)]
prc410agg <- prc410[, .(price_mean = mean(price_mean), 
                        price_min = mean(price_min),
                        price_max = mean(price_max),
                        price_range = mean(price_range),
                        price_book = mean(book_price)), 
                    by = .(type_pp)]
prc410agg[type_pp == "higher", type_pp := "higher than max inital"]
prc410agg[type_pp == "lower", type_pp := "lower than min inital"]
prc410agg[type_pp == "within", type_pp := "within inital"]

p <- ggplot(prc410agg) + 
  geom_bar(aes(x = type_pp, y = price_mean),
           stat = "identity", fill = "#CF5B43", width = 0.5) + 
  xlab("deviance type") + ylab("average price") +
  labs(color='') +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggsave("type_pp2.png", p)


## t-test ##
h <- as.matrix(prc410[type_pp != "higher", .(price_mean)])
l <- as.matrix(prc410[type_pp != "lower", .(price_mean)])
w <- as.matrix(prc410[type_pp != "within", .(price_mean)])

t.test(h, l, paired = FALSE)
t.test(w, l, paired = FALSE)
t.test(w, h, paired = FALSE)
bartlett.test(h ~ l)





## 2. type_price ##

# prepare data
prc410 <- price[!is.na(type_pp) & session < 5 & 
                  squery_order < 11 &
                  squery_order != 5.5]
prc410agg <- prc410[, .(price_mean = mean(price_mean), 
                        price_min = mean(price_min),
                        price_max = mean(price_max),
                        price_range = mean(price_range),
                        price_book = mean(book_price, na.rm = TRUE), 
                        count = .N), 
                    by = .(squery_order, type_price)][order(squery_order, type_price)]
# prepare data for conversions 
prc410agg2 <- prc410[ifbook == 1, .(price_mean = mean(price_mean), 
                        price_min = mean(price_min),
                        price_max = mean(price_max),
                        price_range = mean(price_range),
                        price_book = mean(book_price),  
                        count2 = .N), 
                    by = .(squery_order, type_price)][order(squery_order, type_price)]
# match
prc410agg[, index := paste0(squery_order, type_price)]
prc410agg2[, index := paste0(squery_order, type_price)]
prc410agg <- prc410agg[prc410agg2[, .(index, count2)], on = "index"]
prc410agg[, cr := count2/count]
setnames(prc410agg, "squery_order", "query")

# plot the main part
ggplot(prc410agg) + 
  geom_line(aes(query, price_mean), colour = "#CF5B43") + 
  geom_line(aes(query, price_book), colour = "blue") +
  geom_line(aes(query, price_min), colour = "#EDBDB2") +
  geom_line(aes(query, price_max), colour = "#EDBDB2") +
  geom_point(aes(query, price_mean, size = price_range), colour = "#CF5B43") +
  scale_size(range = c(0, 8)) +
  facet_grid(. ~ type_price) +
  xlab("Query") + ylab("average price") +
  ggtitle("Within-Session Learning: start with high/low/mid prices") +
  labs(color='') 
# ggsave("type_price.png", p)

# plot cr (on the secondary y axis)
ggplot(prc410agg) + 
  geom_line(aes(query, cr), colour = "green") + 
  facet_grid(. ~ type_price)





## 3. type_psize ## 

prc410 <- price[session < 5 & 
                  squery_order < 11 &
                  squery_order != 5.5]
prc410agg <- prc410[, .(price_mean = mean(price_mean), 
                        price_min = mean(price_min),
                        price_max = mean(price_max),
                        price_range = mean(price_range)), 
                    by = .(squery_order, type_psize)][order(squery_order)]
setnames(prc410agg, "squery_order", "query")

p <- ggplot(prc410agg) + 
  geom_line(aes(query, price_mean), colour = "#CF5B43") + 
  geom_point(aes(query, price_mean, size = price_range), colour = "#CF5B43") +
  scale_size(range = c(0, 8)) +
  facet_grid(. ~ type_psize) +
  xlab("Query") + ylab("average price") +
  ggtitle("Within-Session Learning: start with wide/narrow/mid range sizes") +
  labs(color='') 
# ggsave("type_psize.png", p)




#*-- plot individual -----------------------------------------------------------

indiv <- price[!is.na(book_price) &
                 type_pp == "higher"][, .(uid, query_order, session, 
                                          squery_order, book_price, price, 
                                          price_mean, price_min, price_max,
                                          type_pp)][order(uid, query_order)]
# check "higher": length(unique(indiv[, uid])): 779, solid
uid1 <- indiv[, (queries = max(query_order)), by = uid]
uid2 <- uid1[V1 > 1]  # query_order = 10: 35 users

# get sample
select <- data.table(uid2[, uid][sample(nrow(uid2), 10)])
setnames(select, "uid")
sample <- merge(indiv, select, by.test = "uid",  by.select = "uid")
setnames(sample, "squery_order", "query")


ggplot(sample) + 
#  geom_line(aes(query, price_mean, group = uid, colour = factor(uid))) + 
  geom_line(aes(query_order, price_max, group = uid, colour = factor(uid))) + 
  geom_line(aes(query_order, price_min, group = uid, colour = factor(uid))) + 
  geom_smooth(aes(x = query_order, y = price_mean, group = 1)) +
#  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 
#                                                    6, 8, 10)) + 
  xlab("Query") + ylab("price") +
  ggtitle("Price Learning") +
  labs(color='') +
  theme(legend.position="none") 

# ggsave("indiv3.png", p)

#*-- fwrite file individual ----------------------------------------------------
uid <- first_range[!is.na(book_price), ]
type_pp_higher <- sc[uid, on = "uid"]
fwrite(type_pp_higher, "type_pp_higher.csv")

#*-- overlap -------------------------------------------------------------------
ovlp <- sc[, .(book = sum(book)), by = uid]
ovlp2 <- ovlp[book > 1]
ovlp3 <- price[!is.na(type_pp)][ovlp2, on = "uid"]
length(unique(ovlp3[, uid]))
ovlp4 <- unique(ovlp3[, .(uid, book, type_pp)])
ovlp4[, .N, by = type_pp]


#*-- num of price ranges and purchase prices -----------------------------------

## 1. plot booking price vs Nqueries with price ranges ##
num_price_n <- price[, .N, by = uid]
num_price <- book_price[, .(uid, book_price)][num_price_n, on  = "uid"]
setnames(num_price, "N", "Nqueries")

## 1.1 point plot ##
p <- ggplot(num_price, aes(x = Nqueries, y = book_price)) +
  geom_point(colour = "#CF5B43", fill="#CF5B43", stroke=0, alpha = I(0.4)) +
  xlim(0, 100)+
  ylim(0, 2000) +
  xlab("number of queries with price ranges") +
  ylab("booking price")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggsave("price_numq.png", p)

## 1.2 histogram, dist of sessions and queries ##
num_price_dist <- num_price[, .N, by = Nqueries]
summary(num_price[, Nqueries])

num_session_query <- price[, .N, by = .(uid, session)][order(-N)]
num_dist_session_query <- num_session_query[, (queries = mean(N)), by = session]
ggplot(num_dist_session_query, aes(session, V1)) + 
  geom_point() +
  ylab("number of queries")



#*-- plot simple histogram -----------------------------------------------------
uid <- first_range[!is.na(book_price), ]
names(usr)
criteria_type_pp <- usr[uid, on = "uid"]

ggplot(criteria_type_pp) + 
  geom_histogram(aes(sortnum, y = ..density..), 
                 colour = "#EDBDB2",
                 binwidth = 2) + 
  facet_grid(. ~ type_pp) +
  xlab("criteria num") + ylab("count") +
  ggtitle("type_pp: other criteiria") +
  labs(color='') 


## 2. include star in price ##
names(criteria_type_pp)


## price type_pp and usage of other criteria ===================================

#*-- prepare data --------------------------------------------------------------

## 0. get users who set price ranges and purchased
# - check: uid[, .N, by = type_pp]
# - check: length(unique(uid[, uid]))
uid <- unique(price[!is.na(book_price)][, .(uid, type_pp)])

# - check: length(unique(price_criteria[, uid]))
price_criteria <- sc[, .(uid, qid_new, session, query_order, 
                                    squery_order, ifclick, ifbook, starttime, 
                                    price, price_min, price_max, price_range, 
                                    price_mean, price_num,star, keyword, spec,
                                    brand2, dist, filterscore, filterquantity,
                                    com2, fac, sta2, distr2, metro22,sort, 
                                    os2, advancedays, 
                                    staydays, criteria_num)][order(uid, 
                                                     query_order)][uid, 
                                                                   on = "uid"]


## 1.1 identify query with first price range ##
# data tables (price, book_price) was created in the previous section
qstart <- price[, .(qstart = min(query_order)), by = uid]
price_criteria <- qstart[price_criteria, on = "uid"]
rm(qstart)

## 1.2 get first price range user table ##
# - 60413 users, solid
first_range_c <- price_criteria[qstart == query_order, .(uid, price, 
                                                         price_min, price_max, 
                                                         price_mean, 
                                                         price_range)]


## 2. match booking price and first price range, label users ##
# - check length(unique(first.range.test[!is.na(book_low), uid])) : 3535
# - check type_pp: first_range_c[, .N, by = type_pp]
first_range_c <- book_price[first_range_c, on = "uid"]
first_range_c[, type_pp := ifelse(book_low < price_min, "lower", 
                                ifelse(book_high > price_max, "higher",
                                       ifelse(!is.na(book_low), "within", 
                                              "no")))]

## 2.1 get stopping query(purchase) ##
qend1 <- sc[ifbook == 1, .(uid, query_order)]
qend <- qend1[, .(qpurchase = max(query_order)), by = uid]


## 3. combine user label table with price table ## 
price_criteria <- first_range_c[, .(uid, book_high, book_low, book_price, 
                         type_pp)][price_criteria, on = "uid"]
price_criteria <- qend[price_criteria, on = "uid"]



## 4. criteria usage after qstart ##
price_criteria_user <- sqldf("
                             select uid, type_pp, book_price, qstart,
                                    os2 as os, 
                                    avg(advancedays) as advancedays, 
                                    avg(staydays) as staydays, 
                                    max(query_order) as queries, 
                                    max(session) as sessions,
                                    max(squery_order) as squeries,
                                    case when ifbook == 1 
                                       then query_order end as qbook,
                                    sum(case when query_order > qstart and
                                                    star != '' then 1 else 0
                                             end) as star_num,
                                    sum(case when query_order > qstart and
                                                    keyword != '' then 1 else 0
                                             end) as keyword_num,
                                    sum(case when query_order > qstart and
                                                    sort != '' then 1 else 0
                                             end) as sort_num,
                                    sum(case when query_order > qstart and
                                                    dist != '' then 1 else 0
                                             end) as dist_num,
                                    sum(case when query_order > qstart and
                                                    brand2 != '' then 1 else 0
                                             end) as brand2_num,
                                    sum(case when query_order > qstart and
                                                    spec != '' then 1 else 0
                                             end) as spec_num,
                                    sum(case when query_order > qstart and
                                                    com2 != '' then 1 else 0
                                             end) as com_num,
                                    sum(case when query_order > qstart and
                                                    distr2 != '' then 1 else 0
                                             end) as distr_num,
                                    sum(case when query_order > qstart and
                                                    sta2 != '' then 1 else 0
                                             end) as sta_num,
                                    sum(case when query_order > qstart and
                                                    metro22 != '' then 1 else 0
                                             end) as metro2_num,
                                    sum(case when query_order > qstart and
                                                    fac != '' then 1 else 0
                                             end) as fac_num,
                                    sum(case when query_order > qstart and
                                                    filterscore != '' then 1 else 0
                                             end) as filterscore_num,
                                    sum(case when query_order > qstart and
                                                    filterquantity != '' then 1 else 0
                                             end) as filterquantity_num
                             from price_criteria
                             group by uid, type_pp, book_price, qstart,os2
                             ")
## 4.1 label whether after qstart and before purchase##
price_criteria <- price_criteria[, afterq := ifelse(query_order > qstart,
                                                         1, 0)]
price_criteria <- price_criteria[, beforeq := ifelse(query_order <= qpurchase,
                                                    1, 0)]



#*-- plot usage of criteria: share within groups -------------------------------

## 0. prepare usage frame ##
max(price_criteria[, query_order])
criteria_frame <- setnames(data.table(c(1:259, 1:259, 1:259)), "query")
criteria_frame[1:259, type_pp := "higher"]
criteria_frame[260:518, type_pp := "lower"]
criteria_frame[519:777, type_pp := "within"]
criteria_frame[, index := paste0(type_pp, query)]

## 1. star ratings ##
# check: length(price_criteria[query_order == 2 & 
# star != "" & !is.na(star) & afterq == 1 & beforeq == 1, star])
star_num <- price_criteria[!is.na(star) & 
                             star != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(star_num[, percentage])
star_num[type_pp == "higher", 
         star := N/779]
star_num[type_pp == "lower", 
         star := N/323]
star_num[type_pp == "within", 
         star := N/2433]
star_num[, index := paste0(type_pp, query_order)]

criteria_frame <- star_num[, .(index, star)][criteria_frame, on = "index"]


## 2. keyword ##
keyword_num <- price_criteria[!is.na(keyword) & 
                                keyword != "" & 
                                afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(keyword_num[, percentage])
keyword_num[type_pp == "higher", 
            keyword := N/779]
keyword_num[type_pp == "lower", 
            keyword := N/323]
keyword_num[type_pp == "within", 
            keyword := N/2433]
keyword_num[, index := paste0(type_pp, query_order)]

criteria_frame <- keyword_num[, .(index, keyword)][criteria_frame, on = "index"]


## 3. spec ##
spec_num <- price_criteria[!is.na(spec) & 
                             spec != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(spec_num[, percentage])
spec_num[type_pp == "higher", 
         spec := N/779]
spec_num[type_pp == "lower", 
         spec := N/323]
spec_num[type_pp == "within", 
         spec := N/2433]
spec_num[, index := paste0(type_pp, query_order)]

criteria_frame <- spec_num[, .(index, spec)][criteria_frame, on = "index"]

## 4. brand2 ##
brand2_num <- price_criteria[!is.na(brand2) & 
                               brand2 != "" & 
                               afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(brand2_num[, percentage])
brand2_num[type_pp == "higher", 
           brand2 := N/779]
brand2_num[type_pp == "lower", 
           brand2 := N/323]
brand2_num[type_pp == "within", 
           brand2 := N/2433]
brand2_num[, index := paste0(type_pp, query_order)]

criteria_frame <- brand2_num[, .(index, brand2)][criteria_frame, on = "index"]

## 5. dist ##
dist_num <- price_criteria[!is.na(dist) & 
                             dist != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(dist_num[, percentage])
dist_num[type_pp == "higher", 
         dist := N/779]
dist_num[type_pp == "lower", 
         dist := N/323]
dist_num[type_pp == "within", 
         dist := N/2433]
dist_num[, index := paste0(type_pp, query_order)]

criteria_frame <- dist_num[, .(index, dist)][criteria_frame, on = "index"]

## 6. filterscore ##
filterscore_num <- price_criteria[!is.na(filterscore) & 
                                    filterscore != "" & 
                                    afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(filterscore_num[, percentage])
filterscore_num[type_pp == "higher", 
                filterscore := N/779]
filterscore_num[type_pp == "lower", 
                filterscore := N/323]
filterscore_num[type_pp == "within", 
                filterscore := N/2433]
filterscore_num[, index := paste0(type_pp, query_order)]

criteria_frame <- filterscore_num[, .(index, filterscore)][criteria_frame, on = "index"]

names(price_criteria)


## 7. filterquantity ##
filterquantity_num <- price_criteria[!is.na(filterquantity) & 
                                       filterquantity != "" & 
                                       afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(filterquantity_num[, percentage])
filterquantity_num[type_pp == "higher", 
                   filterquantity := N/779]
filterquantity_num[type_pp == "lower", 
                   filterquantity := N/323]
filterquantity_num[type_pp == "within", 
                   filterquantity := N/2433]
filterquantity_num[, index := paste0(type_pp, query_order)]

criteria_frame <- filterquantity_num[, .(index, filterquantity)][criteria_frame, on = "index"]


## 8. com2 ##
com2_num <- price_criteria[!is.na(com2) & 
                             com2 != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(com2_num[, percentage])
com2_num[type_pp == "higher", 
         com2 := N/779]
com2_num[type_pp == "lower", 
         com2 := N/323]
com2_num[type_pp == "within", 
         com2 := N/2433]
com2_num[, index := paste0(type_pp, query_order)]

criteria_frame <- com2_num[, .(index, com2)][criteria_frame, on = "index"]

## 9. fac ##
fac_num <- price_criteria[!is.na(fac) & 
                            fac != "" & 
                            afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(fac_num[, percentage])
fac_num[type_pp == "higher", 
        fac := N/779]
fac_num[type_pp == "lower", 
        fac := N/323]
fac_num[type_pp == "within", 
        fac := N/2433]
fac_num[, index := paste0(type_pp, query_order)]

criteria_frame <- fac_num[, .(index, fac)][criteria_frame, on = "index"]

## 10. sta2 ##
sta2_num <- price_criteria[!is.na(sta2) & 
                             sta2 != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(sta2_num[, percentage])
sta2_num[type_pp == "higher", 
         sta2 := N/779]
sta2_num[type_pp == "lower", 
         sta2 := N/323]
sta2_num[type_pp == "within", 
         sta2 := N/2433]
sta2_num[, index := paste0(type_pp, query_order)]

criteria_frame <- sta2_num[, .(index, sta2)][criteria_frame, on = "index"]

## 11. distr2 ##
distr2_num <- price_criteria[!is.na(distr2) & 
                               distr2 != "" & 
                               afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(distr2_num[, percentage])
distr2_num[type_pp == "higher", 
           distr2 := N/779]
distr2_num[type_pp == "lower", 
           distr2 := N/323]
distr2_num[type_pp == "within", 
           distr2 := N/2433]
distr2_num[, index := paste0(type_pp, query_order)]

criteria_frame <- distr2_num[, .(index, distr2)][criteria_frame, on = "index"]


## 12. metro22 ##
metro22_num <- price_criteria[!is.na(metro22) & 
                                metro22 != "" & 
                                afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(metro22_num[, percentage])
metro22_num[type_pp == "higher", 
            metro22 := N/779]
metro22_num[type_pp == "lower", 
            metro22 := N/323]
metro22_num[type_pp == "within", 
            metro22 := N/2433]
metro22_num[, index := paste0(type_pp, query_order)]

criteria_frame <- metro22_num[, .(index, metro22)][criteria_frame, on = "index"]


## 13. sort ##
sort_num <- price_criteria[!is.na(sort) & 
                             sort != "" & 
                             afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(sort_num[, percentage])
sort_num[type_pp == "higher", 
         sort := N/779]
sort_num[type_pp == "lower", 
         sort := N/323]
sort_num[type_pp == "within", 
         sort := N/2433]
sort_num[, index := paste0(type_pp, query_order)]

criteria_frame <- sort_num[, .(index, sort)][criteria_frame, on = "index"]


## 14. price ##
price_num <- price_criteria[!is.na(price) & 
                              price != "" & 
                              afterq == 1 & beforeq == 1, .N, by = .(type_pp, query_order)]
# check: sum(price_num[, percentage])
price_num[type_pp == "higher", 
          price := N/779]
price_num[type_pp == "lower", 
          price := N/323]
price_num[type_pp == "within", 
          price := N/2433]
price_num[, index := paste0(type_pp, query_order)]

criteria_frame <- price_num[, .(index, price)][criteria_frame, on = "index"]



## plot all ##
p <- ggplot(criteria_frame) +
  geom_line(aes(x = query,  y = price, colour = factor("price range"))) +
  geom_line(aes(x = query,  y = star, colour = factor("star rating"))) +
  geom_line(aes(x = query,  y = keyword, colour = factor("keyword"))) +
  geom_line(aes(x = query,  y = spec, colour = factor("hotel style"))) +
  geom_line(aes(x = query,  y = brand2, colour = factor("brand"))) +
  geom_line(aes(x = query,  y = dist, colour = factor("distance"))) +
  geom_line(aes(x = query,  y = filterscore, colour = factor("review score"))) +
  geom_line(aes(x = query,  y = filterquantity, colour = factor("num of review"))) +
  geom_line(aes(x = query,  y = com2, colour = factor("commercial area"))) +
  geom_line(aes(x = query,  y = fac, colour = factor("facility"))) +
  geom_line(aes(x = query,  y = sta2, colour = factor("airport/train station"))) +
  geom_line(aes(x = query,  y = distr2, colour = factor("administrative district"))) +
  geom_line(aes(x = query,  y = metro22, colour = factor("metro station"))) +
  geom_line(aes(x = query,  y = sort, colour = factor("sorting"))) +
  facet_grid(. ~ type_pp) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) + 
  ylab("percentage") +
  ggtitle("usage of criteria after first price range")
ggsave("price_criteria.png", p)


## plot selected ##
stargazer(criteria_frame, type = "text")

ggplot(criteria_frame) +
  facet_grid(. ~ type_pp) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) + 
  theme(legend.title = element_blank()) +
  ylab("percentage") +
  ggtitle("usage of criteria after first price range") +

  geom_line(aes(x = query,  y = sort, colour = factor("sorting"))) +
  geom_line(aes(x = query,  y = star, colour = factor("star rating"))) +
  geom_line(aes(x = query,  y = keyword, colour = factor("keyword"))) +
  geom_line(aes(x = query,  y = filterscore, colour = factor("review score"))) +
  geom_line(aes(x = query,  y = dist, colour = factor("distance"))) +
  geom_line(aes(x = query,  y = com2, colour = factor("commercial area"))) +


  geom_line(aes(x = query,  y = distr2, colour = factor("administrative district"))) +
  geom_line(aes(x = query,  y = metro22, colour = factor("metro station"))) +
  geom_line(aes(x = query,  y = spec, colour = factor("hotel style"))) +
  geom_line(aes(x = query,  y = brand2, colour = factor("brand"))) +

  
  geom_line(aes(x = query,  y = sta2, colour = factor("airport/train station"))) +
  geom_line(aes(x = query,  y = fac, colour = factor("facility"))) +
  geom_line(aes(x = query,  y = filterquantity, colour = factor("num of review"))) 
  
  
## plot converged ##

criteria_frame[, location := sum(dist, com2, sta2, distr2, metro22, 
                                 na.rm = TRUE), 
               by = 1: nrow(criteria_frame)]
criteria_frame[, attribute := sum(spec, fac, na.rm = TRUE), 
               by = 1: nrow(criteria_frame)]
criteria_frame[, review := sum(filterquantity, filterscore, na.rm = TRUE), 
               by = 1: nrow(criteria_frame)]
criteria_frame[location == 0, location := NA]
criteria_frame[attribute == 0, attribute := NA]
criteria_frame[review == 0, review := NA]


p <- ggplot(criteria_frame) +
  geom_line(aes(x = query,  y = price, colour = factor("price range"))) +
  geom_line(aes(x = query,  y = star, colour = factor("star rating"))) +
  geom_line(aes(x = query,  y = keyword, colour = factor("keyword"))) +
  geom_line(aes(x = query,  y = sort, colour = factor("sorting"))) +
  geom_line(aes(x = query,  y = location, colour = factor("location-related"))) +
  geom_line(aes(x = query,  y = review, colour = factor("review-related"))) +
  geom_line(aes(x = query,  y = attribute, colour = factor("attribute-related"))) +
  facet_grid(. ~ type_pp) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) + 
  ylab("percentage") +
  ggtitle("usage of criteria after first price range")

ggsave("price_criteria_converged.png", p)


#*-- plot usage of criteria: share within queries ------------------------------


## 0. prepare usage frame ##
max(price_criteria[, query_order])
criteria_frame2 <- setnames(data.table(c(1:259, 1:259, 1:259)), "query")
criteria_frame2[1:259, type_pp := "higher"]
criteria_frame2[260:518, type_pp := "lower"]
criteria_frame2[519:777, type_pp := "within"]
criteria_frame2[, index := paste0(type_pp, query)]

## 1. star ratings ##
# check: length(price_criteria[query_order == 2 & 
# star != "" & !is.na(star) & afterq == 1 & beforeq == 1, star])
star_num <- price_criteria[!is.na(star) & 
                             star != "" & 
                             afterq == 1 & beforeq == 1, .(star_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- star_num[, .(index, star_raw)][criteria_frame2, on = "index"]


## 2. keyword ##
keyword_num <- price_criteria[!is.na(keyword) & 
                                keyword != "" & 
                                afterq == 1 & beforeq == 1, .(keyword_raw = .N, 
                                               index = paste0(type_pp, 
                                                              query_order)), 
                              by = .(type_pp, query_order)]

criteria_frame2 <- keyword_num[, .(index, keyword_raw)][criteria_frame2, on = "index"]


## 3. spec ##
spec_num <- price_criteria[!is.na(spec) & 
                             spec != "" & 
                             afterq == 1 & beforeq == 1, .(spec_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- spec_num[, .(index, spec_raw)][criteria_frame2, on = "index"]

## 4. brand2 ##
brand2_num <- price_criteria[!is.na(brand2) & 
                               brand2 != "" & 
                               afterq == 1 & beforeq == 1, .(brand2_raw = .N, 
                                              index = paste0(type_pp, 
                                                             query_order)), 
                             by = .(type_pp, query_order)]

criteria_frame2 <- brand2_num[, .(index, brand2_raw)][criteria_frame2, on = "index"]

## 5. dist ##
dist_num <- price_criteria[!is.na(dist) & 
                             dist != "" & 
                             afterq == 1 & beforeq == 1, .(dist_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- dist_num[, .(index, dist_raw)][criteria_frame2, on = "index"]

## 6. filterscore ##
filterscore_num <- price_criteria[!is.na(filterscore) & 
                                    filterscore != "" & 
                                    afterq == 1 & beforeq == 1, .(filterscore_raw = .N, 
                                                   index = paste0(type_pp, 
                                                                  query_order)), 
                                  by = .(type_pp, query_order)]

criteria_frame2 <- filterscore_num[, .(index, filterscore_raw)][criteria_frame2, on = "index"]


## 7. filterquantity ##
filterquantity_num <- price_criteria[!is.na(filterquantity) & 
                                       filterquantity != "" & 
                                       afterq == 1 & beforeq == 1, .(filterquantity_raw = .N, 
                                                      index = paste0(type_pp, 
                                                                     query_order)), 
                                     by = .(type_pp, query_order)]

criteria_frame2 <- filterquantity_num[, .(index, filterquantity_raw)][criteria_frame2, on = "index"]


## 8. com2 ##
com2_num <- price_criteria[!is.na(com2) & 
                             com2 != "" & 
                             afterq == 1 & beforeq == 1, .(com2_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- com2_num[, .(index, com2_raw)][criteria_frame2, on = "index"]

## 9. fac ##
fac_num <- price_criteria[!is.na(fac) & 
                            fac != "" & 
                            afterq == 1 & beforeq == 1, .(fac_raw = .N, 
                                           index = paste0(type_pp, 
                                                          query_order)), 
                          by = .(type_pp, query_order)]

criteria_frame2 <- fac_num[, .(index, fac_raw)][criteria_frame2, on = "index"]

## 10. sta2 ##
sta2_num <- price_criteria[!is.na(sta2) & 
                             sta2 != "" & 
                             afterq == 1 & beforeq == 1, .(sta2_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- sta2_num[, .(index, sta2_raw)][criteria_frame2, on = "index"]

## 11. distr2 ##
distr2_num <- price_criteria[!is.na(distr2) & 
                               distr2 != "" & 
                               afterq == 1 & beforeq == 1, .(distr2_raw = .N, 
                                              index = paste0(type_pp, 
                                                             query_order)), 
                             by = .(type_pp, query_order)]

criteria_frame2 <- distr2_num[, .(index, distr2_raw)][criteria_frame2, on = "index"]


## 12. metro22 ##
metro22_num <- price_criteria[!is.na(metro22) & 
                                metro22 != "" & 
                                afterq == 1 & beforeq == 1, .(metro22_raw = .N, 
                                               index = paste0(type_pp, 
                                                              query_order)), 
                              by = .(type_pp, query_order)]

criteria_frame2 <- metro22_num[, .(index, metro22_raw)][criteria_frame2, on = "index"]


## 13. sort ##
sort_num <- price_criteria[!is.na(sort) & 
                             sort != "" & 
                             afterq == 1 & beforeq == 1, .(sort_raw = .N, 
                                            index = paste0(type_pp, 
                                                           query_order)), 
                           by = .(type_pp, query_order)]

criteria_frame2 <- sort_num[, .(index, sort_raw)][criteria_frame2, on = "index"]


## 14. price ##
price_num <- price_criteria[!is.na(price) & 
                              price != "" & 
                              afterq == 1 & beforeq == 1, .(price_raw = .N, 
                                             index = paste0(type_pp, 
                                                            query_order)), 
                            by = .(type_pp, query_order)]
criteria_frame2 <- price_num[, .(index, price_raw)][criteria_frame2, on = "index"]


## get percentage within queries ##
criteria_frame2[, Nquery := sum(price_raw, sort_raw, metro22_raw, distr2_raw,
                                sta2_raw, fac_raw, com2_raw, filterquantity_raw,
                                filterscore_raw, dist_raw, brand2_raw, spec_raw, 
                                keyword_raw, star_raw, na.rm = TRUE), 
                by = 1: nrow(criteria_frame2)]

criteria_frame2 <- price_num[, .(index, price_raw)][criteria_frame2, on = "index"]
criteria_frame2[, price := price_raw / Nquery]
criteria_frame2[, sort := sort_raw / Nquery]
criteria_frame2[, metro22 := metro22_raw / Nquery]
criteria_frame2[, distr2 := distr2_raw / Nquery]
criteria_frame2[, sta2 := sta2_raw / Nquery]
criteria_frame2[, fac := fac_raw / Nquery]
criteria_frame2[, com2 := com2_raw / Nquery]
criteria_frame2[, filterquantity := filterquantity_raw / Nquery]
criteria_frame2[, filterscore := filterscore_raw / Nquery]
criteria_frame2[, dist := dist_raw / Nquery]
criteria_frame2[, brand2 := brand2_raw / Nquery]
criteria_frame2[, spec := spec_raw / Nquery]
criteria_frame2[, keyword := keyword_raw / Nquery]
criteria_frame2[, star := star_raw / Nquery]





## plot all ##
ggplot(criteria_frame2) +
  geom_line(aes(x = query,  y = price, colour = factor("price range"))) +
  geom_line(aes(x = query,  y = star, colour = factor("star rating"))) +
  geom_line(aes(x = query,  y = keyword, colour = factor("keyword"))) +
  geom_line(aes(x = query,  y = spec, colour = factor("hotel style"))) +
  geom_line(aes(x = query,  y = brand2, colour = factor("brand"))) +
  geom_line(aes(x = query,  y = dist, colour = factor("distance"))) +
  geom_line(aes(x = query,  y = filterscore, colour = factor("review score"))) +
  geom_line(aes(x = query,  y = filterquantity, colour = factor("num of review"))) +
  geom_line(aes(x = query,  y = com2, colour = factor("commercial area"))) +
  geom_line(aes(x = query,  y = fac, colour = factor("facility"))) +
  geom_line(aes(x = query,  y = sta2, colour = factor("airport/train station"))) +
  geom_line(aes(x = query,  y = distr2, colour = factor("administrative district"))) +
  geom_line(aes(x = query,  y = metro22, colour = factor("metro station"))) +
  geom_line(aes(x = query,  y = sort, colour = factor("sorting"))) +
  facet_grid(. ~ type_pp) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) + 
  ylim(0, 0.42) +
  ylab("percentage") +
  ggtitle("usage of criteria after first price range")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggsave("price_criteria.png", p)



## plot converged ##

criteria_frame2[, location := sum(dist, com2, sta2, distr2, metro22, 
                                 na.rm = TRUE), 
               by = 1: nrow(criteria_frame2)]
criteria_frame2[, attribute := sum(spec, fac, na.rm = TRUE), 
               by = 1: nrow(criteria_frame2)]
criteria_frame2[, review := sum(filterquantity, filterscore, na.rm = TRUE), 
               by = 1: nrow(criteria_frame2)]
criteria_frame2[location == 0, location := NA]
criteria_frame2[attribute == 0, attribute := NA]
criteria_frame2[review == 0, review := NA]

p <- ggplot(criteria_frame2) +
  geom_line(aes(x = query,  y = price, colour = factor("price range"))) +
  geom_line(aes(x = query,  y = star, colour = factor("star rating"))) +
  geom_line(aes(x = query,  y = keyword, colour = factor("keyword"))) +
  geom_line(aes(x = query,  y = sort, colour = factor("sorting"))) +
  geom_line(aes(x = query,  y = brand2, colour = factor("brand"))) +
  geom_line(aes(x = query,  y = location, colour = factor("location-related"))) +
  geom_line(aes(x = query,  y = review, colour = factor("review-related"))) +
  geom_line(aes(x = query,  y = attribute, colour = factor("attribute-related"))) +
  facet_grid(. ~ type_pp) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) + 
  ylim(0, 0.42) +
  ylab("percentage") +
  ggtitle("usage of criteria after first price range")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggsave("price_criteria_converged_q_a8.png", p)


#*-- plot usage of criteria: bar, average --------------------------------------


## 0. prepare usage frame ##
criteria_frame3 <- price_criteria[criteria_num != 0, .N, by = type_pp]


## 1. star ratings ##
star_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when star = '' then 0 else 1 end) as star
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
star_num <- star_num0[, .(star = mean(as.numeric(star))), by = type_pp]
criteria_frame3 <- star_num[criteria_frame3, on = "type_pp"]


## 2. keyword ##
keyword_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when keyword = '' then 0 else 1 end) as keyword
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
keyword_num <- keyword_num0[, .(keyword = mean(as.numeric(keyword))), by = type_pp]
criteria_frame3 <- keyword_num[criteria_frame3, on = "type_pp"]


## 3. spec ##
spec_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when spec = '' then 0 else 1 end) as spec
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
spec_num <- spec_num0[, .(spec = mean(as.numeric(spec))), by = type_pp]
criteria_frame3 <- spec_num[criteria_frame3, on = "type_pp"]

## 4. brand2 ##
brand2_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when brand2 = '' then 0 else 1 end) as brand2
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
brand2_num <- brand2_num0[, .(brand2 = mean(as.numeric(brand2))), by = type_pp]
criteria_frame3 <- brand2_num[criteria_frame3, on = "type_pp"]

## 5. dist ##
dist_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when dist = '' then 0 else 1 end) as dist
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
dist_num <- dist_num0[, .(dist = mean(as.numeric(dist))), by = type_pp]
criteria_frame3 <- dist_num[criteria_frame3, on = "type_pp"]

## 6. filterscore ##
filterscore_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when filterscore = '' then 0 else 1 end) as filterscore
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
filterscore_num <- filterscore_num0[, .(filterscore = mean(as.numeric(filterscore))), by = type_pp]
criteria_frame3 <- filterscore_num[criteria_frame3, on = "type_pp"]


## 7. filterquantity ##
filterquantity_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when filterquantity = '' then 0 else 1 end) as filterquantity
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
filterquantity_num <- filterquantity_num0[, .(filterquantity = mean(as.numeric(filterquantity))), by = type_pp]
criteria_frame3 <- filterquantity_num[criteria_frame3, on = "type_pp"]

## 8. com2 ##
com2_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when com2 = '' then 0 else 1 end) as com2
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
com2_num <- com2_num0[, .(com2 = mean(as.numeric(com2))), by = type_pp]
criteria_frame3 <- com2_num[criteria_frame3, on = "type_pp"]

## 9. fac ##
fac_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when fac = '' then 0 else 1 end) as fac
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
fac_num <- fac_num0[, .(fac = mean(as.numeric(fac))), by = type_pp]
criteria_frame3 <- fac_num[criteria_frame3, on = "type_pp"]

## 10. sta2 ##
sta2_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when sta2 = '' then 0 else 1 end) as sta2
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
sta2_num <- sta2_num0[, .(sta2 = mean(as.numeric(sta2))), by = type_pp]
criteria_frame3 <- sta2_num[criteria_frame3, on = "type_pp"]

## 11. distr2 ##
distr2_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when distr2 = '' then 0 else 1 end) as distr2
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
distr2_num <- distr2_num0[, .(distr2 = mean(as.numeric(distr2))), by = type_pp]
criteria_frame3 <- distr2_num[criteria_frame3, on = "type_pp"]


## 12. metro22 ##
metro22_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when metro22 = '' then 0 else 1 end) as metro22
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
metro22_num <- metro22_num0[, .(metro22 = mean(as.numeric(metro22))), by = type_pp]
criteria_frame3 <- metro22_num[criteria_frame3, on = "type_pp"]


## 13. sort ##
sort_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when sort = '' then 0 else 1 end) as sort
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
sort_num <- sort_num0[, .(sort = mean(as.numeric(sort))), by = type_pp]
criteria_frame3 <- sort_num[criteria_frame3, on = "type_pp"]


## 14. price ##
price_num0 <- data.table(sqldf("
                  select uid, type_pp, sum(case when price = '' then 0 else 1 end) as price
                  from price_criteria
                  where afterq = 1 and beforeq = 1
                  group by uid, type_pp
                  "))
price_num <- price_num0[, .(price = mean(as.numeric(price))), by = type_pp]
criteria_frame3 <- price_num[criteria_frame3, on = "type_pp"]


## converge ##
criteria_frame3[, location := sum(dist, com2, sta2, distr2, metro22, 
                                  na.rm = TRUE), 
                by = 1: nrow(criteria_frame3)]
criteria_frame3[, attribute := sum(spec, fac, na.rm = TRUE), 
                by = 1: nrow(criteria_frame3)]
criteria_frame3[, review := sum(filterquantity, filterscore, na.rm = TRUE), 
                by = 1: nrow(criteria_frame3)]
criteria_frame3[location == 0, location := NA]
criteria_frame3[attribute == 0, attribute := NA]
criteria_frame3[review == 0, review := NA]


## melt ##
criteria_frame3[, N:= NULL]
criteria_frame3 <- melt(criteria_frame3, id = "type_pp")
setnames(criteria_frame3, "variable", "criteria")
setnames(criteria_frame3, "value", "queries")

criteria_frame3[type_pp == "higher", type_pp := "higher than max inital"]
criteria_frame3[type_pp == "lower", type_pp := "lower than min inital"]
criteria_frame3[type_pp == "within", type_pp := "within inital"]

## plot converged ##

p1 <- ggplot(criteria_frame3[criteria == "star"]) +
  geom_bar(aes(x = type_pp, y = queries), 
           fill = "#CF5B43", 
           stat = "identity",
           width = 0.5) +
  facet_grid(criteria ~ .) +
  xlab("star") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- ggplot(criteria_frame3[criteria == "keyword"]) +
  geom_bar(aes(x = type_pp, y = queries), 
           fill = "#CF5B43", 
           stat = "identity") +
  facet_grid(criteria ~ .) +
  xlab("keyword") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

# p4 <- ggplot(criteria_frame3[criteria == "price"]) +
#   geom_bar(aes(x = type_pp, y = queries),
#            fill = "#CF5B43",
#            stat = "identity") +
#   facet_grid(criteria ~ .) +
#   xlab("price") +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))+
#   theme(axis.title.x=element_blank(),
#         axis.ticks.x=element_blank())

ggplot(criteria_frame3[criteria == "brand2"]) +
  geom_bar(aes(x = type_pp, y = queries),
           fill = "#CF5B43",
           stat = "identity") +
  facet_grid(criteria ~ .) +
  xlab("brand") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

p3 <- ggplot(criteria_frame3[criteria == "location"]) +
  geom_bar(aes(x = type_pp, y = queries), 
           fill = "#CF5B43", 
           stat = "identity") +
  facet_grid(criteria ~ .) +
  xlab("location") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

p4 <- ggplot(criteria_frame3[criteria == "review"]) +
  geom_bar(aes(x = type_pp, y = queries),
           fill = "#CF5B43",
           stat = "identity") +
  facet_grid(criteria ~ .) +
  xlab("review") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

# ggplot(criteria_frame3[criteria == "attribute"]) +
#   geom_bar(aes(x = type_pp, y = queries), 
#            fill = "#CF5B43", 
#            stat = "identity") +
#   facet_grid(criteria ~ .) +
#   xlab("attribute") + 
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))

p <- multiplot(p1, p2, p3, p4, cols = 2)
ggsave("price_criteria_bar_star.png", p1)


## fwrite ##
fwrite(criteria_frame3, "price_criteria.csv")


## explore =====================================================================
#*-- advancedays ---------------------------------------------------------------

# who set price ranges and purchase
length(unique(price_criteria[, uid]))
adv_dist <- price_criteria[, .(advancedays = mean(advancedays)), 
                           by = uid][, .N, by = advancedays][order(-N)]
sum(adv_dist[, N])
adv_dist[, percentage := N/3535]

# total 
length(unique(sc[, uid]))
adv_dist <- sc[, .(advancedays = mean(advancedays)), 
                           by = uid][, .N, by = advancedays][order(-N)]
sum(adv_dist[, N])
adv_dist[, percentage := N/404813]

#*-- criteria num --------------------------------------------------------------

criteria_book <- usr[ifbook == 1, .N, criterianum][order(-N)]
sum(criteria_book[, N])
criteria_book[, percentage := N/16169]
