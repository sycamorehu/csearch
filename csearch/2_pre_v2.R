## header ======================================================================
# purpose: for first-year, after literature, with big picture
# author: Zoey Hu
# create date: 04/29/2018 
# last update: //2018 
#
# input file: search_clean.csv
# output file: plots, slides

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


## basics ======================================================================

#*-- overall -------------------------------------------------------------------

## number of sessions ##
setnames(sc, 74, "session")
nsession <- unique(sc[, .(uid, session)])  # 631,325


#*-- user behavior (usr table) -------------------------------------------------

## overivew ##
names(usr)
usr2 <- usr[, .(datenum, sessionnum, querynum, criterianum, iclick, ibook)]
setnames(usr2, c("Ndays", "Nsessions","Nqueries", "Ncriteria", 
                 "Click", "Conversion"))
names(usr2)
stargazer(usr2)

#-- check outlier @@@@@@@@@@@@@@@@@to be continue
usr[criterianum == 160]
aa <- sc[uid == "82cdd2d1610619dd74ce2083df5d00d1"]


## distribution ##
p1 <- ggplot(usr, aes(datenum, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlab("Ndays") +
  ylab("percentage")
p2 <- ggplot(usr, aes(sessionnum, ..density..)) + facet_grid(. ~ type) +
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(0, 10) + 
  xlab("Nsessions") +
  ylab("percentage")
p3 <- ggplot(usr, aes(querynum, ..density..)) + facet_grid(. ~ type) +
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(0, 20) + 
  xlab("Nqueries") +
  ylab("percentage")
p4 <- ggplot(usr, aes(criterianum, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(-1, 30) + 
  xlab("Ncriteria") +
  ylab("percentage")
multiplot(p1, p2, p3, p4, cols=2)

sc[1:100, distr]

p5 <- ggplot(usr, aes(maxsquery, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(0, 30) + 
  xlab("Maxsquery") +
  ylab("percentage")
summary(usr[, maxsquery])
ggsave("maxsquery.png", p5)

#*-- hotel ---------------------------------------------------------------------
names(dt)

## display, click, book ##
htldisplay <- dt[, .(hotelid)][, .N, by = hotelid]
htlcr <- dt[, .(hotelid, click_bool, 
                booking_bool)][, .(clicks = sum(click_bool), 
                                   books = sum(booking_bool)), by = hotelid]
setkey(htldisplay, hotelid)
htl <- htldisplay[htlcr]
names(htl)
setnames(htl, c("hotelid", "views", "clicks", "bookings"))
stargazer(htl)

## distribution ##

p1 <- ggplot(htl, aes(views, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(-1, 10) + 
  xlab("Views") +
  ylab("percentage")
p2 <- ggplot(htl, aes(clicks, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(-1, 10) + 
  xlab("Clicks") +
  ylab("percentage")
p3 <- ggplot(htl, aes(bookings, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(-1, 10) + 
  xlab("Bookings") +
  ylab("percentage")
multiplot(p1, p2, p3, cols=3)


#*-- purchase position ---------------------------------------------------------

book <- sc[, .(uid, session, squery_order, ifbook)]
bookag <- book[, .N, by = list(session, squery_order)]
bookag[, index := paste(session, squery_order, sep = ",")]
setnames(bookag, c("session", "query", "Ntotal", "index"))
bookag2 <- book[ifbook == 1, .N, by = list(session, squery_order)]
bookag2[, index := paste(session, squery_order, sep = ",")]
setnames(bookag2, c("session", "query", "Npurchase", "index"))
setkey(bookag, index)
setkey(bookag2, index)
bookag3 <- bookag[bookag2]
bookag3[, session := i.session]
bookag3[, query := i.query]
bookag3[, i.session := NULL]
bookag3[, i.query := NULL]
bookag3[, percentage := Npurchase/Ntotal]

#-- plot frequency
p <- ggplot(bookag3, aes(session, query)) +
  geom_point(aes(size = Ntotal), colour = "#CF5B43") +
  scale_size(range = c(0, 8)) +
  theme(legend.position = "none")

#-- plot percentage
g <- ggplot(bookag3, aes(session, query)) +
  geom_point(aes(size = percentage), alpha = I(0.3),  colour = "blue") +
  scale_size(range = c(0, 8)) +
  theme(legend.position = "none")
g2 <- ggplot(bookag3, aes(session, query)) +
  geom_point(aes(size = percentage, colour = Ntotal)) 
ggsave("purchase_position_perc2.png", g)
ggsave("purchase_position_freq2.png", p)



#*-- group statistics-----------------------------------------------------------

names(usr)
group <- usr[, .(datenum, sessionnum, querynum, criterianum, ifclick, ifbook,
                 priceset_usd, price_dev_usd, type, os, datenum_type, 
                 criterianum_type, advancedays_type)]
setnames(group, "priceset_usd", "price")
setnames(group, "price_dev_usd", "pdeviance")
total <- sapply(group[, 1:8], mean, na.rm=TRUE)
browser <- sapply(group[type == "browser", 1:8], mean, na.rm=TRUE)
explorer <- sapply(group[type == "explorer", 1:8], mean, na.rm=TRUE)
buyer <- sapply(group[type == "buyer", 1:8], mean, na.rm=TRUE)
Android <- sapply(group[os == "Android", 1:8], mean, na.rm=TRUE)
iOS <- sapply(group[os == "iOS", 1:8], mean, na.rm=TRUE)
book_same_day <- sapply(group[advancedays_type == "book the same day", 
                              1:8], mean, na.rm=TRUE)
book_lt_1week <- sapply(group[advancedays_type == "book within 1 week", 
                              1:8], mean, na.rm=TRUE)
book_mt_1week <- sapply(group[advancedays_type == 
                                         "book earlier than 1 week", 
                              1:8], mean, na.rm=TRUE)
search_1day <- sapply(group[datenum_type == "1 day", 1:8], mean, na.rm=TRUE)
search_2_3days <- sapply(group[datenum_type == "2-3 days", 1:8], mean, 
                         na.rm=TRUE)
search_above3days <- sapply(group[datenum_type == 
                                    "more than 3 days", 1:8], mean, na.rm=TRUE)
crireria_0 <- sapply(group[criterianum_type == "no criteria", 1:8], mean, 
                     na.rm=TRUE)
crireria_1_2 <- sapply(group[criterianum_type == "1-2 criteria", 1:8], mean, 
                       na.rm=TRUE)
crireria_above2 <- sapply(group[criterianum_type == 
                                  "more than 2 criteria", 1:8], mean, 
                          na.rm=TRUE)

groupstats0 <- rbind(total, browser, explorer, buyer, Android, iOS, book_same_day, 
            book_within_1week, book_earlierthan_1week, search_1day, 
            search_2_3days, search_above3days, crireria_0, crireria_1_2, 
            crireria_above2)
groupstats <- rbind(total, browser, explorer, buyer, Android, iOS, book_same_day, 
                    book_lt_1week, book_mt_1week)

stargazer(groupstats, type = "text")
stargazer(groupstats)
groupstats[]

## price =======================================================================
## get price table
prc <- sc[price != '', .(uid, qid_new, query_order, session, squery_order, 
                         price, price_min, price_max, 
                         price_range, price_mean)][order(uid, query_order)]
prc[, index := paste0(uid, session)]

## discover dist
names(usr)
prcdist <- usr[, .N, by = list(sessionnum, maxsquery)]
sum(prcdist[, N])
prcdist[, percentage := N/404813]
prcdist44 <- prcdist[sessionnum < 5 & maxsquery < 5]
sum(prcdist44[, percentage])

#*-- get prc44, within-session learning: mean and range size -------------------
prc44 <- prc[session < 5 & squery_order < 5,]
#-- price_mean
prcm44agg <- prc44[, (price = mean(price_mean)), 
                  by = .(session, squery_order)][order(session, squery_order)]
setnames(prcm44agg, c("session","query", "price_mean"))
prcm44agg[, query := paste("query", query)]
prcm44agg[, session := paste("session", session)]
prcm44tab <- prcm44agg %>% spread(query, price_mean)

p1 <- ggplot(prcm44agg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning: Mean of Price Ranges") +
  labs(color='') 
ggsave("price_mean_withinsession.png", p1)

#-- price_range
prcr44agg <- prc44[, (price = mean(price_range)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcr44agg, c("session","query", "price_range"))
prcr44agg[, query := paste("query", query)]
prcr44agg[, session := paste("session", session)]
prcr44tab <- prcr44agg %>% spread(query, price_range)

p2 <- ggplot(prcr44agg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning: Price Range Sizes") +
  labs(color='') 
ggsave("price_range_withinsession.png", p2)

#-- price_deviance
prcd44agg <- prc44[, (price = mean(price_dev_usd)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcd44agg, c("session","query", "price_dev_usd"))
prcd44agg[, query := paste("query", query)]
prcd44agg[, session := paste("session", session)]
prcd44tab <- prcd44agg %>% spread(query, price_dev_usd)

p2 <- ggplot(prcd44agg, aes(query, price_dev_usd, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning: Price Deviance") +
  labs(color='') 
ggsave("price_dev_usd_withinsession.png", p2)

#*-- try prc with all sessions, within-session learning: mean and range size ---

#-- price_mean
prcmagg <- prc[, (price = mean(price_mean)), 
               by = .(session, squery_order)][order(session, squery_order)]
setnames(prcmagg, c("session","query", "price_mean"))
prcmagg[, query := paste("query", query)]
prcmagg[, session := paste("session", session)]

p1 <- ggplot(prcmagg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning: Mean of Price Ranges") +
  labs(color='') 

#-- price_range
prcragg <- prc[, (price = mean(price_range)), 
               by = .(session, squery_order)][order(session, squery_order)]
setnames(prcragg, c("session","query", "price_range"))
prcragg[, query := paste("query", query)]
prcragg[, session := paste("session", session)]
prcrtab <- prcragg %>% spread(query, price_range)

p2 <- ggplot(prcragg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning: Price Range Sizes") +
  labs(color='') 
ggsave("price_range_withinsession.png", p2)


#*-- try grp, within-session learning: mean and range size ---------------------

#------ price_mean -------------------------------------------------------------
## total ##
prc44 <- prc[session < 5 & squery_order < 5,]
prcm44agg <- prc44[, (price = mean(price_mean)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcm44agg, c("session","query", "price_mean"))
prcm44agg[, query := paste("query", query)]
prcm44agg[, session := paste("session", session)]
prcm44tab <- prcm44agg %>% spread(query, price_mean)

p1 <- ggplot(prcm44agg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning: Mean of Price Ranges") +
  labs(color='') 

## group ##
setkey(prc, uid)
type <- usr[, .(uid, type)]
prc2 <- type[prc]
prc <- prc2
prc443 <- prc[session < 5 & squery_order < 5,]

#-- browsers

prcmagg <- prc443[type == "browser", (price = mean(price_mean)), 
               by = .(session, squery_order)][order(session, squery_order)]
setnames(prcmagg, c("session","query", "price_mean"))
prcmagg[, query := paste("query", query)]
prcmagg[, session := paste("session", session)]

p2 <- ggplot(prcmagg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning (Browsers): Mean of Price Ranges") +
  labs(color='') 

#-- explorers

prcmagg <- prc443[type == "explorer", (price = mean(price_mean)), 
                  by = .(session, squery_order)][order(session, squery_order)]
setnames(prcmagg, c("session","query", "price_mean"))
prcmagg[, query := paste("query", query)]
prcmagg[, session := paste("session", session)]

p3 <- ggplot(prcmagg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning (Explorers): Mean of Price Ranges") +
  labs(color='') 

#-- buyers

prcmagg <- prc443[type == "buyer", (price = mean(price_mean)), 
                  by = .(session, squery_order)][order(session, squery_order)]
setnames(prcmagg, c("session","query", "price_mean"))
prcmagg[, query := paste("query", query)]
prcmagg[, session := paste("session", session)]

p4 <- ggplot(prcmagg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning (Buyers): Mean of Price Ranges") +
  labs(color='') 

multiplot(p1, p2, p3, p4, cols = 2)

#------ price_range ------------------------------------------------------------

## total ##
prcr44agg <- prc44[, (price = mean(price_range)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcr44agg, c("session","query", "price_range"))
prcr44agg[, query := paste("query", query)]
prcr44agg[, session := paste("session", session)]
prcr44tab <- prcr44agg %>% spread(query, price_range)

p1 <- ggplot(prcr44agg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning: Price Range Sizes") +
  labs(color='') 

## group ##

#-- browsers
prcragg <- prc443[type == "browser", (price = mean(price_range)), 
                  by = .(session, squery_order)][order(session, squery_order)]
setnames(prcragg, c("session","query", "price_range"))
prcragg[, query := paste("query", query)]
prcragg[, session := paste("session", session)]
prcrtab <- prcragg %>% spread(query, price_range)

p2 <- ggplot(prcragg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning (Browsers): Price Range Sizes") +
  labs(color='') 

#-- explorers
prcragg <- prc443[type == "explorer", (price = mean(price_range)), 
                  by = .(session, squery_order)][order(session, squery_order)]
setnames(prcragg, c("session","query", "price_range"))
prcragg[, query := paste("query", query)]
prcragg[, session := paste("session", session)]
prcrtab <- prcragg %>% spread(query, price_range)

p3 <- ggplot(prcragg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning (Explorers): Price Range Sizes") +
  labs(color='') 

#-- buysers
prcragg <- prc443[type == "buyer", (price = mean(price_range)), 
               by = .(session, squery_order)][order(session, squery_order)]
setnames(prcragg, c("session","query", "price_range"))
prcragg[, query := paste("query", query)]
prcragg[, session := paste("session", session)]
prcrtab <- prcragg %>% spread(query, price_range)

p4 <- ggplot(prcragg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning (Buyers): Price Range Sizes") +
  labs(color='') 

multiplot(p1, p2, p3, p4, cols = 2)


#*-- price up-down -------------------------------------------------------------
## get mean, max, min for price_mean
pm1 <- prc[order == 1, .(index, uid, session_sep30,  price_mean)]
pm2 <- prc[, .(pmmax = max(price_mean), pmmin = min(price_mean)), by = index]
setkey(pm1, index)
pm3 <- pm1[pm2]
pm3[, pm1 := price_mean]
pm3[, price_mean := NULL]

## label price_mean
pm3[pm1 > pmmin & pm1 < pmmax, mtrend := "mixed"]
pm3[pm1 > pmmin & pm1 == pmmax, mtrend := "down"]
pm3[pm1 == pmmin & pm1 < pmmax, mtrend := "up"]
pm3[pm1 == pmmin & pm1 == pmmax, mtrend := "nochange"]

## get mean, max, min for price_range
pr1 <- prc[order == 1, .(index, uid, session_sep30,  price_range)]
pr2 <- prc[, .(prmax = max(price_range), prmin = min(price_range)), by = index]
setkey(pr1, index)
pr3 <- pr1[pr2]
pr3[, pr1 := price_range]
pr3[, price_range := NULL]

## label price_range
pr3[pr1 > prmin & pr1 < prmax, rtrend := "mixed"]
pr3[pr1 > prmin & pr1 == prmax, rtrend := "narrow"]
pr3[pr1 == prmin & pr1 < prmax, rtrend := "broaden"]
pr3[pr1 == prmin & pr1 == prmax, rtrend := "nochange"]

## merge
setkey(pm3, index)
price <- pm3[pr3]
price[, i.uid := NULL]
price[, i.session_sep30 := NULL]

## get click and book
setkey(price, uid)
price2 <- test3[price]
price3 <- price2[!is.na(uid)]


#*-- price and star ------------------------------------------------------------
ps <- sc[, .(uid, star, price)]
pss <- ps[, .N, by = star][order(-N)]
psp <- ps[, .N, by = price][order(-N)]
psps <- ps[, .N, by = .(star, price)]

p1 <- ggplot(pss[2:6], aes(x=star, y=N)) +
  geom_bar(stat='identity', fill = "#CF5B43") 


p2 <- ggplot(psp[2:6], aes(x=price, y=N)) +
  geom_bar(stat='identity', fill = "#CF5B43") 

ggsave("startop5.png", p1)
ggsave("pricetop5.png", p2)


#*-- priceset deviance(sd) plot 2x2 -----------------------------------------------

ggplot(usr, aes(priceset_sd)) + facet_grid(type ~ .) +
  ylab("percentage") +
  xlab("mean of price ranges") + 
  geom_bar(stat = "identity", fill = "#CF5B43") +
  theme(axis.text=element_text(size=8, family="PingFang SC")) +
  theme(legend.position="none") 

qplot(priceset_sd, ..density.. ,data = usr, 
      facets = type ~ ., 
      geom = "histogram",
      fill = "#CF5B43") +
  theme(legend.position="none") 

#*-- price start-to-purchase ---------------------------------------------------

## explore ##
summary(sc[, price_mean])  # mean 327.8
sd(sc[, price_mean], na.rm = TRUE)  # 287.0175, mean + 1stSD: 614.8175
ggplot(sc, aes(price_mean)) + geom_histogram(fill = "#CF5B43")

## prepare data ##
#-- get price-related data
#*** who set price ranges
price <- sc[price != "", .(uid, qid_new, session, query_order, squery_order, 
                           starttime,price_qry, price, price_min, price_max, 
                           price_range,price_mean, price_num, ifclick, 
                           ifbook)][order(uid, query_order)]
#** who purchased
bkuid <- data.table(unique(sc[ifbook == 1, uid]))  # 16169 users
setnames(bkuid, "V1", "uid")
setkey(price, uid)
#** who set price ranges and purchased
price_2 <- price[bkuid]
price_3 <- price_2[!is.na(price)]  ## 26180 queries have price, and finally purchased.
# check
length(unique(price_2[, uid]))  #16169 users
length(unique(price_3[, uid]))  #3535 users

#-- get start query
price2 <- price_3[, (qstart = min(query_order)), by = uid]
setnames(price2, "V1", "qstart")
setkey(price2, uid)
price3 <- price2[price_3]

#-- get price purchased
htlp <- dt[booking_bool == 1, .(uid, qid_new, fh_price)]
length(unique(htlp[, uid]))  # 16169 users
# get who set price ranges and purchased
puid <- data.table(unique(price_3[, uid]))
setkey(htlp, uid)
htlp2 <- htlp[puid]  # 3685 purchaes
length(unique(htlp2[, uid]))  # 3535 users

#-- merge, get price purchase and qstart
setkey(price3, qid_new)
price4 <- htlp2[price3, on = "qid_new"]

names(price)
price4[, uid := i.uid]
price4[, i.uid := NULL]

#-- merge2
price5 <- sqldf("
                select a.*, b.fh_price
                from price3 a 
                left outer join htlp2 b on (a.qid_new = b.qid_new)
                ")

#-- get csv file 
fwrite(price4, "pricebk.csv")


## analysis: purchase higher than max, lower than min ##
#-- startq and purchase
startq <- price4[qstart == query_order, .(uid, price_min, price_max)]  # 3535
purchase <- price4[ifbook == 1, .(uid, fh_price)]  # 2192
fp <- purchase[startq, on = "uid"]  # 3581
fp <- merge(startq, purchase, by.purchase = "uid", by.startq = "uid")  # 2192
fp2 <- fp[, .(price_l = min(price_min), 
      price_u = max(price_max), 
      pl = min(fh_price, na.rm = TRUE),
      pu = max(fh_price, na.rm = TRUE)), by = uid]
fp2[, test := pu - pl]
summary(fp2[, test])
fp3 <- fp2[, ":=" (high = ifelse(pu >= price_u, 1, 0),
        low = ifelse(pl <= price_l, 1, 0))]
sum(fp3[, high])  # 263
sum(fp3[, low])  # 132

# users: high and low
high <- data.table(fp3[high == 1, uid])
low <- data.table(fp3[low == 1, uid])
setnames(high, "V1", "uid")
setnames(low, "V1", "uid")
high[, type1 := "high"]
low[, type2 := "low"]
price5 <- high[price4, on = "uid"]
price5 <- low[price5, on = "uid"]
price5[, type := paste0(type1, type2)]
price5[, type := gsub("NA", "", type)]
price5[, type1 := NULL]
price5[, type2 := NULL]
price5 <- price5[order(uid, query_order)]

length(unique(purchase[, uid]))  # 2146
length(unique(fp[, uid]))  # 3535
length(unique(startq[, uid]))  # 3535

price_t <- sqldf("
                 select a.*, b.price_l, b.price_u, b.pl, b.pu, b.high, b.low
                 from price a
                 left outer join fp3 b on (a.uid = b.uid)
                 ")
names(fp3)
#*-- price start with high/wide ------------------------------------------------

## define high/wide
summary(price[, price_mean])
summary(price[, price_range])
summary(price[, price_min])
summary(price[, price_max])

type_size <- price5[qstart == query_order, .(uid, price_range)]
type_price <- price5[qstart == query_order, .(uid, price_min, price_max)]
type_size[, type_size := ifelse(price_range > 315, "wide", 
                                ifelse(price_range < 150, "narrow", "mid"))]
type_price[, type_price := ifelse(price_min > 300, "high", 
                                  ifelse(price_max < 234, "low", "mid"))]
length(unique(type_size[, uid]))
length(unique(type_price[, uid]))

## merge
setkey(type_size, uid)
setkey(type_price, uid)
price6 <- type_size[price5]
price6 <- type_price[price6]

price6[type == "", type := "within"]


#*-- plot above two ------------------------------------------------------------

prc44 <- price6[session < 5 & squery_order < 5 & type == "low"]
#-- price_mean
prcm44agg <- prc44[, (price = mean(price_mean)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcm44agg, c("session","query", "price_mean"))
prcm44agg[, query := paste("query", query)]
prcm44agg[, session := paste("session", session)]
prcm44tab <- prcm44agg %>% spread(query, price_mean)

p1 <- ggplot(prcm44agg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("mean price") +
  ggtitle("Within-Session Learning: Mean of Price Ranges") +
  labs(color='') 
ggsave("price_mean_withinsession.png", p1)


prc44 <- price6[session < 5 & squery_order < 5 & type_size == "narrow"]
prcr44agg <- prc44[, (price = mean(price_range)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcr44agg, c("session","query", "price_range"))
prcr44agg[, query := paste("query", query)]
prcr44agg[, session := paste("session", session)]
prcr44tab <- prcr44agg %>% spread(query, price_range)

p2 <- ggplot(prcr44agg, aes(query, price_range, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("price range size") +
  ggtitle("Within-Session Learning: Price Range Sizes") +
  labs(color='') 
ggsave("price_range_withinsession.png", p2)



## keyword =====================================================================
#*-- get key44, within-session learning ----------------------------------------

## total ##
key <- sc[, .(uid, session, squery_order, keyword_len)]
key[, key := ifelse(keyword_len == 0, 0, 1)]
key44 <- key[session < 5 & squery_order < 5,]

key44agg <- key44[squery_order != 2.5 & 
                    squery_order != 3.5 & 
                    squery_order != 4.5, .N, 
                   by = .(session, 
                          squery_order, key)][order(session, squery_order)]
key44agg2 <- cast(key44agg, session + squery_order ~ key)
setnames(key44agg2, c("session","query", "Nnokeyword", "NwithKeyword"))
key44agg2 <- data.table(key44agg2)
key44agg2[, Ntotal := Nnokeyword + NwithKeyword]
key44agg2[, percentage := NwithKeyword/Ntotal]


key44agg2[, query := paste("query", query)]
key44agg2[, session := paste("session", session)]

p1 <- ggplot(key44agg2, aes(query, percentage, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("NwithKeyword") +
  ggtitle("Within-Session Learning: Percentage of Consumers with Keyword") +
  labs(color='') 
#ggsave("Nwithkeyword_withinsession.png", p1)


## group ##

key <- sc[, .(uid, session, squery_order, keyword_len)]
key[, key := ifelse(keyword_len == 0, 0, 1)]

setkey(key, uid)
type <- usr[, .(uid, type)]
key <- type[key]
key443 <- key[session < 5 & squery_order < 5,]


#-- browers 
key44agg <- key443[squery_order != 2.5 & 
                    squery_order != 3.5 & 
                    squery_order != 4.5 &
                     type == "browser", .N,
                  by = .(session, 
                         squery_order, key)][order(session, squery_order)]
key44agg2 <- cast(key44agg, session + squery_order ~ key)
setnames(key44agg2, c("session","query", "Nnokeyword", "NwithKeyword"))
key44agg2 <- data.table(key44agg2)
key44agg2[, Ntotal := Nnokeyword + NwithKeyword]
key44agg2[, percentage := NwithKeyword/Ntotal]


key44agg2[, query := paste("query", query)]
key44agg2[, session := paste("session", session)]

p2 <- ggplot(key44agg2, aes(query, percentage, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("NwithKeyword") +
  ggtitle("Within-Session Learning (Browsers)") +
  labs(color='') 


#-- explorer 
key44agg <- key443[squery_order != 2.5 & 
                     squery_order != 3.5 & 
                     squery_order != 4.5 &
                     type == "browser", .N,
                   by = .(session, 
                          squery_order, key)][order(session, squery_order)]
key44agg2 <- cast(key44agg, session + squery_order ~ key)
setnames(key44agg2, c("session","query", "Nnokeyword", "NwithKeyword"))
key44agg2 <- data.table(key44agg2)
key44agg2[, Ntotal := Nnokeyword + NwithKeyword]
key44agg2[, percentage := NwithKeyword/Ntotal]


key44agg2[, query := paste("query", query)]
key44agg2[, session := paste("session", session)]

p3 <- ggplot(key44agg2, aes(query, percentage, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("NwithKeyword") +
  ggtitle("Within-Session Learning (Explorers)") +
  labs(color='') 


#-- buyers 
key44agg <- key443[squery_order != 2.5 & 
                     squery_order != 3.5 & 
                     squery_order != 4.5 &
                     type == "buyer", .N,
                   by = .(session, 
                          squery_order, key)][order(session, squery_order)]
key44agg2 <- cast(key44agg, session + squery_order ~ key)
setnames(key44agg2, c("session","query", "Nnokeyword", "NwithKeyword"))
key44agg2 <- data.table(key44agg2)
key44agg2[, Ntotal := Nnokeyword + NwithKeyword]
key44agg2[, percentage := NwithKeyword/Ntotal]


key44agg2[, query := paste("query", query)]
key44agg2[, session := paste("session", session)]

p4 <- ggplot(key44agg2, aes(query, percentage, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("NwithKeyword") +
  ggtitle("Within-Session Learning (Buyers)") +
  labs(color='') 


multiplot(p1, p2, p3, p4, cols = 2)

## groups ======================================================================

#*-- os ------------------------------------------------------------------------

usr2 <- usr[, .(os, advancedays_type, datenum, sessionnum, querynum, criterianum, ifclick, 
                ifbook, priceset_usd)]
setnames(usr2, c("os", "advance_type", "Ndays", "Nsessions","Nqueries", "Ncriteria", 
                 "Click", "Conversion", "PriceSet_USD"))
names(usr2)
stargazer(usr2, type = "text")

## distribution ##
osp1 <- ggplot(usr2[os == "Android"], aes(Ndays, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlab("Ndays") +
  ylab("percentage")
osp2 <- ggplot(usr2[os == "iOS"], aes(Ndays, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlab("Ndays") +
  ylab("percentage")
multiplot(osp1, osp2, cols=2)
p2 <- ggplot(usr, aes(sessionnum, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(0, 10) + 
  xlab("Nsessions") +
  ylab("percentage")
p3 <- ggplot(usr, aes(querynum, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(0, 20) + 
  xlab("Nqueries") +
  ylab("percentage")
p4 <- ggplot(usr, aes(criterianum, ..density..)) + 
  geom_histogram(fill = "#CF5B43", binwidth = 1) +
  xlim(-1, 30) + 
  xlab("Ncriteria") +
  ylab("percentage")
multiplot(p1, p2, p3, p4, cols=2)

sc[1:100, distr]


#*-- BEB -----------------------------------------------------------------------
fct <- function(var){
  # get unique var list
  test <<- unique(sc[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click")
              )][order(-sbook)]
  test3 <<- test2[, c(1, 4:5)]
  setkey(test3, uid)
  # merge
  test4 <<- test[test3]
  # aggregate
  test5 <<- test4[V2 != "" & !is.na(V2), .N, by = .(V2, ibook, iclick)][order(-N)]
  test5 <- test5[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "not click", N])]
  test5 <- test5[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "click", N])]
  test5 <<- test5[ibook == "book" & iclick == "click", 
                  N2 := N/sum(test5[ibook == "book" & iclick == "click", N])]
  test6 <<- test5[, V2 := as.numeric(V2)]
  p <<- ggplot(test6, aes(V2, N2)) + facet_grid(ibook ~ iclick) +
        xlim(0, 600) +
    ylab("percentage") +
    xlab("mean of price ranges") + 
    geom_bar(stat = "identity", fill = "#CF5B43") +
    theme(axis.text=element_text(size=8, family="PingFang SC")) +
    theme(legend.position="none") 
  p
}

fct("advancedays")
sum(test6[,N2])
ggsave("grp_price_mean.png", p)
names(usr)

#*-- price deviance ------------------------------------------------------------

## total and advancedays
p1 <- ggplot(usr, aes(price_dev_usd, ..density..)) + 
  geom_histogram(fill = "#CF5B43") +
  geom_vline(xintercept = 0, colour = "darkblue") +
  xlab("price deviance") +
  ylab("percentage") +
  xlim(-200, 200) + ylim(0, 0.03) +
  ggtitle("Total: Price Deviance")
p2 <- ggplot(usr[advancedays_type == "book the same day"], 
             aes(price_dev_usd, ..density..)) + 
  geom_histogram(fill = "#CF5B43") +
  geom_vline(xintercept = 0, colour = "darkblue") +
  xlab("price deviance") +
  ylab("percentage") +
  xlim(-200, 200) + ylim(0, 0.03) +
  ggtitle("Book at the Same Day: Price Deviance")
p3 <- ggplot(usr[advancedays_type == "book within 1 week"], 
             aes(price_dev_usd, ..density..)) + 
  geom_histogram(fill = "#CF5B43") +
  geom_vline(xintercept = 0, colour = "darkblue") +
  xlab("price deviance") +
  ylab("percentage") +
  xlim(-200, 200) + ylim(0, 0.03) +
  ggtitle("Book Within 1 Week: Price Deviance")
p4 <- ggplot(usr[advancedays_type == "book earlier than 1 week"], 
             aes(price_dev_usd, ..density..)) + 
  geom_histogram(fill = "#CF5B43") +
  geom_vline(xintercept = 0, colour = "darkblue") +
  xlab("price deviance") +
  ylab("percentage") +
  xlim(-200, 200) + ylim(0, 0.03) +
  ggtitle("Book Earlier than 1 Week: Price Deviance")
multiplot(p1, p2, p3, p4, cols=2)


## geom_density
p1 <- ggplot(usr, aes(price_dev_usd, ..density..)) + 
  geom_density(aes(colour = datenum_type)) +
  geom_vline(xintercept = 0, colour = "darkblue") +
  xlab("price deviance") +
  xlim(-200, 200) + ylim(0, 0.03)
ggsave("grp_price_dev_usd_adv.png", p1)


## below and above 0
#-- Android
dev <- usr[os == "Android", .(uid, price_dev_usd, os, advancedays_type, 
               criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_os <- dev[!is.na(price_dev_usd), .N, by = .(direction, os)]
devagg_os[, percentage := N/sum(N)]
devagg_os1 <- cast(devagg_os, os ~ direction )
names(devagg_os1)[1] <- "type"
#-- iOS
dev <- usr[os == "iOS", .(uid, price_dev_usd, os, advancedays_type, 
                              criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_os <- dev[!is.na(price_dev_usd), .N, by = .(direction, os)]
devagg_os[, percentage := N/sum(N)]
devagg_os2 <- cast(devagg_os, os ~ direction )
names(devagg_os2)[1] <- "type"
#-- book the same day
dev <- usr[advancedays_type == "book the same day", 
           .(uid, price_dev_usd, os, advancedays_type, 
                              criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd> 0, "greater than price searched",
ifelse(price_dev_usd == 0, "equal to price searched",
"less than price searched"))]
devagg_adv <- dev[!is.na(price_dev_usd), .N, by = .(direction, advancedays_type)]
devagg_adv[, percentage := N/sum(N)]
devagg_adv1 <- cast(devagg_adv, advancedays_type ~ direction )
names(devagg_adv1)[1] <- "type"
#-- book within 1 week
dev <- usr[advancedays_type == "book within 1 week", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_adv <- dev[!is.na(price_dev_usd), .N, by = .(direction, advancedays_type)]
devagg_adv[, percentage := N/sum(N)]
devagg_adv2 <- cast(devagg_adv, advancedays_type ~ direction )
names(devagg_adv2)[1] <- "type"
#-- book earlier than 1 week
dev <- usr[advancedays_type == "book earlier than 1 week", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_adv <- dev[!is.na(price_dev_usd), .N, by = .(direction, advancedays_type)]
devagg_adv[, percentage := N/sum(N)]
devagg_adv3 <- cast(devagg_adv, advancedays_type ~ direction )
names(devagg_adv3)[1] <- "type"
#-- datenum_type :1 day
dev <- usr[datenum_type == "1 day", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_dt <- dev[!is.na(price_dev_usd), .N, by = .(direction, datenum_type)]
devagg_dt[, percentage := N/sum(N)]
devagg_dt1 <- cast(devagg_dt, datenum_type ~ direction )
names(devagg_dt1)[1] <- "type"
#-- datenum_type :2-3 days
dev <- usr[datenum_type == "2-3 days", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_dt <- dev[!is.na(price_dev_usd), .N, by = .(direction, datenum_type)]
devagg_dt[, percentage := N/sum(N)]
devagg_dt2 <- cast(devagg_dt, datenum_type ~ direction )
names(devagg_dt2)[1] <- "type"
#-- datenum_type :more than 3 days
dev <- usr[datenum_type == "more than 3 days", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_dt <- dev[!is.na(price_dev_usd), .N, by = .(direction, datenum_type)]
devagg_dt[, percentage := N/sum(N)]
devagg_dt3 <- cast(devagg_dt, datenum_type ~ direction )
names(devagg_dt3)[1] <- "type"

usr[, criterianum_type := ifelse(criterianum == 0, "no criteria", 
                                 ifelse(criterianum > 0 & criterianum < 3, "1-2 criteria",
                                        "more than 2 criteria"))]
#-- criterianum_type: 1-2 criteria
dev <- usr[criterianum_type == "1-2 criteria", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_cr <- dev[!is.na(price_dev_usd), .N, by = .(direction, criterianum_type)]
devagg_cr[, percentage := N/sum(N)]
devagg_cr1 <- cast(devagg_cr, criterianum_type ~ direction)
names(devagg_cr1)[1] <- "type"
#-- criterianum_type: more than 2 criteria
dev <- usr[criterianum_type == "more than 2 criteria", 
           .(uid, price_dev_usd, os, advancedays_type, 
             criterianum_type, datenum_type)]
dev[, direction := ifelse(price_dev_usd > 0, "greater than price searched",
                          ifelse(price_dev_usd == 0, "equal to price searched",
                                 "less than price searched"))]
devagg_cr <- dev[!is.na(price_dev_usd), .N, by = .(direction, criterianum_type)]
devagg_cr[, percentage := N/sum(N)]
devagg_cr2 <- cast(devagg_cr, criterianum_type ~ direction)
names(devagg_cr2)[1] <- "type"

devgg <- rbind(devagg_os1[, c(1,3,4)], 
               devagg_os2[, c(1,3,4)], 
               devagg_adv1[, c(1,3,4)], 
               devagg_adv2[, c(1,3,4)], 
               devagg_adv3,
               devagg_dt1[, c(1,3,4)],
               devagg_dt2[, c(1,3,4)],
               devagg_dt3,
               devagg_cr1[, c(1,3,4)],
               devagg_cr2[, c(1,3,4)])



## cor: deviance and advancedays

advdev <- usr[, (price_dev_usd = mean(price_dev_usd, rm.na = TRUE)), by = advancedays]
setnames(advdev, "V1", "price_dev_usd")
ggplot(advdev, aes(advancedays, price_dev_usd)) + geom_smooth()


datedev <- usr[, (price_dev_usd = mean(price_dev_usd, rm.na = TRUE)), by = sessionnum]
setnames(datedev, "V1", "price_dev_usd")
ggplot(datedev, aes(datenum, price_dev_usd)) + geom_smooth()

## save ========================================================================
fwrite(sc, "search_clean.csv")
fwrite(usr, "user.csv")

