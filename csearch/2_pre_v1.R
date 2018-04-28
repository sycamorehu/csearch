## header ======================================================================
# purpose: for first-year prensentation, focus on price
# author: Zoey Hu
# create date: 04/25/2018 
# last update: //2018 
#
# input file: search_clean.csv
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
#sc <- fread("/home/zhiying/Dropbox\ (GaTech)/csearch/project/csearch/search_clean.csv")
dt <- fread("/Users/zyhu/Dropbox (GaTech)/csearch/
            data/raw/tmp_jinyongliu_dxj_query_1.csv")
load("2prev1.RData")

## sample and explore ==========================================================

user <- unique(dt[, uid])
setkey(dt, uid)
spdt <- dt[user[sample(100)]]
fwrite(spdt, "sample_dt.csv")

## not every qid has complete rank_in_qid
sp1dt <- dt[uid == "405996808fe0a07160376bfea7befe7b"]
sp1sc <- sc[uid == "405996808fe0a07160376bfea7befe7b"]

## price: what users set in dataset and what available on App is different
aaprice <- setorder(sc[, .N, by = price], -N)
sp2dt <- dt[price_qry %like% "254以下"]
sp2dt2 <- dt[price_qry %like% "￥300以下￥1050以上￥200以下"]
# get csv file
aaprice_qry <- setorder(sc[, .N, by = price_qry], -N)
fwrite(aaprice_qry, "aaprice_qry.csv")
# how many price ranges in one query
aaprice_num <- setorder(sc[, .N, by = price_num], -N)
sum(aaprice_num[price_num > 1,N])/sum(aaprice_num[, N])  # 3548, 0.19%
#-- by user
aaprice_num_uid <- unique(sc[, .(uid, price_num)])[, .N, 
                                                   by = price_num][order(-N)]
sum(aaprice_num_uid[price_num > 1,N])/sum(aaprice_num_uid[, N])  # 1511, 0.3357%
#-- by user and queries
aaprice_num_uidqid <- (sc[price_num > 1, .(uid, qid_new)])[, .N, 
                                                   by = uid][order(-N)]

sum(aaprice_num_uidqid[N == 1, N])  # 583 users
    
## sort
aasort_qry <- setorder(sc[, .N, by = sort_qry], -N)
names(sc)



## distribution ================================================================

#*-- histogram: datenum, sessionnum, querynum ----------------------------------
p <- ggplot(usr, aes(pricenum)) + 
  xlim(-1, 10) + 
  xlab("number of price ranges")
g <- p + geom_histogram(aes(pricenum, ..density..),
                   binwidth = 1, 
                   fill = "#CF5B43")
g
ggsave("pricenum_bar.png", g)


#*-- plot 2x2 general-----------------------------------------------------------
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
#    xlim(0, 1000) +
    ylab("percentage") +
    xlab("mean of price ranges") + 
    geom_bar(stat = "identity", fill = "#CF5B43") +
    theme(axis.text=element_text(size=8, family="PingFang SC")) +
    theme(legend.position="none") 
  p
}

fct("price_mean")
sum(test6[,N2])
ggsave("grp_price_mean.png", p)
names(usr)

#*-- plot 2x2 for price mean, range --------------------------------------------

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
  test4 <<- test[test3][V2 != ""]
  # aggregate
  test5 <<- test4[V2 != "" & !is.na(V2), .N, by = .(V2, ibook, iclick)][order(-N)]
  test5 <- test5[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "not click", N])]
  test5 <- test5[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "click", N])]
  test5 <<- test5[ibook == "book" & iclick == "click", 
                  N2 := N/sum(test5[ibook == "book" & iclick == "click", N])]
  test6 <<- test5[, V2 := as.numeric(V2)]
  p <<- qplot(as.numeric(V2), ..density.., data = test4, 
              facets = ibook ~ iclick,
              geom = "histogram",
              binwidth = 100,
              fill = "#CF5B43") +
    #    xlim(0, 1000) +
    #ylim(0, 0.005) +
    ylab("density") +
    xlab("mean price") + 
    # geom_histogram(stat = "identity", fill = "#CF5B43") +
    theme(axis.text=element_text(size=8, family="PingFang SC")) +
    theme(legend.position="none") + 
    scale_x_continuous(limits=c(0, 1000), breaks=c(0, 200, 400, 600, 800, 1000))
  p
}

fct("price_mean")
sum(test6[,N2])
ggsave("grp_price_mean.png", p)
names(usr)



#*-- explore sequence ----------------------------------------------------------
names(sc)
aa <- sc[1:50000, .(uid, query_order, price_range)][order(uid, query_order)]
bb <- unique(aa[, .(uid, price_range)][, .N, by = uid])[N != 1]
setkey(aa, uid)
cc <- aa[bb]
View(aa)

p <- ggplot(cc, aes(query_order, as.numeric(price_range), group = uid)) + 
  geom_line(alpha = I(0.1)) +
  geom_smooth(aes(group = 1), size = 2, se = T) + 
  xlim(0, 20) + 
  scale_y_continuous()
p


#*-- slice up ------------------------------------------------------------------
s1 <- setkey(sc[query_order == 1, .(uid, price_range, price_mean, 
                             price_min, price_max)], uid)[test3]
s2 <- setkey(sc[query_order == 2, .(uid, price_range, price_mean, 
                             price_min, price_max)], uid)[test3]
s3 <- setkey(sc[query_order == 3, .(uid, price_range, price_mean, 
                             price_min, price_max)], uid)[test3]
s4 <- setkey(sc[query_order == 4, .(uid, price_range, price_mean, 
                             price_min, price_max)], uid)[test3]
p1 <- qplot(as.numeric(price_range), ..density.., data = s1, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick)) + 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 1") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_q1.png", p1)
p2 <- qplot(as.numeric(price_range), ..density..,  data = s2, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 2") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_q2.png", p2)
p3 <- qplot(as.numeric(price_range),  ..density.., data = s3, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 3") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_q3.png", p3)
p4 <- qplot(as.numeric(price_range),  ..density.., data = s4, 
      geom = "histogram", 
      fill = "#CF5B43",
      binwidth = 30,
facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 4") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_q4.png", p4)
multiplot(p1, p2, p3, p4, cols=2)


#*-- wihin session learning ----------------------------------------------------
ss1 <- setkey(sc[session_sep30 == 1 & squery_order == 1, 
                .(uid, price_range, price_mean, 
                                    price_min, price_max)], uid)[test3]
ss2 <- setkey(sc[session_sep30 == 1 & squery_order == 2, 
                 .(uid, price_range, price_mean, 
                                    price_min, price_max)], uid)[test3]
ss3 <- setkey(sc[session_sep30 == 1 & squery_order == 3, 
                 .(uid, price_range, price_mean, 
                                    price_min, price_max)], uid)[test3]
ss4 <- setkey(sc[session_sep30 == 1 & squery_order == 4, 
                 .(uid, price_range, price_mean, 
                                    price_min, price_max)], uid)[test3]


pp1 <- qplot(as.numeric(price_range), ..density.., data = ss1, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick)) + 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 1") +
  ggtitle("Price Span within Session 1: Query 1") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_s1q1.png", pp1)
pp2 <- qplot(as.numeric(price_range), ..density..,  data = ss2, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 2") +
  ggtitle("Price Span within Session 1: Query 2") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_s1q2.png", pp2)
pp3 <- qplot(as.numeric(price_range),  ..density.., data = ss3, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 3") +
  ggtitle("Price Span within Session 1: Query 3") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_s1q3.png", pp3)
pp4 <- qplot(as.numeric(price_range),  ..density.., data = ss4, 
            geom = "histogram", 
            fill = "#CF5B43",
            binwidth = 30,
            facets = (ibook ~ iclick))+ 
  ylim(0, 0.0075) + 
  xlab("price range sizes of query 4") +
  ggtitle("Price Span within Session 1: Query 4") +
  scale_x_continuous(limits = c(0, 500), breaks = c(0, 100, 200, 
                                                    300, 400, 500)) + 
  theme(legend.position = "none")
ggsave("grp_price_range_s1q4.png", pp4)
multiplot(p1, p2, p3, p4, cols=2)


#*-- price up and down ---------------------------------------------------------
#*== to do (update price range) =====

## get price table
prc <- sc[price != '', .(uid, qid_new, query_order, session_sep30, 
                  price, price_min, price_max, 
                  price_range, price_mean)][order(uid, query_order)]
prc[, order := frank(query_order), by = list(uid, session_sep30)]
prc[, index := paste0(uid, session_sep30)]

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


pricer <- price3[, .N, by = .(rtrend, ibook, iclick)][order(-N)]

## plot
#-- price_mean, session 1
pricem <- price3[session_sep30 == 1, .N, 
                 by = .(mtrend, ibook, iclick)][order(-N)]
pricem <- pricem[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(pricem[ibook == "not book" & iclick == "not click", N])]
pricem <- pricem[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(pricem[ibook == "not book" & iclick == "click", N])]
pricem <- pricem[ibook == "book" & iclick == "click", 
                  N2 := N/sum(pricem[ibook == "book" & iclick == "click", N])]
pms1 <- ggplot(pricem, aes(mtrend, N2)) +
  geom_bar(stat = "identity", fill = "#CF5B43") +
  facet_grid(ibook ~ iclick) +
  ggtitle("Session 1: change of mean price") +
  xlab("change of mean price") +
  ylab("percentage")
sum(pricem[, N2])
ggsave("grp_price_mean_trend_s1.png", pms1)

#-- price_mean, session 2
pricem <- price3[session_sep30 == 2, .N, 
                 by = .(mtrend, ibook, iclick)][order(-N)]
pricem <- pricem[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(pricem[ibook == "not book" & iclick == "not click", N])]
pricem <- pricem[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(pricem[ibook == "not book" & iclick == "click", N])]
pricem <- pricem[ibook == "book" & iclick == "click", 
                  N2 := N/sum(pricem[ibook == "book" & iclick == "click", N])]
pms2 <- ggplot(pricem, aes(mtrend, N2)) +
  geom_bar(stat = "identity", fill = "#CF5B43") +
  facet_grid(ibook ~ iclick) +
  ggtitle("Session 2: change of mean price") +
  xlab("change of mean price") +
  ylab("percentage")
sum(pricem[, N2])
ggsave("grp_price_mean_trend_s2.png", pms2)



#-- price_range, session 1
pricer <- price3[session_sep30 == 1, .N, 
                 by = .(rtrend, ibook, iclick)][order(-N)]
pricer <- pricer[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(pricer[ibook == "not book" & iclick == "not click", N])]
pricer <- pricer[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(pricer[ibook == "not book" & iclick == "click", N])]
pricer <- pricer[ibook == "book" & iclick == "click", 
                  N2 := N/sum(pricer[ibook == "book" & iclick == "click", N])]
prs1 <- ggplot(pricer, aes(rtrend, N2)) +
  geom_bar(stat = "identity", fill = "#CF5B43") +
  facet_grid(ibook ~ iclick) +
  ggtitle("Session 1: change of price range span") +
  xlab("change of price range span") +
  ylab("percentage")
sum(pricer[, N2])
ggsave("grp_price_range_trend_s1.png", prs1)

#-- price_range, session 2
pricer <- price3[session_sep30 == 2, .N, 
                 by = .(rtrend, ibook, iclick)][order(-N)]
pricer <- pricer[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(pricer[ibook == "not book" & iclick == "not click", N])]
pricer <- pricer[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(pricer[ibook == "not book" & iclick == "click", N])]
pricer <- pricer[ibook == "book" & iclick == "click", 
                  N2 := N/sum(pricer[ibook == "book" & iclick == "click", N])]
prs2 <- ggplot(pricer, aes(rtrend, N2)) +
  geom_bar(stat = "identity", fill = "#CF5B43") +
  facet_grid(ibook ~ iclick) +
  ggtitle("Session 2: change of price range span") +
  xlab("change of price range span") +
  ylab("percentage")
sum(pricer[, N2])
ggsave("grp_price_range_trend_s2.png", prs2)



#*-- get and plot deviance -----------------------------------------------------
pr4 <- prc[, .(psd = sd(as.numeric(price_mean))), by = list(uid)]
pr5 <- test3[pr4]
pr5$psd[is.na(pr5$psd)] <- 0
p <- ggplot(pr5, aes(psd, ..density..)) + 
  geom_histogram(fill = "#CF5B43") + 
  facet_grid(ibook ~ iclick)+
  xlim(0, 600) +
  ylim(0, 0.01) +
  xlab("standard deviation of price")
ggsave("grp_sd_price_mean.png", p)

## save ========================================================================
save.image("2prev1.RData")
fwrite(sc, "search_clean.csv")
