## header ======================================================================
# purpose: for first-year, price and star learning
# author: Zoey Hu
# create date: 05/10/2018 
# last update: //2018 
#
# notation:
#          - *: things to be consider while proceed to code; pitfalls
#          - %: techniques


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


## 1. first price range ##

## 1.1 identify query with first price range ##
# %.(qstart = ...) with the dot in front, no need to setnames.
price2 <- price[, .(qstart = min(query_order)), by = uid]
setkey(price2, uid)
# price left outer join price2
price <- price2[price]
rm(price2)

## 1.2 get first price range user table ##
# 60413 users, solid
first_range <- price[qstart == query_order, .(uid, price, price_min, price_max, 
                                              price_mean, price_range)]



## 2. label whether booking price higher or lower than price criteria ## 

## 2.1 get booking price ##

# include all queries for they might not book in the query with price criteria
# 16791 orders
# check: length(unique(book.price[, uid]))  # 16169 users, solid
book_price <- dt[booking_bool == 1, .(uid, fh_price)][order(uid)]

## 2.2 get book_low, book_high
# % add multiple variables
# book.price[, ":=" (book_low = min(fh_price), 
#                   book_high = max(fh_price)), by = uid]
# % get only new values
book_price <- book_price[, .(book_low = min(fh_price), 
                             book_high = max(fh_price)), by = uid]

## 2.3 match booking price and first price range, label users ##
# check length(unique(first.range.test[!is.na(book_low), uid])) : 3535
first_range <- book_price[first_range, on = "uid"]
first_range[, type_pp := ifelse(book_low < price_min, "low", 
                           ifelse(book_high > price_max, "high",
                                  ifelse(!is.na(book_low), "mid", "no")))]
# check type_pp: first_range[, .N, by = pp]



## 3. label starting with high/low/mid price criteria ##
# label: if > 3Q then high, <1Q then low, else mid
summary(price[, .(price_mean, price_min, price_max, price_range)])
first_range[, type_price := ifelse(price_max > 550, "high",
                                   ifelse(price_min < 150, "low", "mid"))]
# check type_price: first_range[, .N, by = type_price]
first_range[, type_psize := ifelse(price_range > 315, "high",
                                   ifelse(price_range < 150, "low", "mid"))]
# check type_psize: first_range[, .N, by = type_psize]



## 4. combine user label table with price table
setkey(first_range, uid)
price <- first_range[, .(uid, book_high, book_low, 
                              type_pp, type_price, type_psize)][price]


## 5. analysis and plot




## star ========================================================================