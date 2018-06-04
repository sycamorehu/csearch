## header ======================================================================
# purpose: to test "2_pre_v3"
# author: Zoey Hu
# create date: 05/11/2018 
# last update: //2018 
#
# - users with price criteria and purchase
# ---- solution: if get data from dt or sc with price_qry != "-999" & 
# ---- price_qry != "不限" & booking_bool == 1, then only queries that satisfy
# ---- both set price criteria and purchased will be obtained. However, our 
# ---- intention is to get users who have once set price criteria and purchase,
# ---- not neccesarily those purchase on the pirce-criteria query.
#
# - plots
# - check individual of type_pp
# - try to get purchase prices on precise queries, but they might not purchase
# ---- on those queries. It makes no sense doing that. 

## price =======================================================================

## 2.3 match booking price and first price range, label users ##

#*-- explore -------------------------------------------------------------------
# users with price criteria: 60413
test_1 <- data.table(dt[price_qry != "-999" & price_qry != "不限", uid])
length(unique(test.1[, V1]))

# users with purchase: 16791
test_2 <- data.table(dt[booking_bool == 1, uid])
length(unique(test.2[, V1]))

# users with price criteria and purchase: 2192
test_3 <- data.table(dt[price_qry != "-999" & 
                          price_qry != "不限" &
                          booking_bool == 1, uid])


#*-- get data and exam ---------------------------------------------------------
# check length(unique(test_dt[, uid])): 2146 users, 2192 queries
test_dt <- dt[price_qry != "-999" & 
              price_qry != "不限" &
              booking_bool == 1, .(uid, price_qry, fh_price)]
# check length(unique(test_sc[, uid])): 2146 users, 2192 queries
test_sc <- sc[ifbook == 1 & price != "", .(uid, price, price_min, price_max)]
# check length(unique(test_sc_2[, uid])): 2146 users, 2192 queries
test_sc_2 <- sc[ifbook == 1 & price_qry != "-999" & price_qry != "不限",
                .(uid, price, price_min, price_max)]
# check length(unique(test_combine[!is.na(fh_price), uid])): 3535 users
test_combine <- book_price[first_range, on = "uid"]
# check length(unique(test_combine_2[!is.na(fh_price), uid])): 3535 users
test_combine_2 <- data.table(sqldf("
                        select a.*, b.fh_price
                        from first_range a
                        inner join book_price b on (a.uid = b.uid)
                        "))
# check length(unique(test_combine_3[!is.na(fh_price), uid])): 3535 users
test_combine_3 <- merge(book_price, first_range, by = "uid") 

# download files
fwrite(test_dt, "test_dt.csv")
fwrite(test_sc, "test_sc.csv")
fwrite(test_combine, "test_combine.csv")

# test difference
dt[uid == "001b72cdececf5ffc0e09df95c908770"]

# solution: if get data from dt or sc with price_qry != "-999" & 
# price_qry != "不限" & booking_bool == 1, then only queries that satisfy
# both set price criteria and purchased will be obtained. However, our 
# intention is to get users who have once set price criteria and purchase,
# not neccesarily those purchase on the pirce-criteria query.




#*-- plot ----------------------------------------------------------------------
## 1 type_pp ##

## 1.1 4x4 queries ## 
## 1.1.1 book_high ## 
prc44 <- price[type_pp == "high" & session < 5 & squery_order < 5,]
prcm44high <- prc44[, (price = mean(book_high)), 
                    by = .(session, squery_order)][order(session, squery_order)]
setnames(prcm44high, c("session","query", "price_mean"))
prcm44high[, query := paste("query", query)]
prcm44high[, session := paste("session", session)]

ggplot(prcm44high, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("Average booking price") +
  ggtitle("Within-Session Learning: Book Price > max(Price Criteria)") +
  labs(color='') 

## 1.1.2 price_mean ##
prc44 <- price[type_pp == "low" & session < 5 & squery_order < 10,]
prcm44agg <- prc44[, (price = mean(price_mean)), 
                   by = .(session, squery_order)][order(session, squery_order)]
setnames(prcm44agg, c("session","query", "price_mean"))
prcm44agg[, query := paste("query", query)]
prcm44agg[, session := paste("session", session)]

ggplot(prcm44agg, aes(query, price_mean, group = session)) + 
  geom_line(aes(colour = factor(session))) +
  xlab("Query") + ylab("Average price") +
  ggtitle("Within-Session Learning: high") +
  labs(color='') 

ggsave("price_mean_withinsession.png", p1)


## 1.2 mean, min, max, with range sizes ##
prc44 <- price[type_pp == "mid" & session < 5 & squery_order < 11]
prcm44agg <- prc44[, .(price = mean(price_mean), 
                       price_min = mean(price_min),
                       price_max = mean(price_max),
                       price_range = mean(price_range)), 
                   by = .(squery_order)][order(squery_order)]
setnames(prcm44agg, c("query", "price_mean", "price_min", "price_max", "price_range"))

ggplot(prcm44agg) + 
  geom_line(aes(query, price_mean), colour = "orange") + 
  geom_line(aes(query, price_min), colour = "green") +
  geom_line(aes(query, price_max), colour = "red") +
  geom_point(aes(query, price_mean, size = price_range), colour = "orange") +
  scale_size(range = c(0, 8)) +
  xlab("Query") + ylab("average price") + 
  ggtitle("Within-Session Learning: mid") +
  labs(color='') 


#*-- check individual of type_pp -----------------------------------------------
indiv <- price[!is.na(book_price) &
               type_pp == "higher"][, .(uid, query_order, 
                                       book_price, price, price_mean, 
                                       type_pp)][order(uid, session, squery)]

# distribution of number of every query
dist_query <- indiv[, .N, by = query_order][order(query_order)]


# uid = 1aeb2dc1bb1809ed3b9fd8e1fedd502a
checkuid <- sc[uid == "1aeb2dc1bb1809ed3b9fd8e1fedd502a", ][order(starttime)]
checkuid <- sc[uid == "20165ad6d83196380226bdadfbb55db2", ][order(starttime)]



#*-- get prices on the precise queries -----------------------------------------

## 4. get prices on the precise queries ##
# book_price2 <- dt[booking_bool == 1, .(qid_new, fh_price)]
# ! discarded


## 6. combine price table with purchase prices ##
# join by qid_new
# ! discarded: consumers may not purchase on queries with price ranges.
# setkey(book_price2, qid_new)
# price_test <- book_price2[price]
# rm(price_test)
