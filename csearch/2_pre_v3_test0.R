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
