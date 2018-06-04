## header ======================================================================
# purpose: to test "2_pre_v0.R"
# author: Zoey Hu
# create date: 04/15/2018 
# last update: 04/25/2018 
#
# - check dft[!b, on = "name1", ...]
# - test plot
# - test bar top 10 ggplot
# - test facets and crs




## usage of criteria ===========================================================

name1 <- c("Bob","Mary","Jane","Kim")
name2 <- c("Bob","Mary","Kim","Jane")
weight <- c(60,65,45,55)
height <- c(170,165,140,135)
birth <- c("1990-1","1980-2","1995-5","1996-4")
accept <- c("no","ok","ok","no")
library(data.table)
dft <- data.table(name1,weight,height,accept)

dft <- rbind(dft, dft[1,])
b <- c("Mary", "Kim")
c <- setorder(dft[!b, on = "name1", .N, by = name1], -N)

# assign(paste("N", i, sep = ""), setorder(sc[!na, on = i, .N, by = uid], -N))




## test plot =======================================================================

## sort
qplot(sort_qry, data = sc[sort_qry != 0 
                          & sort_qry != 6], 
      facets = . ~ ifbook,
      geom = "bar", xlim = c(0, 10))


## price range
qplot(price_range, data = sc[price_range != "NA"],
      facets = . ~ ifbook,
      geom = "density")

## filter score
qplot(filter_quantity, data = sc[filter_quantity != "-999"], 
      facets = . ~ ifclick,
      geom = "bar")




## test top 10 plotting ========================================================

qplot(sort_qry, N, data = asort_qry, xlim = c(0, 10))

price_top10 <- (aaprice %>% filter(aaprice$price != "-999"))[1:10,1]
top10 <- (aaprice %>% filter(aaprice$price != "-999"))[1:10,]
qplot(price, data = sc[price %in% price_top10], geom = "bar") + coord_flip()
ggplot(data=top10, aes(x = reorder(top10$price, top10$N), y =top10$N)) + 
  geom_bar(stat = "identity", aes(fill = factor(top10$price))) + 
  coord_flip() + theme(legend.position="none")

colnames(sc)


## test facets and cr ==========================================================

fct <- function(var){
  # get unique var list
  test <- unique(sc[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click"))][order(-sbook)]
  test3 <- test2[, c(1, 4:5)]
  setkey(test3, uid)
  # merge
  test4 <- test[test3]
  # aggregate
  test5 <<- test4[, .N, by = .(V2, ibook, iclick)]
  test8 <- test5[V2 != ""]
  # plot
  p <<- qplot(V2, N, data = test8, facets = ibook ~ iclick,
        ylab = "frequency", xlab = var) + coord_flip()
  p
}

fct("star")
names(sc)

cr <- function(var){
    # get unique var list
    test <- unique(sc[, .(uid, get(var))])
    setkey(test, uid)
    # get labels of click and book
    test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
                by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                  iclick = ifelse(sclick > 0, "click", "not click"))][order(-sbook)]
    test3 <- test2[, c(1, 4:5)]
    setkey(test3, uid)
    # merge
    test4 <- test[test3]
    test6 <- cast(test5, V2 ~ ibook + iclick)
    setnames(test6, c(var, "c11", "c10", "c00"))
    test6 <- data.table(test6)
    # get cr
    test6[, ':=' (csum = c00 + c10 + c11, cr = c11/(c00 + c10 + c11))]
    test7 <<- test6[get(var) != ""]
    # plot
    p <- ggplot(test7, aes(get(var), csum)) + geom_point() + coord_flip()
    p + geom_line(aes(get(var), cr*+ 514190, group = 1)) +
      scale_y_continuous(sec.axis = sec_axis(~./514190, name = "CR"))
  
}

cr("star")

# star, brand: only for exploration
# dist: no more than 4.5km
# criteria_num, keywordlen

