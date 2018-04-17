## header ======================================================================
# purpose: for presentation v2
# author: Zoey Hu
# create date: 04/14/2018 
# last update: //2018 
#
# input file: 
# output file: 
# 

library(data.table)
library(ggplot2)

setwd("~/a/ctrip")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters


## usage of criteria ===========================================================

colnames(sc)

# 3:5 -> 32:33, 45 = ""
# 6:8, 12:13
# 10:11, 14:23, 25:27 = "nolimit", "-999", "4公里内"
# 24sort = "0", "6", 9spec: "-999", "0"

1:length(colnames(sc))
for (i in colnames(sc)[9]){
  na <- c("0", "-999") # c("", "nolimit", "-999", "4公里内")
  tmp <- unique(sc[, .(uid, get(i))])
  setnames(tmp, c("uid", i))
  assign(paste("U", i, sep = ""), setorder(tmp[, .N, by = i], -N))
  line <- paste0(i, ": ", round(100*sum(tmp[!na, on = i, .N, by = i]$N)/404813,
                               2), "%",
                "(", sum(tmp[!na, on = i, .N, by = i]$N), ")")
  print(line)
}



## plotting ====================================================================

## sort
qplot(sort_qry, data = sc[sort_qry != 0 
                          & sort_qry != 6 
                          & sort_qry != 9], 
      facets = . ~ ifclick,
      geom = "bar", xlim = c(0, 10))


## price range
qplot(price_range, data = sc[price_range != "NA"],
      facets = . ~ ifclick,
      geom = "density")

## filter score
qplot(filter_quantity, data = sc[filter_quantity != "-999"], 
      facets = . ~ ifclick,
      geom = "bar")


myplot <- function(x){
  qplot(x, data = sc[x != "-999"], 
        facets = . ~ ifclick,
        geom = "bar")
}

myplot(filter_quantity)


colnames(sc)


