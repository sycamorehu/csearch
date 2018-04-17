## header ======================================================================
# purpose: to test "2_pre_v0.R"
# author: Zoey Hu
# create date: 04/15/2018 
# last update: //2018 
#
# - check dft[!b, on = "name1", ...]



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
