## header ======================================================================
# purpose: for presentation v2
# author: Zoey Hu
# create date: 04/14/2018 
# last update: 04/25/2018 
#
# input file: 
# output file: 
# 

library(data.table)
library(ggplot2)
library(reshape)

setwd("~/a/ctrip")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters
Sys.setlocale(category = "LC_ALL", locale ="zh_CN.UTF-8")


avg <- function(x){
  assign(paste("n", x, sep = ""), sc[, .(uid, get(x))][, .N, by = uid][order(-N)])
  print(sum(get(paste("n", x, sep = ""))[, N]))
  print(mean(get(paste("n", x, sep = ""))[, N]))
}

## usage of criteria ===========================================================

for (i in colnames(sc)[29: 50]){
  na <- ""
  tmp <- unique(sc[, .(uid, get(i))])
  setnames(tmp, c("uid", i))
  assign(paste("U", i, sep = ""), setorder(tmp[, .N, by = i], -N))
  line <- paste0(i, ": ", round(100*sum(tmp[!na, on = i, .N, by = i]$N)/404813,
                               2), "%",
                "(", sum(tmp[!na, on = i, .N, by = i]$N), ")")
  print(line)
}



## plotting basics==============================================================

#*-- number of queries ---------------------------------------------------------
nqid <- sc[, .(uid, qid_new)][, .N, by = uid][order(-N)]
sum(nqid[, N])
mean(nqid[, N])
p <- qplot(N, ..density.., data = nqid, geom = "histogram", 
      xlim = c(0, 50),
      xlab = "number of queries",
      fill = "E26562") 
p + theme(legend.position="none")
ggsave("nqueries.png", p + theme(legend.position="none"))

#*-- number of criteria --------------------------------------------------------
ncriteria <- sc[, .(uid, criteria_num)][, .(nc = sum(criteria_num)), 
                                        by = uid][order(-nc)]
sum(ncriteria[, nc])
sum(sc[, criteria_num])
summary(ncriteria[, nc])
p <- qplot(nc, ..density.., data = ncriteria, geom = "histogram", 
           xlim = c(0, 50),
           binwidth = 1,
           xlab = "number of criteria",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("ncriteria.png", p + theme(legend.position="none"))

#*-- advance days --------------------------------------------------------------
nadvdays <- sc[, .(uid, advancedays)][, .(nadv = mean(advancedays)), 
                                        by = uid][order(-nadv)]
nadvdays2 <- nadvdays[, .N, by = nadv][order(-N)]
summary(nadvdays[, nadv])
p <- qplot(nadv, ..density.., data = nadvdays, geom = "histogram", 
           xlim = c(0, 50),
           binwidth = 1,
           xlab = "advance days",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("nadvdays.png", p + theme(legend.position="none"))

#*-- consideration set ---------------------------------------------------------
nhotelid <- htl[, .(uid, hotelid)][, .N, by = uid][order(-N)]
sum(nhotelid[, N])
mean(nhotelid[, N])
summary(nhotelid)
p <- qplot(N, ..density.., data = nhotelid, geom = "histogram", 
           xlim = c(0, 500),
           binwidth = 1,
           xlab = "size of consideration set",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("ncset.png", p + theme(legend.position="none"))


#*-- conversion ----------------------------------------------------------------
## click
nclick <- sc[, .(uid, click)][, .(nc = sum(click)), 
                                        by = uid][order(-nc)]
sum(nclick[, nc])
sum(sc[, click])
# click rate
nclick2 <- nclick[, .N, by = nc][order(-N)]
nclick2[nc == 0, N]/sum(nclick2[, N])

summary(nclick[, nc])
p <- qplot(nc, ..density.., data = nclick, geom = "histogram", 
           xlim = c(0, 20), ylim = c(0, 0.3),
           binwidth = 1,
           xlab = "number of clicks",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("nclicks.png", p + theme(legend.position="none"))

## conversion
nbook <- sc[, .(uid, book)][, .(nb = sum(book)), 
                              by = uid][order(-nb)]
sum(nbook[, nb])
sum(sc[, book])
# conversion rate
nbook2 <- nbook[, .N, by = nb][order(-N)]
nclick2[nc == 0, N]/sum(nclick2[, N])

summary(nbook[, nb])
p <- qplot(nb, ..density.., data = nbook, geom = "histogram", 
           xlim = c(0, 7), ylim = c(0, 0.3),
           binwidth = 1,
           xlab = "number of conversions",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("nbooks.png", p + theme(legend.position="none"))


## plotting 2x2 ================================================================

fct <- function(var){
  # get unique var list
  test <- unique(sc[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click")
                                )][order(-sbook)]
  test3 <- test2[, c(1, 4:5)]
  setkey(test3, uid)
  # merge
  test4 <- test[test3]
  # aggregate
  test5 <<- test4[V2 != "" & !is.na(V2), .N, by = .(V2, ibook, iclick)][order(-N)]
  test5 <- test5[ibook == "not book" & iclick == "not click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "not click", N])]
  test5 <- test5[ibook == "not book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "not book" & iclick == "click", N])]
  test5 <<- test5[ibook == "book" & iclick == "click", 
                 N2 := N/sum(test5[ibook == "book" & iclick == "click", N])]
  test6 <<- test5
  # setkey(test6, V2)
  # test6 <<- test6[aametro22[2:101, 1]]
  #plot
  # p <<- qplot(V2, log(N2), data = test6, facets = ibook ~ iclick,
  #             ylab = "percentage",
  #             xlab = var,
  #             alpha = I(0.2)) +
  #       coord_flip() +
  #       theme(axis.text=element_text(size=8, family="PingFang SC"))
  p <<- qplot(x = reorder(V2, N), log(N2), data = test6,
              facets = ibook ~ iclick,
              ylab = "percentage",
              xlab = var,
              alpha = I(0.1)) +
        coord_flip() +
        theme(axis.text=element_text(size=5, family="PingFang SC"))
  p
}

fct("metro22")
sum(test6[,N2])
ggsave("fmetro22_log.png", p)

sum(aaspec[2:81, 2])/sum(aaspec[2:nrow(aaspec), 2])

names(sc)
View(aapaytype2)


cr <- function(var){
  # get unique var list
  test <- unique(sc[, .(uid, get(var))])
  setkey(test, uid)
  # get labels of click and book
  test2 <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
              by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                                iclick = ifelse(sclick > 0, "click", "not click")
                                )][order(-sbook)]
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
  g <<- ggplot(test7, aes(get(var), csum)) + geom_point() + coord_flip()
  g2 <<- g + geom_line(aes(get(var), cr*+ 514190, group = 1)) +
    scale_y_continuous(sec.axis = sec_axis(~./514190, name = "CR")) +
    labs(x = var, y = "frequency")
  g2
}

cr("star")
ggsave("cstar.png", g2)


## plot keyword ================================================================
# sequence
nseq <- sc[, .N, by = uid][order(-N)]
setnames(nseq, c("uid", "ns"))
# sequence = 10
n10 <- nseq[ns == "7", 1]
setkey(sc, uid)
names(sc)
seq10 <- sc[n10][, c(1, 61, 71)][order(uid, rank)]
# conversion
ucr <- sc[, .(sbook = sum(ifbook), sclick = sum(ifclick)), 
            by = uid][, ':=' (ibook = ifelse(sbook > 0, "book", "not book"), 
                              iclick = ifelse(sclick > 0, "click", "not click")
            )][order(-sbook)][, c(1, 4:5)]
setkey(ucr, uid)
ucr <- ucr[n10]
# merge
setkey(seq10, uid)
seq10a <- seq10[ucr]

#plot
p <- qplot(rank, star_num, data = seq10a, alpha = I(0.0001),
                               group = (uid), facets = ibook ~ iclick) +
     geom_jitter(alpha = I(0.03)) +
     geom_path(aes(colour = factor(uid)), alpha = I(0.1)) +
     stat_smooth(aes(group = 1), geom='line', alpha=0.5, se=TRUE, size = 2) +
     theme(legend.position="none")
p

ggsave("crinum_5.png", p)

## other =======================================================================

#*-- keywords ------------------------------------------------------------------
## assisting variabale
nuid <- data.table(unique(sc[, uid]))
nuid[, keyword := ""]
## keyword
nkeyword <- unique(rbindlist(list(sc[, .(uid, keyword)], nuid)))
nkeyword2 <- nkeyword[, .N, by = uid][order(-N)]
nkeyword2[, N := N - 1]
summary(nkeyword2[, N])

p <- qplot(N, ..density.., data = nkeyword2, geom = "histogram", 
           xlim = c(0, 10), ylim = c(0, 0.4),
           binwidth = 1,
           xlab = "number of keywords",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("nkeyword.png", p + theme(legend.position="none"))


#*-- advancedays ------------------------------------------------------------------
## assisting variabale
nuid <- data.table(unique(sc[, uid]))
nuid[, advancedays := ""]
## keyword
nadvdiff <- unique(sc[, .(uid, advancedays)])[order(advancedays)]
nkeyword <- unique(rbindlist(list(sc[, .(uid, keyword)], nuid)))
nkeyword2 <- nkeyword[, .N, by = uid][order(-N)]
nkeyword2[, N := N - 1]
summary(nkeyword2[, N])

p <- qplot(N, ..density.., data = nkeyword2, geom = "histogram", 
           xlim = c(0, 10), ylim = c(0, 0.4),
           binwidth = 1,
           xlab = "number of keywords",
           fill = "E26562") 
p + theme(legend.position="none")
ggsave("nkeyword.png", p + theme(legend.position="none"))




#*-- star and price ------------------------------------------------------------
starprice <- unique(sc[star != "" & price != "", .(uid, star, price)])
fwrite(starprice, "starprice.csv", sep = "\t")


## converstion and criteria ====================================================


#*-- keyworkd_len

cuid <- unique(sc[, .(uid, ifbook)])
cuid2 <-  cuid[, .(ifbook = sum(ifbook)), by = uid]
setkey(cuid2, uid)

## get keylen
ckeylen <- unique(sc[, .(uid, keyword_len)])
setkey(ckeylen, uid)
ckeylen <- ckeylen[cuid2]

ckeylen2 <- ckeylen[keyword_len != 0, .(conv = sum(ifbook)), by = keyword_len]
p <- qplot(keyword_len, log(conv), data = ckeylen2)
p


#*-- sorting

cuid <- unique(sc[, .(uid, ifbook)])
cuid2 <-  cuid[, .(ifbook = sum(ifbook)), by = uid]
setkey(cuid2, uid)

## get keylen
csort <- unique(sc[, .(uid, sort)])
setkey(csort, uid)
csort <- ckeylen[cuid2]

csort2 <- csort[keyword_len != 0, .(conv = sum(ifbook)), by = keyword_len]
p <- qplot(keyword_len, log(conv), data = ckeylen2)
p



