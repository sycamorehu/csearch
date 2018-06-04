## header ======================================================================
# purpose: test "0_explore_v0.R"
# author: Zoey Hu
# create date: 04/12/2018 
# last update: //2018 
#
# - data.table and unique on starttime: 33.9 vs 102.4
# - data.table and sqldf on uidqid: 2.2 vs 196.5



library(sqldf)

## explore data ================================================================

#*-- starttime -----------------------------------------------------------------
tic("dt")
starttime <- dt[, .N, by = as_date(starttime)]
toc()  # dt: 33.893 sec elapsed

tic("unique")
table(as_date(search$starttime))
toc()  # unique: 102.432 sec elapsed


#*-- uid, qid ------------------------------------------------------------------
tic("dt")
uidqid<- dt[, .N, by = .(uid, qid_new)]
toc()  # dt: 2.209 sec elapsed

tic("sqldf")
uidqid2 <- sqldf("
                 select uid, qid_new, count(uid) as N
                 from dt
                 group by uid, qid_new
                 ")
toc()  # sqldf: 196.48 sec elapsed
rm(uidqid2)
