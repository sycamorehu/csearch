## header ======================================================================
# purpose: to test "2_pre_v1"
# author: Zoey Hu
# create date: 04/25/2018 
# last update: //2018 
#
# - test date_num, session_num


library(sqldf)


## time distribution ===========================================================

#*-- test date_num, session_num ggplot -----------------------------------------
## stat_bin2d
aa <- user[, .N, by = .(date_num, session_num)][order(-N)]
d <- ggplot(aa, aes(x=date_num, y=session_num))
d + stat_bin2d(binwidth = 1.5)
## stat_density2d
ggplot(data = aa, 
       mapping = aes(x = date_num, y = session_num)) + 
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = FALSE)
## histgram
p <- qplot(session_num, ..density.., data = user, 
           geom = "histogram",
           binwidth = 1,
           xlim = c(1, 10)) 
p