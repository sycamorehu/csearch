## header ======================================================================
# purpose: to test "2_pre_v2"
# author: Zoey Hu
# create date: 05/03/2018 
# last update: //2018 
#
# - test within-session, query-step learning

## price =======================================================================
prc44 <- prc[session < 5 & squery_order < 5,]
prc44plot <- prc44[, (price = mean(price_mean)), by = .(session, squery_order)]
setnames(prc44plot, "V1", "price_mean")
ggplot(prc44, aes(session, mean(price_mean)-300, group = squery_order, 
                  fill = factor(squery_order))) +
  geom_col(position = "dodge")