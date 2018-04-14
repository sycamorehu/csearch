## header ======================================================================
# purpose: to translate columns with Chinese values into English
# author: Zoey Hu
# create date: 04/12/2018 
# last update: //2018 
#
# input file: raw data 
#            "tmp_jinyongliu_dxj_query_1.csv.csv"
# output file: translated data (dimension: 6080365 x 60)
#            "search2_translated.csv"
#            "search2_translated.RData"
# 



## settings ====================================================================

library(tictoc)
library(stringr)
library(dplyr)

setwd("~/a/ctrip")
Sys.setlocale("LC_ALL", 'en_US.UTF-8') # read Chinese characters

## load data ===================================================================

search <- read.csv("~/a/ctrip/data/raw/tmp_jinyongliu_dxj_query_1.csv", 
                   header = TRUE, sep = '\t')

