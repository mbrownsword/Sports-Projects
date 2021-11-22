library(textclean)
library(data.table)
library(flexdashboard)
library(rvest)
library(googlesheets)
library(readxl)
library(dplyr)
library(raster)
library(tidyverse)
library(data.table)
library(tools)
library(httr)
library(RSelenium)
library(jsonlite)
library(RSelenium)
library(httr)
library(rvest)
library(ttutils)
library(jsonlite)
library(remotes)
library(emayili)
library(dplyr)
library(curl)
library(openssl)
library(sys)
library(mailR)
library(knitr)
library(png)
library(mailR)
library(DT)
library(webshot)
library(stringdist)
library(dplyr)
library(curl)
library(openssl)
library(sys)
library(mailR)
library(printr)
library(png)
library(mailR)
library(DT)
library(webshot)
library(ggpubr)
library(png)
library(grid)
library(gridExtra)
library(tidyquant)
library(pracma)
setwd("C:/Users/CORSAIR GAMING/Documents/Action Analysis")

#### Source League Files #####


source("nhl_test.R")
source("nba_analysis.R")
source("cfb_test.R")
source("nfl_test.R")
source("mlb_test.R")
source("cbb_acc_analysis.R")
source("cbb_sec_analysis.R")
source("cbb_big10_analysis.R")
source("cbb_big12_analysis.R")
source("cbb_pac12_analysis.R")
source("cbb_ivy_analysis.R")
options(scipen=999)

#### Backtest #####

nhl_games_all <- nhl_backtest()

nhl_games_all$money_home_ml <- as.numeric(nhl_games_all$money_home_ml)
nhl_games_all$money_home <- as.numeric(nhl_games_all$money_home)
nhl_games_all$money_away_ml <- as.numeric(nhl_games_all$money_away_ml)
nhl_games_all$money_away <- as.numeric(nhl_games_all$money_away)
nhl_games_all$public_home_ml <- as.numeric(nhl_games_all$public_home_ml)
nhl_games_all$public_home <- as.numeric(nhl_games_all$public_home)
nhl_games_all$public_away_ml <- as.numeric(nhl_games_all$public_away_ml)
nhl_games_all$public_away <- as.numeric(nhl_games_all$public_away)

nhl_udf_games_all <- nhl_udf_analysis(nhl_games_all)

nba_games_all <- nba_backtest()

nba_games_all$money_home_ml <- as.numeric(nba_games_all$money_home_ml)
nba_games_all$money_home <- as.numeric(nba_games_all$money_home)
nba_games_all$money_away_ml <- as.numeric(nba_games_all$money_away_ml)
nba_games_all$money_away <- as.numeric(nba_games_all$money_away)
nba_games_all$public_home_ml <- as.numeric(nba_games_all$public_home_ml)
nba_games_all$public_home <- as.numeric(nba_games_all$public_home)
nba_games_all$public_away_ml <- as.numeric(nba_games_all$public_away_ml)
nba_games_all$public_away <- as.numeric(nba_games_all$public_away)

nba_udf_games_all <- nba_udf_analysis(nba_games_all)

cfb_games_all <- cfb_backtest()

cfb_games_all$money_home_ml <- as.numeric(cfb_games_all$money_home_ml)
cfb_games_all$money_home <- as.numeric(cfb_games_all$money_home)
cfb_games_all$money_away_ml <- as.numeric(cfb_games_all$money_away_ml)
cfb_games_all$money_away <- as.numeric(cfb_games_all$money_away)
cfb_games_all$public_home_ml <- as.numeric(cfb_games_all$public_home_ml)
cfb_games_all$public_home <- as.numeric(cfb_games_all$public_home)
cfb_games_all$public_away_ml <- as.numeric(cfb_games_all$public_away_ml)
cfb_games_all$public_away <- as.numeric(cfb_games_all$public_away)

cfb_udf_games_all <- cfb_udf_analysis(cfb_games_all)

#### College Basketball ####

source("cbb backtesting.R")

nfl_games_all <- nfl_backtest()

nfl_games_all$money_home_ml <- as.numeric(nfl_games_all$money_home_ml)
nfl_games_all$money_home <- as.numeric(nfl_games_all$money_home)
nfl_games_all$money_away_ml <- as.numeric(nfl_games_all$money_away_ml)
nfl_games_all$money_away <- as.numeric(nfl_games_all$money_away)
nfl_games_all$public_home_ml <- as.numeric(nfl_games_all$public_home_ml)
nfl_games_all$public_home <- as.numeric(nfl_games_all$public_home)
nfl_games_all$public_away_ml <- as.numeric(nfl_games_all$public_away_ml)
nfl_games_all$public_away <- as.numeric(nfl_games_all$public_away)

nfl_udf_games_all <- nfl_udf_analysis(nfl_games_all)

mlb_games_all <- mlb_backtest()

mlb_games_all$money_home_ml <- as.numeric(mlb_games_all$money_home_ml)
mlb_games_all$money_home <- as.numeric(mlb_games_all$money_home)
mlb_games_all$money_away_ml <- as.numeric(mlb_games_all$money_away_ml)
mlb_games_all$money_away <- as.numeric(mlb_games_all$money_away)
mlb_games_all$public_home_ml <- as.numeric(mlb_games_all$public_home_ml)
mlb_games_all$public_home <- as.numeric(mlb_games_all$public_home)
mlb_games_all$public_away_ml <- as.numeric(mlb_games_all$public_away_ml)
mlb_games_all$public_away <- as.numeric(mlb_games_all$public_away)

mlb_udf_games_all <- mlb_udf_analysis(mlb_games_all)

#### Save backtest data #####

backtest_games <- list(nhl_games_all,
                       nba_games_all,
                       cfb_games_all,
                       nfl_games_all,
                       sec_cbb_games_all,
                       acc_cbb_games_all,
                       big10_cbb_games_all,
                       big12_cbb_games_all,
                       pac12_cbb_games_all,
                       ivy_cbb_games_all,
                       bigsky_cbb_games_all,
                       bigeast_cbb_games_all,
                       a10_cbb_games_all,
                       aac_cbb_games_all,
                       cusa_cbb_games_all,
                       mlb_games_all)

backtest_games_udf <- list(nhl_udf_games_all,
                           nba_udf_games_all,
                           cfb_udf_games_all,
                           nfl_udf_games_all,
                           sec_cbb_udf_games_all,
                           acc_cbb_udf_games_all,
                           big10_cbb_udf_games_all,
                           big12_cbb_udf_games_all,
                           pac12_cbb_udf_games_all,
                           ivy_cbb_udf_games_all,
                           bigsky_cbb_udf_games_all,
                           bigeast_cbb_udf_games_all,
                           a10_cbb_udf_games_all,
                           aac_cbb_udf_games_all,
                           cusa_cbb_udf_games_all,
                           mlb_udf_games_all)


#### League Backtest Analysis (H/A) #####

aml_amlsharp <- 0
aml_amlsharp_pct <- 0
ac_amlsharp <- 0
ac_amlsharp_pct <- 0
aml_asharp <- 0
aml_asharp_pct <- 0
ac_asharp <- 0
ac_asharp_pct <- 0

hml_amlsharp <- 0
hml_amlsharp_pct  <- 0
hc_amlsharp  <- 0
hc_amlsharp_pct<- 0
hc_asharp<- 0
hc_asharp_pct<- 0
hml_asharp<- 0
hml_asharp_pct<- 0

for(i in 1:length(backtest_games)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$asd_ml)))){
    indicator <- unique(backtest_games[[i]]$asd_ml)[-which(is.na(unique(backtest_games[[i]]$asd_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$asd_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$asd_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$asd_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$asd_ml>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$asd_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$asd_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$asd_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$asd_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_amlsharp[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_amlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  hml_amlsharp[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_amlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$asd_ml)))){
    indicator <- unique(backtest_games[[i]]$asd_ml)[-which(is.na(unique(backtest_games[[i]]$asd_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$asd_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$asd_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$asd_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$asd_ml>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$asd_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$asd_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$asd_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$asd_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  ac_amlsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_amlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_amlsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_amlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games[[i]]$asd_s)))){
    indicator <- unique(backtest_games[[i]]$asd_s)[-which(is.na(unique(backtest_games[[i]]$asd_s)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$asd_s)),]
  }else{
    indicator <- unique(backtest_games[[i]]$asd_s)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$asd_s>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$asd_s>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$asd_s>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$asd_s>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$asd_s>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$asd_s>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_asharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_asharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hml_asharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_asharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$asd_s)))){
    indicator <- unique(backtest_games[[i]]$asd_s)[-which(is.na(unique(backtest_games[[i]]$asd_s)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$asd_s)),]
  }else{
    indicator <- unique(backtest_games[[i]]$asd_s)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$asd_s>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$asd_s>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$asd_s>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$asd_s>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$asd_s>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$asd_s>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  ac_asharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_asharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_asharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_asharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

away_sharp_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                  aml_amlsharp, 
                                  aml_amlsharp_pct, 
                                  ac_amlsharp, 
                                  ac_amlsharp_pct, 
                                  aml_asharp, 
                                  aml_asharp_pct ,
                                  ac_asharp, 
                                  ac_asharp_pct,
                                  
                                  hml_amlsharp, 
                                  hml_amlsharp_pct,  
                                  hc_amlsharp,  
                                  hc_amlsharp_pct,
                                  hc_asharp,
                                  hc_asharp_pct,
                                  hml_asharp,
                                  hml_asharp_pct)



hml_hmlsharp <- 0
hml_hmlsharp_pct <- 0
hc_hmlsharp <- 0
hc_hmlsharp_pct <- 0
hml_hsharp <- 0
hml_hsharp_pct <- 0
hc_hsharp <- 0
hc_hsharp_pct <- 0

aml_hmlsharp <- 0
aml_hmlsharp_pct <- 0
ac_hmlsharp <- 0
ac_hmlsharp_pct <- 0
aml_hsharp <- 0
aml_hsharp_pct <- 0
ac_hsharp <- 0
ac_hsharp_pct <- 0

for(i in 1:length(backtest_games)){
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$hsd_ml)))){
    indicator <- unique(backtest_games[[i]]$hsd_ml)[-which(is.na(unique(backtest_games[[i]]$hsd_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$hsd_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$hsd_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$hsd_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$hsd_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$hsd_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$hsd_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$hsd_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$hsd_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hmlsharp[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hmlsharp[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$hsd_ml)))){
    indicator <- unique(backtest_games[[i]]$hsd_ml)[-which(is.na(unique(backtest_games[[i]]$hsd_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$hsd_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$hsd_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$hsd_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$hsd_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$hsd_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$hsd_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$hsd_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$hsd_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hmlsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hmlsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$hsd_s)))){
    indicator <- unique(backtest_games[[i]]$hsd_s)[-which(is.na(unique(backtest_games[[i]]$hsd_s)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$hsd_s)),]
  }else{
    indicator <- unique(backtest_games[[i]]$hsd_s)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$hsd_s>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$hsd_s>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$hsd_s>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$hsd_s>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$hsd_s>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$hsd_s>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$hsd_s)))){
    indicator <- unique(backtest_games[[i]]$hsd_s)[-which(is.na(unique(backtest_games[[i]]$hsd_s)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$hsd_s)),]
  }else{
    indicator <- unique(backtest_games[[i]]$hsd_s)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$hsd_s>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$hsd_s>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$hsd_s>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$hsd_s>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$hsd_s>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$hsd_s>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
}

home_sharp_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                  hml_hmlsharp ,
                                  hml_hmlsharp_pct ,
                                  hc_hmlsharp ,
                                  hc_hmlsharp_pct ,
                                  hml_hsharp ,
                                  hml_hsharp_pct ,
                                  hc_hsharp ,
                                  hc_hsharp_pct ,
                                  
                                  aml_hmlsharp ,
                                  aml_hmlsharp_pct ,
                                  ac_hmlsharp ,
                                  ac_hmlsharp_pct ,
                                  aml_hsharp ,
                                  aml_hsharp_pct ,
                                  ac_hsharp ,
                                  ac_hsharp_pct)


aml_amlpublic <- 0
aml_amlpublic_pct <- 0
ac_amlpublic <- 0
ac_amlpublic_pct <- 0
aml_apublic <- 0
aml_apublic_pct <- 0
ac_apublic <- 0
ac_apublic_pct <- 0

hml_amlpublic <- 0
hml_amlpublic_pct  <- 0
hc_amlpublic  <- 0
hc_amlpublic_pct<- 0
hc_apublic<- 0
hc_apublic_pct<- 0
hml_apublic<- 0
hml_apublic_pct<- 0

for(i in 1:length(backtest_games)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_away_ml)))){
    indicator <- unique(backtest_games[[i]]$public_away_ml)[-which(is.na(unique(backtest_games[[i]]$public_away_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_away_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_away_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$public_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$public_away_ml>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$public_away_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$public_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$public_away_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$public_away_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_amlpublic[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_amlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  hml_amlpublic[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_amlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_away_ml)))){
    indicator <- unique(backtest_games[[i]]$public_away_ml)[-which(is.na(unique(backtest_games[[i]]$public_away_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_away_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_away_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$public_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$public_away_ml>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$public_away_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$public_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$public_away_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$public_away_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  ac_amlpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_amlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_amlpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_amlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games[[i]]$public_away)))){
    indicator <- unique(backtest_games[[i]]$public_away)[-which(is.na(unique(backtest_games[[i]]$public_away)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_away)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_away)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$public_away>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$public_away>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$public_away>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$public_away>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$public_away>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$public_away>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_apublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_apublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hml_apublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_apublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_away)))){
    indicator <- unique(backtest_games[[i]]$public_away)[-which(is.na(unique(backtest_games[[i]]$public_away)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_away)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_away)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$public_away>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$public_away>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$public_away>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$public_away>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$public_away>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$public_away>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  ac_apublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_apublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_apublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_apublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

away_public_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                   aml_amlpublic, 
                                   aml_amlpublic_pct, 
                                   ac_amlpublic, 
                                   ac_amlpublic_pct, 
                                   aml_apublic, 
                                   aml_apublic_pct ,
                                   ac_apublic, 
                                   ac_apublic_pct,
                                   
                                   hml_amlpublic, 
                                   hml_amlpublic_pct,  
                                   hc_amlpublic,  
                                   hc_amlpublic_pct,
                                   hc_apublic,
                                   hc_apublic_pct,
                                   hml_apublic,
                                   hml_apublic_pct)



hml_hmlpublic <- 0
hml_hmlpublic_pct <- 0
hc_hmlpublic <- 0
hc_hmlpublic_pct <- 0
hml_hpublic <- 0
hml_hpublic_pct <- 0
hc_hpublic <- 0
hc_hpublic_pct <- 0

aml_hmlpublic <- 0
aml_hmlpublic_pct <- 0
ac_hmlpublic <- 0
ac_hmlpublic_pct <- 0
aml_hpublic <- 0
aml_hpublic_pct <- 0
ac_hpublic <- 0
ac_hpublic_pct <- 0

for(i in 1:length(backtest_games)){
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_home_ml)))){
    indicator <- unique(backtest_games[[i]]$public_home_ml)[-which(is.na(unique(backtest_games[[i]]$public_home_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_home_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_home_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$public_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$public_home_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$public_home_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$public_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$public_home_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$public_home_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hmlpublic[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hmlpublic[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_home_ml)))){
    indicator <- unique(backtest_games[[i]]$public_home_ml)[-which(is.na(unique(backtest_games[[i]]$public_home_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_home_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_home_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$public_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$public_home_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$public_home_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$public_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$public_home_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$public_home_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hmlpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hmlpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_home)))){
    indicator <- unique(backtest_games[[i]]$public_home)[-which(is.na(unique(backtest_games[[i]]$public_home)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_home)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_home)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$public_home>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$public_home>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$public_home>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$public_home>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$public_home>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$public_home>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$public_home)))){
    indicator <- unique(backtest_games[[i]]$public_home)[-which(is.na(unique(backtest_games[[i]]$public_home)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$public_home)),]
  }else{
    indicator <- unique(backtest_games[[i]]$public_home)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$public_home>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$public_home>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$public_home>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$public_home>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$public_home>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$public_home>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
}

home_public_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                   hml_hmlpublic ,
                                   hml_hmlpublic_pct ,
                                   hc_hmlpublic ,
                                   hc_hmlpublic_pct ,
                                   hml_hpublic ,
                                   hml_hpublic_pct ,
                                   hc_hpublic ,
                                   hc_hpublic_pct ,
                                   
                                   aml_hmlpublic ,
                                   aml_hmlpublic_pct ,
                                   ac_hmlpublic ,
                                   ac_hmlpublic_pct ,
                                   aml_hpublic ,
                                   aml_hpublic_pct ,
                                   ac_hpublic ,
                                   ac_hpublic_pct)

aml_amlmoney <- 0
aml_amlmoney_pct <- 0
ac_amlmoney <- 0
ac_amlmoney_pct <- 0
aml_amoney <- 0
aml_amoney_pct <- 0
ac_amoney <- 0
ac_amoney_pct <- 0

hml_amlmoney <- 0
hml_amlmoney_pct  <- 0
hc_amlmoney  <- 0
hc_amlmoney_pct<- 0
hc_amoney<- 0
hc_amoney_pct<- 0
hml_amoney<- 0
hml_amoney_pct<- 0

for(i in 1:length(backtest_games)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_away_ml)))){
    indicator <- unique(backtest_games[[i]]$money_away_ml)[-which(is.na(unique(backtest_games[[i]]$money_away_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_away_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_away_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$money_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$money_away_ml>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$money_away_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$money_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$money_away_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$money_away_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_amlmoney[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_amlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  hml_amlmoney[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_amlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_away_ml)))){
    indicator <- unique(backtest_games[[i]]$money_away_ml)[-which(is.na(unique(backtest_games[[i]]$money_away_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_away_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_away_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$money_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$money_away_ml>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$money_away_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$money_away_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$money_away_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$money_away_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  ac_amlmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_amlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_amlmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_amlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games[[i]]$money_away)))){
    indicator <- unique(backtest_games[[i]]$money_away)[-which(is.na(unique(backtest_games[[i]]$money_away)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_away)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_away)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$amlc[backtest_data$money_away>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$money_away>=indicator[t]])
    n[t] <-  length(backtest_data$amlc[backtest_data$money_away>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hmlc[backtest_data$money_away>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$money_away>=indicator[t]])
    n1[t] <-  length(backtest_data$hmlc[backtest_data$money_away>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  aml_amoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  aml_amoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hml_amoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hml_amoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_away)))){
    indicator <- unique(backtest_games[[i]]$money_away)[-which(is.na(unique(backtest_games[[i]]$money_away)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_away)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_away)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$ac[backtest_data$money_away>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$money_away>=indicator[t]])
    n[t] <-  length(backtest_data$ac[backtest_data$money_away>=indicator[t]])
    roi1[t] <-  sum(backtest_data$hc[backtest_data$money_away>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$money_away>=indicator[t]])
    n1[t] <-  length(backtest_data$hc[backtest_data$money_away>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  ac_amoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  ac_amoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  hc_amoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  hc_amoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

away_money_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                  aml_amlmoney, 
                                  aml_amlmoney_pct, 
                                  ac_amlmoney, 
                                  ac_amlmoney_pct, 
                                  aml_amoney, 
                                  aml_amoney_pct ,
                                  ac_amoney, 
                                  ac_amoney_pct,
                                  
                                  hml_amlmoney, 
                                  hml_amlmoney_pct,  
                                  hc_amlmoney,  
                                  hc_amlmoney_pct,
                                  hc_amoney,
                                  hc_amoney_pct,
                                  hml_amoney,
                                  hml_amoney_pct)



hml_hmlmoney <- 0
hml_hmlmoney_pct <- 0
hc_hmlmoney <- 0
hc_hmlmoney_pct <- 0
hml_hmoney <- 0
hml_hmoney_pct <- 0
hc_hmoney <- 0
hc_hmoney_pct <- 0

aml_hmlmoney <- 0
aml_hmlmoney_pct <- 0
ac_hmlmoney <- 0
ac_hmlmoney_pct <- 0
aml_hmoney <- 0
aml_hmoney_pct <- 0
ac_hmoney <- 0
ac_hmoney_pct <- 0

for(i in 1:length(backtest_games)){
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_home_ml)))){
    indicator <- unique(backtest_games[[i]]$money_home_ml)[-which(is.na(unique(backtest_games[[i]]$money_home_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_home_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_home_ml)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$money_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$money_home_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$money_home_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$money_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$money_home_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$money_home_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hmlmoney[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hmlmoney[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_home_ml)))){
    indicator <- unique(backtest_games[[i]]$money_home_ml)[-which(is.na(unique(backtest_games[[i]]$money_home_ml)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_home_ml)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_home_ml)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$money_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$money_home_ml>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$money_home_ml>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$money_home_ml>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$money_home_ml>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$money_home_ml>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hmlmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hmlmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_home)))){
    indicator <- unique(backtest_games[[i]]$money_home)[-which(is.na(unique(backtest_games[[i]]$money_home)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_home)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_home)
    backtest_data <- backtest_games[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$home_ml))>400|as.numeric(as.character(backtest_data$away_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hmlc[backtest_data$money_home>=indicator[t]], na.rm = T)/length(backtest_data$hmlc[backtest_data$money_home>=indicator[t]])
    n[t] <-  length(backtest_data$hmlc[backtest_data$money_home>=indicator[t]])
    roi1[t] <-  sum(backtest_data$amlc[backtest_data$money_home>=indicator[t]], na.rm = T)/length(backtest_data$amlc[backtest_data$money_home>=indicator[t]])
    n1[t] <-  length(backtest_data$amlc[backtest_data$money_home>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hml_hmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hml_hmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  aml_hmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  aml_hmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games[[i]]$money_home)))){
    indicator <- unique(backtest_games[[i]]$money_home)[-which(is.na(unique(backtest_games[[i]]$money_home)))]
    backtest_data <- backtest_games[[i]][-which(is.na(backtest_games[[i]]$money_home)),]
  }else{
    indicator <- unique(backtest_games[[i]]$money_home)
    backtest_data <- backtest_games[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$hc[backtest_data$money_home>=indicator[t]], na.rm = T)/length(backtest_data$hc[backtest_data$money_home>=indicator[t]])
    n[t] <-  length(backtest_data$hc[backtest_data$money_home>=indicator[t]])
    roi1[t] <-  sum(backtest_data$ac[backtest_data$money_home>=indicator[t]], na.rm = T)/length(backtest_data$ac[backtest_data$money_home>=indicator[t]])
    n1[t] <-  length(backtest_data$ac[backtest_data$money_home>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  hc_hmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  hc_hmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  ac_hmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  ac_hmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
}

home_money_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                  hml_hmlmoney ,
                                  hml_hmlmoney_pct ,
                                  hc_hmlmoney ,
                                  hc_hmlmoney_pct ,
                                  hml_hmoney ,
                                  hml_hmoney_pct ,
                                  hc_hmoney ,
                                  hc_hmoney_pct ,
                                  
                                  aml_hmlmoney ,
                                  aml_hmlmoney_pct ,
                                  ac_hmlmoney ,
                                  ac_hmlmoney_pct ,
                                  aml_hmoney ,
                                  aml_hmoney_pct ,
                                  ac_hmoney ,
                                  ac_hmoney_pct)




#### League Backtest Analysis (Ud/F) #####

udml_udmlsharp <- 0
udml_udmlsharp_pct <- 0
udc_udmlsharp <- 0
udc_udmlsharp_pct <- 0
udml_udsharp <- 0
udml_udsharp_pct <- 0
udc_udsharp <- 0
udc_udsharp_pct <- 0

fml_udmlsharp <- 0
fml_udmlsharp_pct  <- 0
fc_udmlsharp  <- 0
fc_udmlsharp_pct<- 0
fc_udsharp<- 0
fc_udsharp_pct<- 0
fml_udsharp<- 0
fml_udsharp_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlunderdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlunderdog_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlunderdog_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlunderdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlunderdog_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlunderdog_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udmlsharp[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_udmlsharp[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlunderdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlunderdog_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlunderdog_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlunderdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlunderdog_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlunderdog_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_udmlsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udmlsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$underdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$underdog_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$underdog_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$underdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$underdog_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$underdog_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_udsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$underdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$underdog_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$underdog_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$underdog_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$underdog_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$underdog_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_udsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

underdog_sharp_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                  udml_udmlsharp, 
                                  udml_udmlsharp_pct, 
                                  udc_udmlsharp, 
                                  udc_udmlsharp_pct, 
                                  udml_udsharp, 
                                  udml_udsharp_pct ,
                                  udc_udsharp, 
                                  udc_udsharp_pct,
                                  
                                  fml_udmlsharp, 
                                  fml_udmlsharp_pct,  
                                  fc_udmlsharp,  
                                  fc_udmlsharp_pct,
                                  fc_udsharp,
                                  fc_udsharp_pct,
                                  fml_udsharp,
                                  fml_udsharp_pct)

udml_udmlpublic <- 0
udml_udmlpublic_pct <- 0
udc_udmlpublic <- 0
udc_udmlpublic_pct <- 0
udml_udpublic <- 0
udml_udpublic_pct <- 0
udc_udpublic <- 0
udc_udpublic_pct <- 0

fml_udmlpublic <- 0
fml_udmlpublic_pct  <- 0
fc_udmlpublic  <- 0
fc_udmlpublic_pct<- 0
fc_udpublic<- 0
fc_udpublic_pct<- 0
fml_udpublic<- 0
fml_udpublic_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_public)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_public)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlunderdog_public>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlunderdog_public>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlunderdog_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlunderdog_public>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlunderdog_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlunderdog_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udmlpublic[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_udmlpublic[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_public)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_public)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlunderdog_public>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlunderdog_public>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlunderdog_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlunderdog_public>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlunderdog_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlunderdog_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_udmlpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udmlpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_public)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_public)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$underdog_public>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$underdog_public>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$underdog_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$underdog_public>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$underdog_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$underdog_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_udpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_public)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_public)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$underdog_public>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$underdog_public>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$underdog_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$underdog_public>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$underdog_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$underdog_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_udpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

underdog_public_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                      udml_udmlpublic, 
                                      udml_udmlpublic_pct, 
                                      udc_udmlpublic, 
                                      udc_udmlpublic_pct, 
                                      udml_udpublic, 
                                      udml_udpublic_pct ,
                                      udc_udpublic, 
                                      udc_udpublic_pct,
                                      
                                      fml_udmlpublic, 
                                      fml_udmlpublic_pct,  
                                      fc_udmlpublic,  
                                      fc_udmlpublic_pct,
                                      fc_udpublic,
                                      fc_udpublic_pct,
                                      fml_udpublic,
                                      fml_udpublic_pct)

udml_udmlmoney <- 0
udml_udmlmoney_pct <- 0
udc_udmlmoney <- 0
udc_udmlmoney_pct <- 0
udml_udmoney <- 0
udml_udmoney_pct <- 0
udc_udmoney <- 0
udc_udmoney_pct <- 0

fml_udmlmoney <- 0
fml_udmlmoney_pct  <- 0
fc_udmlmoney  <- 0
fc_udmlmoney_pct<- 0
fc_udmoney<- 0
fc_udmoney_pct<- 0
fml_udmoney<- 0
fml_udmoney_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_money)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_money)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlunderdog_money>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlunderdog_money>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlunderdog_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlunderdog_money>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlunderdog_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlunderdog_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udmlmoney[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_udmlmoney[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlunderdog_money)))){
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_money)[-which(is.na(unique(backtest_games_udf[[i]]$mlunderdog_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlunderdog_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlunderdog_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlunderdog_money>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlunderdog_money>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlunderdog_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlunderdog_money>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlunderdog_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlunderdog_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_udmlmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udmlmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_money)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_money)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$underdog_money>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$underdog_money>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$underdog_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$underdog_money>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$underdog_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$underdog_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_udmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_udmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_udmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_udmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$underdog_money)))){
    indicator <- unique(backtest_games_udf[[i]]$underdog_money)[-which(is.na(unique(backtest_games_udf[[i]]$underdog_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$underdog_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$underdog_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$underdog_money>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$underdog_money>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$underdog_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$underdog_money>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$underdog_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$underdog_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_udmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_udmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_udmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_udmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

underdog_money_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                       udml_udmlmoney, 
                                       udml_udmlmoney_pct, 
                                       udc_udmlmoney, 
                                       udc_udmlmoney_pct, 
                                       udml_udmoney, 
                                       udml_udmoney_pct ,
                                       udc_udmoney, 
                                       udc_udmoney_pct,
                                       
                                       fml_udmlmoney, 
                                       fml_udmlmoney_pct,  
                                       fc_udmlmoney,  
                                       fc_udmlmoney_pct,
                                       fc_udmoney,
                                       fc_udmoney_pct,
                                       fml_udmoney,
                                       fml_udmoney_pct)

udml_fmlsharp <- 0
udml_fmlsharp_pct <- 0
udc_fmlsharp <- 0
udc_fmlsharp_pct <- 0
udml_fsharp <- 0
udml_fsharp_pct <- 0
udc_fsharp <- 0
udc_fsharp_pct <- 0

fml_fmlsharp <- 0
fml_fmlsharp_pct  <- 0
fc_fmlsharp  <- 0
fc_fmlsharp_pct<- 0
fc_fsharp<- 0
fc_fsharp_pct<- 0
fml_fsharp<- 0
fml_fsharp_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlfavorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlfavorite_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlfavorite_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlfavorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlfavorite_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlfavorite_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fmlsharp[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_fmlsharp[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlfavorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlfavorite_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlfavorite_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlfavorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlfavorite_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlfavorite_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_fmlsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fmlsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fmlsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fmlsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$favorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$favorite_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$favorite_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$favorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$favorite_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$favorite_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_fsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_sharp)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_sharp)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_sharp)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_sharp)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_sharp)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$favorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$favorite_sharp>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$favorite_sharp>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$favorite_sharp>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$favorite_sharp>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$favorite_sharp>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_fsharp[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fsharp_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fsharp[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fsharp_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

favorite_sharp_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                      udml_fmlsharp, 
                                      udml_fmlsharp_pct, 
                                      udc_fmlsharp, 
                                      udc_fmlsharp_pct, 
                                      udml_fsharp, 
                                      udml_fsharp_pct ,
                                      udc_fsharp, 
                                      udc_fsharp_pct,
                                      
                                      fml_fmlsharp, 
                                      fml_fmlsharp_pct,  
                                      fc_fmlsharp,  
                                      fc_fmlsharp_pct,
                                      fc_fsharp,
                                      fc_fsharp_pct,
                                      fml_fsharp,
                                      fml_fsharp_pct)

udml_fmlpublic <- 0
udml_fmlpublic_pct <- 0
udc_fmlpublic <- 0
udc_fmlpublic_pct <- 0
udml_fpublic <- 0
udml_fpublic_pct <- 0
udc_fpublic <- 0
udc_fpublic_pct <- 0

fml_fmlpublic <- 0
fml_fmlpublic_pct  <- 0
fc_fmlpublic  <- 0
fc_fmlpublic_pct<- 0
fc_fpublic<- 0
fc_fpublic_pct<- 0
fml_fpublic<- 0
fml_fpublic_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_public)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_public)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlfavorite_public>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlfavorite_public>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlfavorite_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlfavorite_public>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlfavorite_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlfavorite_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fmlpublic[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_fmlpublic[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_public)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_public)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlfavorite_public>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlfavorite_public>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlfavorite_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlfavorite_public>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlfavorite_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlfavorite_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_fmlpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fmlpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fmlpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fmlpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_public)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_public)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$favorite_public>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$favorite_public>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$favorite_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$favorite_public>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$favorite_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$favorite_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_fpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_public)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_public)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_public)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_public)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_public)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$favorite_public>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$favorite_public>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$favorite_public>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$favorite_public>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$favorite_public>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$favorite_public>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_fpublic[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fpublic_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fpublic[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fpublic_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

favorite_public_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                       udml_fmlpublic, 
                                       udml_fmlpublic_pct, 
                                       udc_fmlpublic, 
                                       udc_fmlpublic_pct, 
                                       udml_fpublic, 
                                       udml_fpublic_pct ,
                                       udc_fpublic, 
                                       udc_fpublic_pct,
                                       
                                       fml_fmlpublic, 
                                       fml_fmlpublic_pct,  
                                       fc_fmlpublic,  
                                       fc_fmlpublic_pct,
                                       fc_fpublic,
                                       fc_fpublic_pct,
                                       fml_fpublic,
                                       fml_fpublic_pct)

udml_fmlmoney <- 0
udml_fmlmoney_pct <- 0
udc_fmlmoney <- 0
udc_fmlmoney_pct <- 0
udml_fmoney <- 0
udml_fmoney_pct <- 0
udc_fmoney <- 0
udc_fmoney_pct <- 0

fml_fmlmoney <- 0
fml_fmlmoney_pct  <- 0
fc_fmlmoney  <- 0
fc_fmlmoney_pct<- 0
fc_fmoney<- 0
fc_fmoney_pct<- 0
fml_fmoney<- 0
fml_fmoney_pct<- 0

for(i in 1:length(backtest_games_udf)){
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_money)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_money)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$mlfavorite_money>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$mlfavorite_money>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$mlfavorite_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$mlfavorite_money>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$mlfavorite_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$mlfavorite_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fmlmoney[i] <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  fml_fmlmoney[i] <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$mlfavorite_money)))){
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_money)[-which(is.na(unique(backtest_games_udf[[i]]$mlfavorite_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$mlfavorite_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$mlfavorite_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$mlfavorite_money>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$mlfavorite_money>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$mlfavorite_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$mlfavorite_money>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$mlfavorite_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$mlfavorite_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udc_fmlmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fmlmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fmlmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fmlmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1<- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_money)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_money)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  backtest_data <- backtest_data[-which(as.numeric(as.character(backtest_data$favorite_ml))>400|as.numeric(as.character(backtest_data$underdog_ml))>400),]
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udmlc[backtest_data$favorite_money>=indicator[t]], na.rm = T)/length(backtest_data$udmlc[backtest_data$favorite_money>=indicator[t]])
    n[t] <-  length(backtest_data$udmlc[backtest_data$favorite_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fmlc[backtest_data$favorite_money>=indicator[t]], na.rm = T)/length(backtest_data$fmlc[backtest_data$favorite_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fmlc[backtest_data$favorite_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  
  udml_fmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udml_fmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fml_fmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fml_fmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
  roi <- 0
  n <- 0
  roi1 <- 0
  n1 <- 0
  if(any(is.na(unique(backtest_games_udf[[i]]$favorite_money)))){
    indicator <- unique(backtest_games_udf[[i]]$favorite_money)[-which(is.na(unique(backtest_games_udf[[i]]$favorite_money)))]
    backtest_data <- backtest_games_udf[[i]][-which(is.na(backtest_games_udf[[i]]$favorite_money)),]
  }else{
    indicator <- unique(backtest_games_udf[[i]]$favorite_money)
    backtest_data <- backtest_games_udf[[i]]
  }
  indicator <- indicator[order(as.numeric(indicator))]
  for(t in 1:length(indicator)){
    roi[t] <-  sum(backtest_data$udc[backtest_data$favorite_money>=indicator[t]], na.rm = T)/length(backtest_data$udc[backtest_data$favorite_money>=indicator[t]])
    n[t] <-  length(backtest_data$udc[backtest_data$favorite_money>=indicator[t]])
    roi1[t] <-  sum(backtest_data$fc[backtest_data$favorite_money>=indicator[t]], na.rm = T)/length(backtest_data$fc[backtest_data$favorite_money>=indicator[t]])
    n1[t] <-  length(backtest_data$fc[backtest_data$favorite_money>=indicator[t]])
  }
  roi_df <- data.frame(roi,n,indicator,totaln = length(unlist(backtest_data[,1])))
  roi_df1 <- data.frame(roi1,n1,indicator,totaln = length(unlist(backtest_data[,1])))
  udc_fmoney[i]  <- roi_df$indicator[which(roi_df$roi>.10)[1]]
  udc_fmoney_pct[i]  <- paste0(roi_df$n[which(roi_df$roi>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  fc_fmoney[i]  <- roi_df1$indicator[which(roi_df1$roi1>.10)[1]]
  fc_fmoney_pct[i]  <- paste0(roi_df1$n1[which(roi_df1$roi1>.10)[1]]," / ",length(unlist(backtest_data[,1])))
  
}

favorite_money_analysis <- data.frame(league=c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb"),
                                      udml_fmlmoney, 
                                      udml_fmlmoney_pct, 
                                      udc_fmlmoney, 
                                      udc_fmlmoney_pct, 
                                      udml_fmoney, 
                                      udml_fmoney_pct ,
                                      udc_fmoney, 
                                      udc_fmoney_pct,
                                      
                                      fml_fmlmoney, 
                                      fml_fmlmoney_pct,  
                                      fc_fmlmoney,  
                                      fc_fmlmoney_pct,
                                      fc_fmoney,
                                      fc_fmoney_pct,
                                      fml_fmoney,
                                      fml_fmoney_pct)

#### Today's games #####


todays_games_functions <- function(backtest_games,
                                   nhl_games_all,
                                   mlb_games_all,
                                   sec_cbb_games_all,
                                   acc_cbb_games_all,
                                   big10_cbb_games_all,
                                   big12_cbb_games_all,
                                   pac12_cbb_games_all,
                                   ivy_cbb_games_all,
                                   nfl_games_all,
                                   cfb_games_all,
                                   nba_games_all,
                                   home_money_analysis,
                                   away_money_analysis,
                                   home_sharp_analysis,
                                   away_sharp_analysis,
                                   home_public_analysis,
                                   away_public_analysis,
                                   favorite_money_analysis,
                                   underdog_money_analysis,
                                   favorite_sharp_analysis,
                                   underdog_sharp_analysis,
                                   favorite_public_analysis,
                                   underdog_public_analysis,
                                   year,
                                   nfl_week,
                                   cfb_week){
  
  
  
  
  leagues <- c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb")
  
  nhl_sharp_analysis_a <- NA
  nhl_public_analysis_a <- NA
  nhl_money_analysis_a <- NA
  nhl_sharp_analysis_h <-NA
  nhl_public_analysis_h <- NA
  nhl_money_analysis_h <- NA
  nhl_udf_games_today <- NA
  
  tryCatch({
    nhl_games_today <- todays_nhl_analysis(nhl_games_all)
    
    nhl_sharp_analysis_a <- nhl_games_today[[1]]
    nhl_public_analysis_a <- nhl_games_today[[2]]
    nhl_money_analysis_a <- nhl_games_today[[3]]
    nhl_sharp_analysis_h <- nhl_games_today[[4]]
    nhl_public_analysis_h <- nhl_games_today[[5]]
    nhl_money_analysis_h <- nhl_games_today[[6]]
    nhl_udf_games_today <- nhl_games_today[[7]]
    
    nhl_sharp_analysis_a$x <- 1
    nhl_public_analysis_a$x <- 1
    nhl_money_analysis_a$x <- 1
    nhl_sharp_analysis_h$x <- 1
    nhl_public_analysis_h$x <- 1
    nhl_money_analysis_h$x <- 1
    nhl_udf_games_today$x <- 1
    print("The NHL has games this week.")
  }, error=function(e){
    
    print("Nothing for the NHL this week.")
  })
  
  nba_sharp_analysis_a <- NA
  nba_public_analysis_a <- NA
  nba_money_analysis_a <- NA
  nba_sharp_analysis_h <-NA
  nba_public_analysis_h <- NA
  nba_money_analysis_h <- NA
  nba_udf_games_today <- NA
  
  tryCatch({
    nba_games_today <- todays_nba_analysis(nba_games_all)
    
    nba_sharp_analysis_a <- nba_games_today[[1]]
    nba_public_analysis_a <- nba_games_today[[2]]
    nba_money_analysis_a <- nba_games_today[[3]]
    nba_sharp_analysis_h <- nba_games_today[[4]]
    nba_public_analysis_h <- nba_games_today[[5]]
    nba_money_analysis_h <- nba_games_today[[6]]
    nba_udf_games_today <- nba_games_today[[7]]
    
    nba_sharp_analysis_a$x <- 2
    nba_public_analysis_a$x <- 2
    nba_money_analysis_a$x <- 2
    nba_sharp_analysis_h$x <- 2
    nba_public_analysis_h$x <- 2
    nba_money_analysis_h$x <- 2
    nba_udf_games_today$x <- 2
    print("The NBA has games this week.")
  }, error=function(e){
    
    print("Nothing for the NBA this week.")
  })
  
  cfb_sharp_analysis_a <- NA
  cfb_public_analysis_a <- NA
  cfb_money_analysis_a <- NA
  cfb_sharp_analysis_h <-NA
  cfb_public_analysis_h <- NA
  cfb_money_analysis_h <- NA
  cfb_udf_games_today <- NA
  
  tryCatch({
    cfb_games_today <- todays_cfb_analysis(cfb_games_all, cfb_week ,year)
    
    cfb_sharp_analysis_a <- cfb_games_today[[1]]
    cfb_public_analysis_a <- cfb_games_today[[2]]
    cfb_money_analysis_a <- cfb_games_today[[3]]
    cfb_sharp_analysis_h <- cfb_games_today[[4]]
    cfb_public_analysis_h <- cfb_games_today[[5]]
    cfb_money_analysis_h <- cfb_games_today[[6]]
    cfb_udf_games_today <- cfb_games_today[[7]]
    
    cfb_sharp_analysis_a$x <- 3
    cfb_public_analysis_a$x <- 3
    cfb_money_analysis_a$x <- 3
    cfb_sharp_analysis_h$x <- 3
    cfb_public_analysis_h$x <- 3
    cfb_money_analysis_h$x <- 3
    cfb_udf_games_today$x <- 3
    print("The NCAAF has games this week.")
    
  }, error=function(e){
    
    print("Nothing for the NCAAF this week.")
  })
  
  nfl_sharp_analysis_a <- NA
  nfl_public_analysis_a <- NA
  nfl_money_analysis_a <- NA
  nfl_sharp_analysis_h <-NA
  nfl_public_analysis_h <- NA
  nfl_money_analysis_h <- NA
  nfl_udf_games_today <- NA
  tryCatch({
    nfl_games_today <- todays_nfl_analysis(nfl_games_all, nfl_week ,year)
    
    nfl_sharp_analysis_a <- nfl_games_today[[1]]
    nfl_public_analysis_a <- nfl_games_today[[2]]
    nfl_money_analysis_a <- nfl_games_today[[3]]
    nfl_sharp_analysis_h <- nfl_games_today[[4]]
    nfl_public_analysis_h <- nfl_games_today[[5]]
    nfl_money_analysis_h <- nfl_games_today[[6]]
    nfl_udf_games_today <- nfl_games_today[[7]]
    
    nfl_sharp_analysis_a$x <- 4
    nfl_public_analysis_a$x <- 4
    nfl_money_analysis_a$x <- 4
    nfl_sharp_analysis_h$x <- 4
    nfl_public_analysis_h$x <- 4
    nfl_money_analysis_h$x <- 4
    nfl_udf_games_today$x <- 4
    print("The NFL has games this week.")
    
    
  }, error=function(e){
    
    print("Nothing for the NFL this week.")
  })
  
  #### College Basketball ####
  source("cbb today.R")
  
  
  mlb_sharp_analysis_a <- NA
  mlb_public_analysis_a <- NA
  mlb_money_analysis_a <- NA
  mlb_sharp_analysis_h <-NA
  mlb_public_analysis_h <- NA
  mlb_money_analysis_h <- NA
  mlb_udf_games_today <- NA
  tryCatch({
    mlb_games_today <- todays_mlb_analysis(mlb_games_all)
    
    mlb_sharp_analysis_a <- mlb_games_today[[1]]
    mlb_public_analysis_a <- mlb_games_today[[2]]
    mlb_money_analysis_a <- mlb_games_today[[3]]
    mlb_sharp_analysis_h <- mlb_games_today[[4]]
    mlb_public_analysis_h <- mlb_games_today[[5]]
    mlb_money_analysis_h <- mlb_games_today[[6]]
    mlb_udf_games_today <- mlb_games_today[[7]]
    
    mlb_sharp_analysis_a$x <- 16
    mlb_public_analysis_a$x <- 16
    mlb_money_analysis_a$x <- 16
    mlb_sharp_analysis_h$x <- 16
    mlb_public_analysis_h$x <- 16
    mlb_money_analysis_h$x <- 16
    mlb_udf_games_today$x <- 16
    
    print("The MLB has games this week.")
    
    
    
  }, error=function(e){
    
    print("Nothing for the MLB this week.")
    
  })
  
  
  
  
  
  away_sharp_analysis_today <- rbind(nfl_sharp_analysis_a,
                                     nba_sharp_analysis_a,
                                     nhl_sharp_analysis_a,
                                     cfb_sharp_analysis_a,
                                     mlb_sharp_analysis_a,
                                     acc_cbbsharp_analysis_a,
                                     sec_cbbsharp_analysis_a,
                                     big10_cbbsharp_analysis_a,
                                     big12_cbbsharp_analysis_a,
                                     pac12_cbbsharp_analysis_a,
                                     ivy_cbbsharp_analysis_a,
                                     bigsky_cbbsharp_analysis_a,
                                     bigeast_cbbsharp_analysis_a,
                                     a10_cbbsharp_analysis_a,
                                     aac_cbbsharp_analysis_a,
                                     cusa_cbbsharp_analysis_a, fill =TRUE)
  
  away_public_analysis_today <- rbind(nfl_public_analysis_a,
                                      nba_public_analysis_a,
                                      nhl_public_analysis_a,
                                      cfb_public_analysis_a,
                                      mlb_public_analysis_a,
                                      acc_cbbpublic_analysis_a,
                                      sec_cbbpublic_analysis_a,
                                      big10_cbbpublic_analysis_a,
                                      big12_cbbpublic_analysis_a,
                                      pac12_cbbpublic_analysis_a,
                                      ivy_cbbpublic_analysis_a,
                                      bigsky_cbbpublic_analysis_a,
                                      bigeast_cbbpublic_analysis_a,
                                      a10_cbbpublic_analysis_a,
                                      aac_cbbpublic_analysis_a,
                                      cusa_cbbpublic_analysis_a, fill =TRUE)
  
  away_money_analysis_today <- rbind(nfl_money_analysis_a,
                                     nba_money_analysis_a,
                                     nhl_money_analysis_a,
                                     cfb_money_analysis_a,
                                     mlb_money_analysis_a,
                                     acc_cbbmoney_analysis_a,
                                     sec_cbbmoney_analysis_a,
                                     big10_cbbmoney_analysis_a,
                                     big12_cbbmoney_analysis_a,
                                     pac12_cbbmoney_analysis_a,
                                     ivy_cbbmoney_analysis_a,
                                     bigsky_cbbmoney_analysis_a,
                                     bigeast_cbbmoney_analysis_a,
                                     a10_cbbmoney_analysis_a,
                                     aac_cbbmoney_analysis_a,
                                     cusa_cbbmoney_analysis_a, fill =TRUE)
  
  
  home_sharp_analysis_today <- rbind(nfl_sharp_analysis_h,
                                     nba_sharp_analysis_h,
                                     nhl_sharp_analysis_h,
                                     cfb_sharp_analysis_h,
                                     mlb_sharp_analysis_h,
                                     acc_cbbsharp_analysis_h,
                                     sec_cbbsharp_analysis_h,
                                     big10_cbbsharp_analysis_h,
                                     big12_cbbsharp_analysis_h,
                                     pac12_cbbsharp_analysis_h,
                                     ivy_cbbsharp_analysis_h,
                                     bigsky_cbbsharp_analysis_h,
                                     bigeast_cbbsharp_analysis_h,
                                     a10_cbbsharp_analysis_h,
                                     aac_cbbsharp_analysis_h,
                                     cusa_cbbsharp_analysis_h, fill =TRUE)
  
  home_public_analysis_today <- rbind(nfl_public_analysis_h,
                                      nba_public_analysis_h,
                                      nhl_public_analysis_h,
                                      cfb_public_analysis_h,
                                      mlb_public_analysis_h,
                                      acc_cbbpublic_analysis_h,
                                      sec_cbbpublic_analysis_h,
                                      big10_cbbpublic_analysis_h,
                                      big12_cbbpublic_analysis_h,
                                      pac12_cbbpublic_analysis_h,
                                      ivy_cbbpublic_analysis_h,
                                      bigsky_cbbpublic_analysis_h,
                                      bigeast_cbbpublic_analysis_h,
                                      a10_cbbpublic_analysis_h,
                                      aac_cbbpublic_analysis_h,
                                      cusa_cbbpublic_analysis_h, fill =TRUE)
  
  home_money_analysis_today <- rbind(nfl_money_analysis_h,
                                     nba_money_analysis_h,
                                     nhl_money_analysis_h,
                                     cfb_money_analysis_h,
                                     mlb_money_analysis_h,
                                     acc_cbbmoney_analysis_h,
                                     sec_cbbmoney_analysis_h,
                                     big10_cbbmoney_analysis_h,
                                     big12_cbbmoney_analysis_h,
                                     pac12_cbbmoney_analysis_h,
                                     ivy_cbbmoney_analysis_h,
                                     bigsky_cbbmoney_analysis_h,
                                     bigeast_cbbmoney_analysis_h,
                                     a10_cbbmoney_analysis_h,
                                     aac_cbbmoney_analysis_h,
                                     cusa_cbbmoney_analysis_h, fill =TRUE)
  
  udf_analysis_today <- rbind(nfl_udf_games_today,
                              nba_udf_games_today,
                              nhl_udf_games_today,
                              cfb_udf_games_today,
                              mlb_udf_games_today,
                              acc_cbbudf_games_today,
                              sec_cbbudf_games_today,
                              big10_cbbudf_games_today,
                              big12_cbbudf_games_today,
                              pac12_cbbudf_games_today,
                              ivy_cbbudf_games_today,
                              bigsky_cbbudf_games_today,
                              bigeast_cbbudf_games_today,
                              a10_cbbudf_games_today,
                              aac_cbbudf_games_today,
                              cusa_cbbudf_games_today, fill =TRUE)
  
  udf_analysis_today$est_time <- as.POSIXct(udf_analysis_today$time, format = "%Y-%m-%dT%H:%M:%OSZ")-(60*60*5)
  
  if(length(which(is.na(home_sharp_analysis_today$away_team)))==0){
    home_sharp_analysis_today <- home_sharp_analysis_today[order(home_sharp_analysis_today$est_time),]
  }else{
    home_sharp_analysis_today <- home_sharp_analysis_today[-which(is.na(home_sharp_analysis_today$away_team)),]
    home_sharp_analysis_today <- home_sharp_analysis_today[order(home_sharp_analysis_today$est_time),]
  }
  

  
  if(length(which(is.na(home_public_analysis_today$away_team)))==0){
    home_public_analysis_today <- home_public_analysis_today[order(home_public_analysis_today$est_time),]
  }else{
    home_public_analysis_today <- home_public_analysis_today[-which(is.na(home_public_analysis_today$away_team)),]
    home_public_analysis_today <- home_public_analysis_today[order(home_public_analysis_today$est_time),]
  }
  
  if(length(which(is.na(home_money_analysis_today$away_team)))==0){
    home_money_analysis_today <- home_money_analysis_today[order(home_money_analysis_today$est_time),]
  }else{
    home_money_analysis_today <- home_money_analysis_today[-which(is.na(home_money_analysis_today$away_team)),]
    home_money_analysis_today <- home_money_analysis_today[order(home_money_analysis_today$est_time),]
    
  }
  
  if(length(which(is.na(away_sharp_analysis_today$away_team)))==0){
    away_sharp_analysis_today <- away_sharp_analysis_today[order(away_sharp_analysis_today$est_time),]
  }else{
    away_sharp_analysis_today <- away_sharp_analysis_today[-which(is.na(away_sharp_analysis_today$away_team)),]
    away_sharp_analysis_today <- away_sharp_analysis_today[order(away_sharp_analysis_today$est_time),]
  }
  
  if(length(which(is.na(away_public_analysis_today$away_team)))==0){
    away_public_analysis_today <- away_public_analysis_today[order(away_public_analysis_today$est_time),]
  }else{
    away_public_analysis_today <- away_public_analysis_today[-which(is.na(away_public_analysis_today$away_team)),]
    away_public_analysis_today <- away_public_analysis_today[order(away_public_analysis_today$est_time),]
  }
  
  if(length(which(is.na(away_money_analysis_today$away_team)))==0){
    away_money_analysis_today <- away_money_analysis_today[order(away_money_analysis_today$est_time),]
  }else{
    away_money_analysis_today <- away_money_analysis_today[-which(is.na(away_money_analysis_today$away_team)),]
    away_money_analysis_today <- away_money_analysis_today[order(away_money_analysis_today$est_time),]
  }
  
  if(length(which(is.na(udf_analysis_today$favorite)))==0){
    udf_analysis_today <- udf_analysis_today[order(udf_analysis_today$est_time),]
  }else{
    udf_analysis_today <- udf_analysis_today[-which(is.na(udf_analysis_today$favorite)),]
    udf_analysis_today <- udf_analysis_today[order(udf_analysis_today$est_time),]
  }
  
  
  
  
  
  
  
  
 
  
  todays_games <- home_sharp_analysis_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml",
                                               "away_spread_line","home_spread_line","est_time","x")]
  
  
  for(p in 1:length(todays_games$away_team)){
    todays_games$league[p] <- leagues[todays_games$x[p]]
  }
  
  
  todays_games$spread_bets_away <- 0
  todays_games$spread_bets_home <- 0
  todays_games$ml_bets_away <- 0
  todays_games$ml_bets_home <- 0
  
  todays_games$statements_away_spread <- ""
  todays_games$statements_home_spread <- ""
  todays_games$statements_away_ml <- ""
  todays_games$statements_home_ml <- ""
  
  todays_games_udf <- udf_analysis_today[,c("favorite","underdog","favorite_spread","underdog_spread","favorite_ml","underdog_ml",
                                               "favorite_spread_line","underdog_spread_line","est_time","x")]
  
  
  for(p in 1:length(todays_games_udf$favorite)){
    todays_games_udf$league[p] <- leagues[todays_games_udf$x[p]]
  }
  
  todays_games_udf$spread_bets_underdog <- 0
  todays_games_udf$spread_bets_favorite <- 0
  todays_games_udf$ml_bets_underdog <- 0
  todays_games_udf$ml_bets_favorite <- 0
  
  todays_games_udf$statements_underdog_spread <- ""
  todays_games_udf$statements_favorite_spread <- ""
  todays_games_udf$statements_underdog_ml <- ""
  todays_games_udf$statements_favorite_ml <- ""
  
  #### Home sharp ####
  
  for(j in 1:length(home_sharp_analysis_today$away_team)){
    
    league <- leagues[home_sharp_analysis_today$x[j]]
    
    hsd_s <- home_sharp_analysis_today$hsd_s[j]
    hsd_ml <- home_sharp_analysis_today$hsd_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(home_sharp_analysis$hml_hsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_sharp_analysis$hml_hsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_sharp_analysis_today$home_team[j],
                            ", against the ",home_sharp_analysis_today$away_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",home_sharp_analysis_today$home_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," spread, and home teams receiving ",home_sharp_analysis$hml_hsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps on the spread have an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$hml_hsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_sharp_analysis$hml_hmlsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_sharp_analysis$hml_hmlsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_sharp_analysis_today$home_team[j],
                            ", against the ",home_sharp_analysis_today$away_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",home_sharp_analysis_today$home_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_sharp_analysis$hml_hmlsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$hml_hmlsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_sharp_analysis$hc_hsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_sharp_analysis$hc_hsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_sharp_analysis_today$home_team[j],
                            ", against the ",home_sharp_analysis_today$away_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the spread at ",home_sharp_analysis_today$home_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," spread, and home teams receiving ",home_sharp_analysis$hc_hsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps on the spread have an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$hc_hsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_sharp_analysis$hc_hmlsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_sharp_analysis$hc_hmlsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_sharp_analysis_today$home_team[j],
                            ", against the ",home_sharp_analysis_today$away_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the spread at ",home_sharp_analysis_today$home_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_sharp_analysis$hc_hmlsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$hc_hmlsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(home_sharp_analysis$aml_hsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_sharp_analysis$aml_hsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_sharp_analysis_today$away_team[j],
                            ", against the ",home_sharp_analysis_today$home_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",home_sharp_analysis_today$away_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_sharp_analysis$aml_hsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$aml_hsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_sharp_analysis$aml_hmlsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_sharp_analysis$aml_hmlsharp[home_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_sharp_analysis_today$away_team[j],
                             ", against the ",home_sharp_analysis_today$home_team[j]," at ",home_sharp_analysis_today$est_time[j],
                             " on the moneyline at ",home_sharp_analysis_today$away_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                             home_sharp_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_sharp_analysis$aml_hmlsharp[home_sharp_analysis$league==league],
                             " percent more from the sharps, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                             home_sharp_analysis$aml_hmlsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_sharp_analysis$ac_hsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_sharp_analysis$ac_hsharp[home_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_sharp_analysis_today$away_team[j],
                             ", against the ",home_sharp_analysis_today$home_team[j]," at ",home_sharp_analysis_today$est_time[j],
                             " on the spread at ",home_sharp_analysis_today$away_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                             home_sharp_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_sharp_analysis$ac_hsharp[home_sharp_analysis$league==league],
                             " percent more from the sharps, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                             home_sharp_analysis$ac_hsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_sharp_analysis$ac_hmlsharp[home_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_sharp_analysis$ac_hmlsharp[home_sharp_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_sharp_analysis_today$away_team[j],
                            ", against the ",home_sharp_analysis_today$home_team[j]," at ",home_sharp_analysis_today$est_time[j],
                            " on the spread at ",home_sharp_analysis_today$away_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            home_sharp_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_sharp_analysis$ac_hmlsharp[home_sharp_analysis$league==league],
                            " percent more from the sharps, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                            home_sharp_analysis$ac_hmlsharp_pct[home_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
  }
  
  #### Home public ####
  
  for(j in 1:length(home_public_analysis_today$away_team)){
    
    league <- leagues[home_public_analysis_today$x[j]]
    
    hsd_s <- home_public_analysis_today$public_home[j]
    hsd_ml <- home_public_analysis_today$public_home_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(home_public_analysis$hml_hpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_public_analysis$hml_hpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_public_analysis_today$home_team[j],
                            ", against the ",home_public_analysis_today$away_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the moneyline at ",home_public_analysis_today$home_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            home_public_analysis_today$home_team[j]," spread, and home teams receiving ",home_public_analysis$hml_hpublic[home_public_analysis$league==league],
                            " percent from the publics on the spread have an eROI of 10% or better over a sample size of ",
                            home_public_analysis$hml_hpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_public_analysis$hml_hmlpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_public_analysis$hml_hmlpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_public_analysis_today$home_team[j],
                            ", against the ",home_public_analysis_today$away_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the moneyline at ",home_public_analysis_today$home_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                            home_public_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_public_analysis$hml_hmlpublic[home_public_analysis$league==league],
                            " percent from the publics on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_public_analysis$hml_hmlpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_public_analysis$hc_hpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_public_analysis$hc_hpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_public_analysis_today$home_team[j],
                            ", against the ",home_public_analysis_today$away_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the spread at ",home_public_analysis_today$home_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                            home_public_analysis_today$home_team[j]," spread, and home teams receiving ",home_public_analysis$hc_hpublic[home_public_analysis$league==league],
                            " percent from the publics on the spread have an eROI of 10% or better over a sample size of ",
                            home_public_analysis$hc_hpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_public_analysis$hc_hmlpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_public_analysis$hc_hmlpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_public_analysis_today$home_team[j],
                            ", against the ",home_public_analysis_today$away_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the spread at ",home_public_analysis_today$home_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            home_public_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_public_analysis$hc_hmlpublic[home_public_analysis$league==league],
                            " percent from the publics on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_public_analysis$hc_hmlpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(home_public_analysis$aml_hpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_public_analysis$aml_hpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_public_analysis_today$away_team[j],
                            ", against the ",home_public_analysis_today$home_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the moneyline at ",home_public_analysis_today$away_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            home_public_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_public_analysis$aml_hpublic[home_public_analysis$league==league],
                            " percent from the publics, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                            home_public_analysis$aml_hpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_public_analysis$aml_hmlpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_public_analysis$aml_hmlpublic[home_public_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_public_analysis_today$away_team[j],
                             ", against the ",home_public_analysis_today$home_team[j]," at ",home_public_analysis_today$est_time[j],
                             " on the moneyline at ",home_public_analysis_today$away_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                             home_public_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_public_analysis$aml_hmlpublic[home_public_analysis$league==league],
                             " percent from the publics, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                             home_public_analysis$aml_hmlpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_public_analysis$ac_hpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_public_analysis$ac_hpublic[home_public_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_public_analysis_today$away_team[j],
                             ", against the ",home_public_analysis_today$home_team[j]," at ",home_public_analysis_today$est_time[j],
                             " on the spread at ",home_public_analysis_today$away_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                             home_public_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_public_analysis$ac_hpublic[home_public_analysis$league==league],
                             " percent from the publics, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                             home_public_analysis$ac_hpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_public_analysis$ac_hmlpublic[home_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_public_analysis$ac_hmlpublic[home_public_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_public_analysis_today$away_team[j],
                            ", against the ",home_public_analysis_today$home_team[j]," at ",home_public_analysis_today$est_time[j],
                            " on the spread at ",home_public_analysis_today$away_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            home_public_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_public_analysis$ac_hmlpublic[home_public_analysis$league==league],
                            " percent from the publics, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                            home_public_analysis$ac_hmlpublic_pct[home_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
  }
  
  #### Home money ####
  
  
  for(j in 1:length(home_money_analysis_today$away_team)){
    
    league <- leagues[home_money_analysis_today$x[j]]
    
    hsd_s <- home_money_analysis_today$money_home[j]
    hsd_ml <- home_money_analysis_today$money_home_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(home_money_analysis$hml_hmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_money_analysis$hml_hmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_money_analysis_today$home_team[j],
                            ", against the ",home_money_analysis_today$away_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the moneyline at ",home_money_analysis_today$home_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            home_money_analysis_today$home_team[j]," spread, and home teams receiving ",home_money_analysis$hml_hmoney[home_money_analysis$league==league],
                            " percent from the moneys on the spread have an eROI of 10% or better over a sample size of ",
                            home_money_analysis$hml_hmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_money_analysis$hml_hmlmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_money_analysis$hml_hmlmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_money_analysis_today$home_team[j],
                            ", against the ",home_money_analysis_today$away_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the moneyline at ",home_money_analysis_today$home_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            home_money_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_money_analysis$hml_hmlmoney[home_money_analysis$league==league],
                            " percent from the moneys on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_money_analysis$hml_hmlmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_money_analysis$hc_hmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_money_analysis$hc_hmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_money_analysis_today$home_team[j],
                            ", against the ",home_money_analysis_today$away_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the spread at ",home_money_analysis_today$home_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                            home_money_analysis_today$home_team[j]," spread, and home teams receiving ",home_money_analysis$hc_hmoney[home_money_analysis$league==league],
                            " percent from the moneys on the spread have an eROI of 10% or better over a sample size of ",
                            home_money_analysis$hc_hmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_money_analysis$hc_hmlmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_money_analysis$hc_hmlmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",home_money_analysis_today$home_team[j],
                            ", against the ",home_money_analysis_today$away_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the spread at ",home_money_analysis_today$home_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            home_money_analysis_today$home_team[j]," moneyline, and home teams receiving ",home_money_analysis$hc_hmlmoney[home_money_analysis$league==league],
                            " percent from the moneys on the moneyline have an eROI of 10% or better over a sample size of ",
                            home_money_analysis$hc_hmlmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(home_money_analysis$aml_hmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_money_analysis$aml_hmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_money_analysis_today$away_team[j],
                            ", against the ",home_money_analysis_today$home_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the moneyline at ",home_money_analysis_today$away_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            home_money_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_money_analysis$aml_hmoney[home_money_analysis$league==league],
                            " percent from the moneys, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                            home_money_analysis$aml_hmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_money_analysis$aml_hmlmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_money_analysis$aml_hmlmoney[home_money_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_money_analysis_today$away_team[j],
                             ", against the ",home_money_analysis_today$home_team[j]," at ",home_money_analysis_today$est_time[j],
                             " on the moneyline at ",home_money_analysis_today$away_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                             home_money_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_money_analysis$aml_hmlmoney[home_money_analysis$league==league],
                             " percent from the moneys, betting the moneyline for the away team has an eROI of 10% or better over a sample size of ",
                             home_money_analysis$aml_hmlmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(home_money_analysis$ac_hmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=home_money_analysis$ac_hmoney[home_money_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",home_money_analysis_today$away_team[j],
                             ", against the ",home_money_analysis_today$home_team[j]," at ",home_money_analysis_today$est_time[j],
                             " on the spread at ",home_money_analysis_today$away_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                             home_money_analysis_today$home_team[j]," spread, and when home teams are receiving ",home_money_analysis$ac_hmoney[home_money_analysis$league==league],
                             " percent from the moneys, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                             home_money_analysis$ac_hmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(home_money_analysis$ac_hmlmoney[home_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=home_money_analysis$ac_hmlmoney[home_money_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",home_money_analysis_today$away_team[j],
                            ", against the ",home_money_analysis_today$home_team[j]," at ",home_money_analysis_today$est_time[j],
                            " on the spread at ",home_money_analysis_today$away_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            home_money_analysis_today$home_team[j]," moneyline, and when home teams are receiving ",home_money_analysis$ac_hmlmoney[home_money_analysis$league==league],
                            " percent from the moneys, betting the spread for the away team has an eROI of 10% or better over a sample size of ",
                            home_money_analysis$ac_hmlmoney_pct[home_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
  }
  
  #### Away Sharp ####
  
  
  for(j in 1:length(away_sharp_analysis_today$away_team)){
    
    league <- leagues[away_sharp_analysis_today$x[j]]
    
    hsd_s <- away_sharp_analysis_today$asd_s[j]
    hsd_ml <- away_sharp_analysis_today$asd_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(away_sharp_analysis$hml_asharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_sharp_analysis$hml_asharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_sharp_analysis_today$home_team[j],
                            ", against the ",away_sharp_analysis_today$away_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",away_sharp_analysis_today$home_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," spread, and when away teams are receiving ",away_sharp_analysis$hml_asharp[away_sharp_analysis$league==league],
                            " percent more from the sharps on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$hml_asharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_sharp_analysis$hml_amlsharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_sharp_analysis$hml_amlsharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_sharp_analysis_today$home_team[j],
                            ", against the ",away_sharp_analysis_today$away_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",away_sharp_analysis_today$home_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_sharp_analysis$hml_amlsharp[away_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$hml_amlsharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_sharp_analysis$hc_asharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_sharp_analysis$hc_asharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_sharp_analysis_today$home_team[j],
                            ", against the ",away_sharp_analysis_today$away_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the spread at ",away_sharp_analysis_today$home_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," spread, and when the away team is receiving ",away_sharp_analysis$hc_asharp[away_sharp_analysis$league==league],
                            " percent more from the sharps on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$hc_asharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_sharp_analysis$hc_amlsharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_sharp_analysis$hc_amlsharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_sharp_analysis_today$home_team[j],
                            ", against the ",away_sharp_analysis_today$away_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the spread at ",away_sharp_analysis_today$home_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_sharp_analysis$hc_amlsharp[away_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$hc_amlsharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(away_sharp_analysis$aml_asharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_sharp_analysis$aml_asharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_sharp_analysis_today$away_team[j],
                            ", against the ",away_sharp_analysis_today$home_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the moneyline at ",away_sharp_analysis_today$away_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," spread, and away teams receiving ",away_sharp_analysis$aml_asharp[away_sharp_analysis$league==league],
                            " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$aml_asharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_sharp_analysis$aml_amlsharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_sharp_analysis$aml_amlsharp[away_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_sharp_analysis_today$away_team[j],
                             ", against the ",away_sharp_analysis_today$home_team[j]," at ",away_sharp_analysis_today$est_time[j],
                             " on the moneyline at ",away_sharp_analysis_today$away_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                             away_sharp_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_sharp_analysis$aml_amlsharp[away_sharp_analysis$league==league],
                             " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                             away_sharp_analysis$aml_amlsharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_sharp_analysis$ac_asharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_sharp_analysis$ac_asharp[away_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_sharp_analysis_today$away_team[j],
                             ", against the ",away_sharp_analysis_today$home_team[j]," at ",away_sharp_analysis_today$est_time[j],
                             " on the spread at ",away_sharp_analysis_today$away_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                             away_sharp_analysis_today$away_team[j]," spread, and away teams receiving ",away_sharp_analysis$ac_asharp[away_sharp_analysis$league==league],
                             " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                             away_sharp_analysis$ac_asharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_sharp_analysis$ac_amlsharp[away_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_sharp_analysis$ac_amlsharp[away_sharp_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_sharp_analysis_today$away_team[j],
                            ", against the ",away_sharp_analysis_today$home_team[j]," at ",away_sharp_analysis_today$est_time[j],
                            " on the spread at ",away_sharp_analysis_today$away_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            away_sharp_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_sharp_analysis$ac_amlsharp[away_sharp_analysis$league==league],
                            " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                            away_sharp_analysis$ac_amlsharp_pct[away_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
  }
  
  #### Away Public ####
  
  for(j in 1:length(away_public_analysis_today$away_team)){
    
    league <- leagues[away_public_analysis_today$x[j]]
    
    hsd_s <- away_public_analysis_today$public_away[j]
    hsd_ml <- away_public_analysis_today$public_away_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(away_public_analysis$hml_apublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_public_analysis$hml_apublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_public_analysis_today$home_team[j],
                            ", against the ",away_public_analysis_today$away_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the moneyline at ",away_public_analysis_today$home_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            away_public_analysis_today$away_team[j]," spread, and when away teams are receiving ",away_public_analysis$hml_apublic[away_public_analysis$league==league],
                            " percent from the publics on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_public_analysis$hml_apublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_public_analysis$hml_amlpublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_public_analysis$hml_amlpublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_public_analysis_today$home_team[j],
                            ", against the ",away_public_analysis_today$away_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the moneyline at ",away_public_analysis_today$home_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                            away_public_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_public_analysis$hml_amlpublic[away_public_analysis$league==league],
                            " percent from the publics on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_public_analysis$hml_amlpublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_public_analysis$hc_apublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_public_analysis$hc_apublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_public_analysis_today$home_team[j],
                            ", against the ",away_public_analysis_today$away_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the spread at ",away_public_analysis_today$home_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                            away_public_analysis_today$away_team[j]," spread, and when the away team is receiving ",away_public_analysis$hc_apublic[away_public_analysis$league==league],
                            " percent from the publics on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_public_analysis$hc_apublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_public_analysis$hc_amlpublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_public_analysis$hc_amlpublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_public_analysis_today$home_team[j],
                            ", against the ",away_public_analysis_today$away_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the spread at ",away_public_analysis_today$home_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            away_public_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_public_analysis$hc_amlpublic[away_public_analysis$league==league],
                            " percent from the publics on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_public_analysis$hc_amlpublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(away_public_analysis$aml_apublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_public_analysis$aml_apublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_public_analysis_today$away_team[j],
                            ", against the ",away_public_analysis_today$home_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the moneyline at ",away_public_analysis_today$away_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            away_public_analysis_today$away_team[j]," spread, and away teams receiving ",away_public_analysis$aml_apublic[away_public_analysis$league==league],
                            " percent from the publics have an eROI of 10% or better over a sample size of ",
                            away_public_analysis$aml_apublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_public_analysis$aml_amlpublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_public_analysis$aml_amlpublic[away_public_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_public_analysis_today$away_team[j],
                             ", against the ",away_public_analysis_today$home_team[j]," at ",away_public_analysis_today$est_time[j],
                             " on the moneyline at ",away_public_analysis_today$away_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                             away_public_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_public_analysis$aml_amlpublic[away_public_analysis$league==league],
                             " percent from the publics have an eROI of 10% or better over a sample size of ",
                             away_public_analysis$aml_amlpublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_public_analysis$ac_apublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_public_analysis$ac_apublic[away_public_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_public_analysis_today$away_team[j],
                             ", against the ",away_public_analysis_today$home_team[j]," at ",away_public_analysis_today$est_time[j],
                             " on the spread at ",away_public_analysis_today$away_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                             away_public_analysis_today$away_team[j]," spread, and away teams receiving ",away_public_analysis$ac_apublic[away_public_analysis$league==league],
                             " percent from the publics have an eROI of 10% or better over a sample size of ",
                             away_public_analysis$ac_apublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_public_analysis$ac_amlpublic[away_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_public_analysis$ac_amlpublic[away_public_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_public_analysis_today$away_team[j],
                            ", against the ",away_public_analysis_today$home_team[j]," at ",away_public_analysis_today$est_time[j],
                            " on the spread at ",away_public_analysis_today$away_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            away_public_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_public_analysis$ac_amlpublic[away_public_analysis$league==league],
                            " percent from the publics have an eROI of 10% or better over a sample size of ",
                            away_public_analysis$ac_amlpublic_pct[away_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
      
    }
  }
  
  #### Away Money ####
  
  for(j in 1:length(away_money_analysis_today$away_team)){
    
    league <- leagues[away_money_analysis_today$x[j]]
    
    hsd_s <- away_money_analysis_today$money_away[j]
    hsd_ml <- away_money_analysis_today$money_away_ml[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(away_money_analysis$hml_amoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_money_analysis$hml_amoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_money_analysis_today$home_team[j],
                            ", against the ",away_money_analysis_today$away_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the moneyline at ",away_money_analysis_today$home_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            away_money_analysis_today$away_team[j]," spread, and when away teams are receiving ",away_money_analysis$hml_amoney[away_money_analysis$league==league],
                            " percent from the moneys on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_money_analysis$hml_amoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_money_analysis$hml_amlmoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_money_analysis$hml_amlmoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_money_analysis_today$home_team[j],
                            ", against the ",away_money_analysis_today$away_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the moneyline at ",away_money_analysis_today$home_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            away_money_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_money_analysis$hml_amlmoney[away_money_analysis$league==league],
                            " percent from the moneys on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_money_analysis$hml_amlmoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_ml[j] <- paste0(todays_games$statements_home_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_home[j] <- todays_games$ml_bets_home[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_money_analysis$hc_amoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_money_analysis$hc_amoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_money_analysis_today$home_team[j],
                            ", against the ",away_money_analysis_today$away_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the spread at ",away_money_analysis_today$home_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                            away_money_analysis_today$away_team[j]," spread, and when the away team is receiving ",away_money_analysis$hc_amoney[away_money_analysis$league==league],
                            " percent from the moneys on the spread, the home team has an eROI of 10% or better over a sample size of ",
                            away_money_analysis$hc_amoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_money_analysis$hc_amlmoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_money_analysis$hc_amlmoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the home team, the ",away_money_analysis_today$home_team[j],
                            ", against the ",away_money_analysis_today$away_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the spread at ",away_money_analysis_today$home_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            away_money_analysis_today$away_team[j]," moneyline, and when the away team is receiving ",away_money_analysis$hc_amlmoney[away_money_analysis$league==league],
                            " percent from the moneys on the moneyline, the home team has an eROI of 10% or better over a sample size of ",
                            away_money_analysis$hc_amlmoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_home_spread[j] <- paste0(todays_games$statements_home_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_home[j] <- todays_games$spread_bets_home[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(away_money_analysis$aml_amoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_money_analysis$aml_amoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_money_analysis_today$away_team[j],
                            ", against the ",away_money_analysis_today$home_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the moneyline at ",away_money_analysis_today$away_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            away_money_analysis_today$away_team[j]," spread, and away teams receiving ",away_money_analysis$aml_amoney[away_money_analysis$league==league],
                            " percent from the moneys have an eROI of 10% or better over a sample size of ",
                            away_money_analysis$aml_amoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_money_analysis$aml_amlmoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_money_analysis$aml_amlmoney[away_money_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_money_analysis_today$away_team[j],
                             ", against the ",away_money_analysis_today$home_team[j]," at ",away_money_analysis_today$est_time[j],
                             " on the moneyline at ",away_money_analysis_today$away_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                             away_money_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_money_analysis$aml_amlmoney[away_money_analysis$league==league],
                             " percent from the moneys have an eROI of 10% or better over a sample size of ",
                             away_money_analysis$aml_amlmoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_ml[j] <- paste0(todays_games$statements_away_ml[j]," \n\n",statement)
      statement <- 0
      todays_games$ml_bets_away[j] <- todays_games$ml_bets_away[j] + 1
    }
    if(is.na(hsd_s) | is.na(away_money_analysis$ac_amoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=away_money_analysis$ac_amoney[away_money_analysis$league==league]){
        statement <-  paste0("The bet is the away team, the ",away_money_analysis_today$away_team[j],
                             ", against the ",away_money_analysis_today$home_team[j]," at ",away_money_analysis_today$est_time[j],
                             " on the spread at ",away_money_analysis_today$away_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                             away_money_analysis_today$away_team[j]," spread, and away teams receiving ",away_money_analysis$ac_amoney[away_money_analysis$league==league],
                             " percent from the moneys have an eROI of 10% or better over a sample size of ",
                             away_money_analysis$ac_amoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
    if(is.na(hsd_ml) | is.na(away_money_analysis$ac_amlmoney[away_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=away_money_analysis$ac_amlmoney[away_money_analysis$league==league]){
        statement <- paste0("The bet is the away team, the ",away_money_analysis_today$away_team[j],
                            ", against the ",away_money_analysis_today$home_team[j]," at ",away_money_analysis_today$est_time[j],
                            " on the spread at ",away_money_analysis_today$away_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            away_money_analysis_today$away_team[j]," moneyline, and away teams receiving ",away_money_analysis$ac_amlmoney[away_money_analysis$league==league],
                            " percent from the moneys have an eROI of 10% or better over a sample size of ",
                            away_money_analysis$ac_amlmoney_pct[away_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games$statements_away_spread[j] <- paste0(todays_games$statements_away_spread[j]," \n\n",statement)
      statement <- 0
      todays_games$spread_bets_away[j] <- todays_games$spread_bets_away[j] + 1
    }
  }
  
  #### Favorite sharp ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$favorite_sharp[j]
    hsd_ml <- udf_analysis_today$mlfavorite_sharp[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(favorite_sharp_analysis$fml_fsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_sharp_analysis$fml_fsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_sharp_analysis$fml_fsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$fml_fsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_sharp_analysis$fml_fmlsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_sharp_analysis$fml_fmlsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_sharp_analysis$fml_fmlsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$fml_fmlsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_sharp_analysis$fc_fsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_sharp_analysis$fc_fsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_sharp_analysis$fc_fsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$fc_fsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_sharp_analysis$fc_fmlsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_sharp_analysis$fc_fmlsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_sharp_analysis$fc_fmlsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$fc_fmlsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(favorite_sharp_analysis$udml_fsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_sharp_analysis$udml_fsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_sharp_analysis$udml_fsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$udml_fsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_sharp_analysis$udml_fmlsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_sharp_analysis$udml_fmlsharp[favorite_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                             udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_sharp_analysis$udml_fmlsharp[favorite_sharp_analysis$league==league],
                             " percent more from the sharps, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_sharp_analysis$udml_fmlsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_sharp_analysis$udc_fsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_sharp_analysis$udc_fsharp[favorite_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                             udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_sharp_analysis$udc_fsharp[favorite_sharp_analysis$league==league],
                             " percent more from the sharps, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_sharp_analysis$udc_fsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_sharp_analysis$udc_fmlsharp[favorite_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_sharp_analysis$udc_fmlsharp[favorite_sharp_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_sharp_analysis$udc_fmlsharp[favorite_sharp_analysis$league==league],
                            " percent more from the sharps, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_sharp_analysis$udc_fmlsharp_pct[favorite_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  #### Underdog sharp ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$underdog_sharp[j]
    hsd_ml <- udf_analysis_today$mlunderdog_sharp[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(underdog_sharp_analysis$fml_udsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_sharp_analysis$fml_udsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_sharp_analysis$fml_udsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$fml_udsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_sharp_analysis$fml_udmlsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_sharp_analysis$fml_udmlsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_sharp_analysis$fml_udmlsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$fml_udmlsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_sharp_analysis$fc_udsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_sharp_analysis$fc_udsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_sharp_analysis$fc_udsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$fc_udsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_sharp_analysis$fc_udmlsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_sharp_analysis$fc_udmlsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_sharp_analysis$fc_udmlsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$fc_udmlsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(underdog_sharp_analysis$udml_udsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_sharp_analysis$udml_udsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the sharps are ",hsd_s," percent more on the ",
                            udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_sharp_analysis$udml_udsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$udml_udsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_sharp_analysis$udml_udmlsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_sharp_analysis$udml_udmlsharp[underdog_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                             udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_sharp_analysis$udml_udmlsharp[underdog_sharp_analysis$league==league],
                             " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                             underdog_sharp_analysis$udml_udmlsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_sharp_analysis$udc_udsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_sharp_analysis$udc_udsharp[underdog_sharp_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the sharps are ",hsd_s," percent more on the ",
                             udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_sharp_analysis$udc_udsharp[underdog_sharp_analysis$league==league],
                             " percent more from the sharps have has an eROI of 10% or better over a sample size of ",
                             underdog_sharp_analysis$udc_udsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_sharp_analysis$udc_udmlsharp[underdog_sharp_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_sharp_analysis$udc_udmlsharp[underdog_sharp_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the sharps are ",hsd_ml," percent more on the ",
                            udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_sharp_analysis$udc_udmlsharp[underdog_sharp_analysis$league==league],
                            " percent more from the sharps have an eROI of 10% or better over a sample size of ",
                            underdog_sharp_analysis$udc_udmlsharp_pct[underdog_sharp_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  #### Favorite public ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$favorite_public[j]
    hsd_ml <- udf_analysis_today$mlfavorite_public[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(favorite_public_analysis$fml_fpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_public_analysis$fml_fpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_public_analysis$fml_fpublic[favorite_public_analysis$league==league],
                            " percent from the publics on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$fml_fpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_public_analysis$fml_fmlpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_public_analysis$fml_fmlpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_public_analysis$fml_fmlpublic[favorite_public_analysis$league==league],
                            " percent from the publics on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$fml_fmlpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_public_analysis$fc_fpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_public_analysis$fc_fpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_public_analysis$fc_fpublic[favorite_public_analysis$league==league],
                            " percent from the publics on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$fc_fpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_public_analysis$fc_fmlpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_public_analysis$fc_fmlpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_public_analysis$fc_fmlpublic[favorite_public_analysis$league==league],
                            " percent from the publics on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$fc_fmlpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(favorite_public_analysis$udml_fpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_public_analysis$udml_fpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_public_analysis$udml_fpublic[favorite_public_analysis$league==league],
                            " percent from the publics, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$udml_fpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_public_analysis$udml_fmlpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_public_analysis$udml_fmlpublic[favorite_public_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                             udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_public_analysis$udml_fmlpublic[favorite_public_analysis$league==league],
                             " percent from the publics, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_public_analysis$udml_fmlpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_public_analysis$udc_fpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_public_analysis$udc_fpublic[favorite_public_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                             udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_public_analysis$udc_fpublic[favorite_public_analysis$league==league],
                             " percent from the publics, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_public_analysis$udc_fpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_public_analysis$udc_fmlpublic[favorite_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_public_analysis$udc_fmlpublic[favorite_public_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_public_analysis$udc_fmlpublic[favorite_public_analysis$league==league],
                            " percent from the publics, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_public_analysis$udc_fmlpublic_pct[favorite_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  #### Underdog public ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$underdog_public[j]
    hsd_ml <- udf_analysis_today$mlunderdog_public[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(underdog_public_analysis$fml_udpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_public_analysis$fml_udpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_public_analysis$fml_udpublic[underdog_public_analysis$league==league],
                            " percent from the publics on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$fml_udpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_public_analysis$fml_udmlpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_public_analysis$fml_udmlpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_public_analysis$fml_udmlpublic[underdog_public_analysis$league==league],
                            " percent from the publics on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$fml_udmlpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_public_analysis$fc_udpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_public_analysis$fc_udpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_public_analysis$fc_udpublic[underdog_public_analysis$league==league],
                            " percent from the publics on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$fc_udpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_public_analysis$fc_udmlpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_public_analysis$fc_udmlpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_public_analysis$fc_udmlpublic[underdog_public_analysis$league==league],
                            " percent from the publics on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$fc_udmlpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(underdog_public_analysis$udml_udpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_public_analysis$udml_udpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the publics are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_public_analysis$udml_udpublic[underdog_public_analysis$league==league],
                            " percent from the publics have an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$udml_udpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_public_analysis$udml_udmlpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_public_analysis$udml_udmlpublic[underdog_public_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the publics are ",hsd_ml," percent on the ",
                             udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_public_analysis$udml_udmlpublic[underdog_public_analysis$league==league],
                             " percent from the publics have an eROI of 10% or better over a sample size of ",
                             underdog_public_analysis$udml_udmlpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_public_analysis$udc_udpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_public_analysis$udc_udpublic[underdog_public_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the publics are ",hsd_s," percent on the ",
                             udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_public_analysis$udc_udpublic[underdog_public_analysis$league==league],
                             " percent from the publics have has an eROI of 10% or better over a sample size of ",
                             underdog_public_analysis$udc_udpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_public_analysis$udc_udmlpublic[underdog_public_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_public_analysis$udc_udmlpublic[underdog_public_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the publics are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_public_analysis$udc_udmlpublic[underdog_public_analysis$league==league],
                            " percent from the publics have an eROI of 10% or better over a sample size of ",
                            underdog_public_analysis$udc_udmlpublic_pct[underdog_public_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  #### Favorite money ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$favorite_money[j]
    hsd_ml <- udf_analysis_today$mlfavorite_money[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(favorite_money_analysis$fml_fmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_money_analysis$fml_fmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_money_analysis$fml_fmoney[favorite_money_analysis$league==league],
                            " percent from the moneys on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$fml_fmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_money_analysis$fml_fmlmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_money_analysis$fml_fmlmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_money_analysis$fml_fmlmoney[favorite_money_analysis$league==league],
                            " percent from the moneys on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$fml_fmlmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_money_analysis$fc_fmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_money_analysis$fc_fmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and favorites receiving ",favorite_money_analysis$fc_fmoney[favorite_money_analysis$league==league],
                            " percent from the moneys on the spread have an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$fc_fmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_money_analysis$fc_fmlmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_money_analysis$fc_fmlmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and favorites receiving ",favorite_money_analysis$fc_fmlmoney[favorite_money_analysis$league==league],
                            " percent from the moneys on the moneyline have an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$fc_fmlmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(favorite_money_analysis$udml_fmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_money_analysis$udml_fmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_money_analysis$udml_fmoney[favorite_money_analysis$league==league],
                            " percent from the moneys, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$udml_fmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_money_analysis$udml_fmlmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_money_analysis$udml_fmlmoney[favorite_money_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                             udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_money_analysis$udml_fmlmoney[favorite_money_analysis$league==league],
                             " percent from the moneys, betting the moneyline for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_money_analysis$udml_fmlmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(favorite_money_analysis$udc_fmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=favorite_money_analysis$udc_fmoney[favorite_money_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                             udf_analysis_today$favorite[j]," spread, and when favorites are receiving ",favorite_money_analysis$udc_fmoney[favorite_money_analysis$league==league],
                             " percent from the moneys, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                             favorite_money_analysis$udc_fmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(favorite_money_analysis$udc_fmlmoney[favorite_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=favorite_money_analysis$udc_fmlmoney[favorite_money_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$favorite[j]," moneyline, and when favorites are receiving ",favorite_money_analysis$udc_fmlmoney[favorite_money_analysis$league==league],
                            " percent from the moneys, betting the spread for the underdog has an eROI of 10% or better over a sample size of ",
                            favorite_money_analysis$udc_fmlmoney_pct[favorite_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  #### Underdog money ####
  
  for(j in 1:length(todays_games_udf$favorite)){
    
    league <- leagues[todays_games_udf$x[j]]
    
    hsd_s <- udf_analysis_today$underdog_money[j]
    hsd_ml <- udf_analysis_today$mlunderdog_money[j]
    statement <- 0
    if(is.na(hsd_s) | is.na(underdog_money_analysis$fml_udmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_money_analysis$fml_udmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_money_analysis$fml_udmoney[underdog_money_analysis$league==league],
                            " percent from the moneys on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$fml_udmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_money_analysis$fml_udmlmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_money_analysis$fml_udmlmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$favorite_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_money_analysis$fml_udmlmoney[underdog_money_analysis$league==league],
                            " percent from the moneys on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$fml_udmlmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_ml[j] <- paste0(todays_games_udf$statements_favorite_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_favorite[j] <- todays_games_udf$ml_bets_favorite[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_money_analysis$fc_udmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_money_analysis$fc_udmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and when the underdog is receiving ",underdog_money_analysis$fc_udmoney[underdog_money_analysis$league==league],
                            " percent from the moneys on the spread, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$fc_udmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_money_analysis$fc_udmlmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_money_analysis$fc_udmlmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the favorite, the ",udf_analysis_today$favorite[j],
                            ", against the ",udf_analysis_today$underdog[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$favorite_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and when the underdog is receiving ",underdog_money_analysis$fc_udmlmoney[underdog_money_analysis$league==league],
                            " percent from the moneys on the moneyline, betting the favorite has an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$fc_udmlmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_favorite_spread[j] <- paste0(todays_games_udf$statements_favorite_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_favorite[j] <- todays_games_udf$spread_bets_favorite[j] + 1
    }
    
    if(is.na(hsd_s) | is.na(underdog_money_analysis$udml_udmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_money_analysis$udml_udmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the moneys are ",hsd_s," percent on the ",
                            udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_money_analysis$udml_udmoney[underdog_money_analysis$league==league],
                            " percent from the moneys have an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$udml_udmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_money_analysis$udml_udmlmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_money_analysis$udml_udmlmoney[underdog_money_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the moneyline at ",udf_analysis_today$underdog_ml[j],". This is because the moneys are ",hsd_ml," percent on the ",
                             udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_money_analysis$udml_udmlmoney[underdog_money_analysis$league==league],
                             " percent from the moneys have an eROI of 10% or better over a sample size of ",
                             underdog_money_analysis$udml_udmlmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_ml[j] <- paste0(todays_games_udf$statements_underdog_ml[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$ml_bets_underdog[j] <- todays_games_udf$ml_bets_underdog[j] + 1
    }
    if(is.na(hsd_s) | is.na(underdog_money_analysis$udc_udmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_s>=underdog_money_analysis$udc_udmoney[underdog_money_analysis$league==league]){
        statement <-  paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                             ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                             " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the moneys are ",hsd_s," percent on the ",
                             udf_analysis_today$underdog[j]," spread, and underdogs receiving ",underdog_money_analysis$udc_udmoney[underdog_money_analysis$league==league],
                             " percent from the moneys have has an eROI of 10% or better over a sample size of ",
                             underdog_money_analysis$udc_udmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
    if(is.na(hsd_ml) | is.na(underdog_money_analysis$udc_udmlmoney[underdog_money_analysis$league==league])){
      
    }else{
      if(hsd_ml>=underdog_money_analysis$udc_udmlmoney[underdog_money_analysis$league==league]){
        statement <- paste0("The bet is the underdog, the ",udf_analysis_today$underdog[j],
                            ", against the ",udf_analysis_today$favorite[j]," at ",udf_analysis_today$est_time[j],
                            " on the spread at ",udf_analysis_today$underdog_spread[j],". This is because the moneys are ",hsd_ml," percent on the ",
                            udf_analysis_today$underdog[j]," moneyline, and underdogs receiving ",underdog_money_analysis$udc_udmlmoney[underdog_money_analysis$league==league],
                            " percent from the moneys have an eROI of 10% or better over a sample size of ",
                            underdog_money_analysis$udc_udmlmoney_pct[underdog_money_analysis$league==league]," games played in the last four years in the ",league," .")
      }else{
        
      } 
    }
    if(statement==0){
      
    }else{
      
      todays_games_udf$statements_underdog_spread[j] <- paste0(todays_games_udf$statements_underdog_spread[j]," \n\n",statement)
      statement <- 0
      todays_games_udf$spread_bets_underdog[j] <- todays_games_udf$spread_bets_underdog[j] + 1
    }
  }
  
  todays_games <- list(todays_games,todays_games_udf)
  
  
  
  
}

#### Update the Matrix #####
leagues <- c("nhl","nba","cfb","nfl",   "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",   "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",   "mlb")
for(q in 1:length(leagues)){
  #q  <- 1
  thematrix <- read.csv(paste0("Bets - ",toupper(leagues[q])," Matrix.csv"))
  
  thematrix$Sharp[1] <- away_sharp_analysis$aml_amlsharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[1] <- away_sharp_analysis$aml_amlsharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[1] <- away_public_analysis$aml_amlpublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[1] <- away_public_analysis$aml_amlpublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[1] <- away_money_analysis$aml_amlmoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[1] <- away_money_analysis$aml_amlmoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[2] <- home_sharp_analysis$aml_hmlsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[2] <- home_sharp_analysis$aml_hmlsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[2] <- home_public_analysis$aml_hmlpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[2] <- home_public_analysis$aml_hmlpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[2] <- home_money_analysis$aml_hmlmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[2] <- home_money_analysis$aml_hmlmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[3] <- away_sharp_analysis$ac_amlsharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[3] <- away_sharp_analysis$ac_amlsharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[3] <- away_public_analysis$ac_amlpublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[3] <- away_public_analysis$ac_amlpublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[3] <- away_money_analysis$ac_amlmoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[3] <- away_money_analysis$ac_amlmoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[4] <- home_sharp_analysis$ac_hmlsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[4] <- home_sharp_analysis$ac_hmlsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[4] <- home_public_analysis$ac_hmlpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[4] <- home_public_analysis$ac_hmlpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[4] <- home_money_analysis$ac_hmlmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[4] <- home_money_analysis$ac_hmlmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[5] <- away_sharp_analysis$hml_amlsharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[5] <- away_sharp_analysis$hml_amlsharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[5] <- away_public_analysis$hml_amlpublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[5] <- away_public_analysis$hml_amlpublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[5] <- away_money_analysis$hml_amlmoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[5] <- away_money_analysis$hml_amlmoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[6] <- home_sharp_analysis$hml_hmlsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[6] <- home_sharp_analysis$hml_hmlsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[6] <- home_public_analysis$hml_hmlpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[6] <- home_public_analysis$hml_hmlpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[6] <- home_money_analysis$hml_hmlmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[6] <- home_money_analysis$hml_hmlmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[7] <- away_sharp_analysis$hc_amlsharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[7] <- away_sharp_analysis$hc_amlsharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[7] <- away_public_analysis$hc_amlpublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[7] <- away_public_analysis$hc_amlpublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[7] <- away_money_analysis$hc_amlmoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[7] <- away_money_analysis$hc_amlmoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[8] <- home_sharp_analysis$hc_hmlsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[8] <- home_sharp_analysis$hc_hmlsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[8] <- home_public_analysis$hc_hmlpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[8] <- home_public_analysis$hc_hmlpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[8] <- home_money_analysis$hc_hmlmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[8] <- home_money_analysis$hc_hmlmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[17] <- away_sharp_analysis$aml_asharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[17] <- away_sharp_analysis$aml_asharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[17] <- away_public_analysis$aml_apublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[17] <- away_public_analysis$aml_apublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[17] <- away_money_analysis$aml_amoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[17] <- away_money_analysis$aml_amoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[18] <- home_sharp_analysis$aml_hsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[18] <- home_sharp_analysis$aml_hsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[18] <- home_public_analysis$aml_hpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[18] <- home_public_analysis$aml_hpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[18] <- home_money_analysis$aml_hmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[18] <- home_money_analysis$aml_hmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[19] <- away_sharp_analysis$ac_asharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[19] <- away_sharp_analysis$ac_asharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[19] <- away_public_analysis$ac_apublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[19] <- away_public_analysis$ac_apublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[19] <- away_money_analysis$ac_amoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[19] <- away_money_analysis$ac_amoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[20] <- home_sharp_analysis$ac_hsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[20] <- home_sharp_analysis$ac_hsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[20] <- home_public_analysis$ac_hpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[20] <- home_public_analysis$ac_hpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[20] <- home_money_analysis$ac_hmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[20] <- home_money_analysis$ac_hmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[21] <- away_sharp_analysis$hml_asharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[21] <- away_sharp_analysis$hml_asharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[21] <- away_public_analysis$hml_apublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[21] <- away_public_analysis$hml_apublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[21] <- away_money_analysis$hml_amoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[21] <- away_money_analysis$hml_amoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[22] <- home_sharp_analysis$hml_hsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[22] <- home_sharp_analysis$hml_hsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[22] <- home_public_analysis$hml_hpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[22] <- home_public_analysis$hml_hpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[22] <- home_money_analysis$hml_hmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[22] <- home_money_analysis$hml_hmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[23] <- away_sharp_analysis$hc_asharp[away_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[23] <- away_sharp_analysis$hc_asharp_pct[away_sharp_analysis$league==leagues[q]]
  thematrix$Public[23] <- away_public_analysis$hc_apublic[away_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[23] <- away_public_analysis$hc_apublic_pct[away_public_analysis$league==leagues[q]]
  thematrix$Money[23] <- away_money_analysis$hc_amoney[away_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[23] <- away_money_analysis$hc_amoney_pct[away_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[24] <- home_sharp_analysis$hc_hsharp[home_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[24] <- home_sharp_analysis$hc_hsharp_pct[home_sharp_analysis$league==leagues[q]]
  thematrix$Public[24] <- home_public_analysis$hc_hpublic[home_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[24] <- home_public_analysis$hc_hpublic_pct[home_public_analysis$league==leagues[q]]
  thematrix$Money[24] <- home_money_analysis$hc_hmoney[home_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[24] <- home_money_analysis$hc_hmoney_pct[home_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[9] <- underdog_sharp_analysis$udml_udmlsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[9] <- underdog_sharp_analysis$udml_udmlsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[9] <- underdog_public_analysis$udml_udmlpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[9] <- underdog_public_analysis$udml_udmlpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[9] <- underdog_money_analysis$udml_udmlmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[9] <- underdog_money_analysis$udml_udmlmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[10] <- favorite_sharp_analysis$udml_fmlsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[10] <- favorite_sharp_analysis$udml_fmlsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[10] <- favorite_public_analysis$udml_fmlpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[10] <- favorite_public_analysis$udml_fmlpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[10] <- favorite_money_analysis$udml_fmlmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[10] <- favorite_money_analysis$udml_fmlmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[11] <- underdog_sharp_analysis$udc_udmlsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[11] <- underdog_sharp_analysis$udc_udmlsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[11] <- underdog_public_analysis$udc_udmlpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[11] <- underdog_public_analysis$udc_udmlpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[11] <- underdog_money_analysis$udc_udmlmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[11] <- underdog_money_analysis$udc_udmlmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[12] <- favorite_sharp_analysis$udc_fmlsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[12] <- favorite_sharp_analysis$udc_fmlsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[12] <- favorite_public_analysis$udc_fmlpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[12] <- favorite_public_analysis$udc_fmlpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[12] <- favorite_money_analysis$udc_fmlmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[12] <- favorite_money_analysis$udc_fmlmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[13] <- underdog_sharp_analysis$fml_udmlsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[13] <- underdog_sharp_analysis$fml_udmlsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[13] <- underdog_public_analysis$fml_udmlpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[13] <- underdog_public_analysis$fml_udmlpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[13] <- underdog_money_analysis$fml_udmlmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[13] <- underdog_money_analysis$fml_udmlmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[14] <- favorite_sharp_analysis$fml_fmlsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[14] <- favorite_sharp_analysis$fml_fmlsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[14] <- favorite_public_analysis$fml_fmlpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[14] <- favorite_public_analysis$fml_fmlpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[14] <- favorite_money_analysis$fml_fmlmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[14] <- favorite_money_analysis$fml_fmlmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[15] <- underdog_sharp_analysis$fc_udmlsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[15] <- underdog_sharp_analysis$fc_udmlsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[15] <- underdog_public_analysis$fc_udmlpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[15] <- underdog_public_analysis$fc_udmlpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[15] <- underdog_money_analysis$fc_udmlmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[15] <- underdog_money_analysis$fc_udmlmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[16] <- favorite_sharp_analysis$fc_fmlsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[16] <- favorite_sharp_analysis$fc_fmlsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[16] <- favorite_public_analysis$fc_fmlpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[16] <- favorite_public_analysis$fc_fmlpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[16] <- favorite_money_analysis$fc_fmlmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[16] <- favorite_money_analysis$fc_fmlmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[25] <- underdog_sharp_analysis$udml_udsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[25] <- underdog_sharp_analysis$udml_udsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[25] <- underdog_public_analysis$udml_udpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[25] <- underdog_public_analysis$udml_udpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[25] <- underdog_money_analysis$udml_udmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[25] <- underdog_money_analysis$udml_udmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[26] <- favorite_sharp_analysis$udml_fsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[26] <- favorite_sharp_analysis$udml_fsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[26] <- favorite_public_analysis$udml_fpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[26] <- favorite_public_analysis$udml_fpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[26] <- favorite_money_analysis$udml_fmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[26] <- favorite_money_analysis$udml_fmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[27] <- underdog_sharp_analysis$udc_udsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[27] <- underdog_sharp_analysis$udc_udsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[27] <- underdog_public_analysis$udc_udpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[27] <- underdog_public_analysis$udc_udpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[27] <- underdog_money_analysis$udc_udmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[27] <- underdog_money_analysis$udc_udmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[28] <- favorite_sharp_analysis$udc_fsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[28] <- favorite_sharp_analysis$udc_fsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[28] <- favorite_public_analysis$udc_fpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[28] <- favorite_public_analysis$udc_fpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[28] <- favorite_money_analysis$udc_fmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[28] <- favorite_money_analysis$udc_fmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[29] <- underdog_sharp_analysis$fml_udsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[29] <- underdog_sharp_analysis$fml_udsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[29] <- underdog_public_analysis$fml_udpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[29] <- underdog_public_analysis$fml_udpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[29] <- underdog_money_analysis$fml_udmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[29] <- underdog_money_analysis$fml_udmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[30] <- favorite_sharp_analysis$fml_fsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[30] <- favorite_sharp_analysis$fml_fsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[30] <- favorite_public_analysis$fml_fpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[30] <- favorite_public_analysis$fml_fpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[30] <- favorite_money_analysis$fml_fmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[30] <- favorite_money_analysis$fml_fmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[31] <- underdog_sharp_analysis$fc_udsharp[underdog_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[31] <- underdog_sharp_analysis$fc_udsharp_pct[underdog_sharp_analysis$league==leagues[q]]
  thematrix$Public[31] <- underdog_public_analysis$fc_udpublic[underdog_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[31] <- underdog_public_analysis$fc_udpublic_pct[underdog_public_analysis$league==leagues[q]]
  thematrix$Money[31] <- underdog_money_analysis$fc_udmoney[underdog_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[31] <- underdog_money_analysis$fc_udmoney_pct[underdog_money_analysis$league==leagues[q]]
  
  thematrix$Sharp[32] <- favorite_sharp_analysis$fc_fsharp[favorite_sharp_analysis$league==leagues[q]]
  thematrix$no_bets[32] <- favorite_sharp_analysis$fc_fsharp_pct[favorite_sharp_analysis$league==leagues[q]]
  thematrix$Public[32] <- favorite_public_analysis$fc_fpublic[favorite_public_analysis$league==leagues[q]]
  thematrix$no_bets.1[32] <- favorite_public_analysis$fc_fpublic_pct[favorite_public_analysis$league==leagues[q]]
  thematrix$Money[32] <- favorite_money_analysis$fc_fmoney[favorite_money_analysis$league==leagues[q]]
  thematrix$no_bets.2[32] <- favorite_money_analysis$fc_fmoney_pct[favorite_money_analysis$league==leagues[q]]
  
  write.csv(thematrix, paste0("UPDATED Bets - ",toupper(leagues[q])," Matrix.csv"))
}



#### Today's games analysis ####
save.image(paste0(as.character(gsub("-","",Sys.Date())), " Action backdata.RData"))
load(paste0(as.character(gsub("-","",Sys.Date())), " Action backdata.RData"))

year <- 2021
nfl_week <- 11
cfb_week <- 13

repeat{
  
  
  
  
  todays_games_list <- todays_games_functions(backtest_games,
                                              nhl_games_all,
                                              mlb_games_all,
                                              sec_cbb_games_all,
                                              acc_cbb_games_all,
                                              big10_cbb_games_all,
                                              big12_cbb_games_all,
                                              pac12_cbb_games_all,
                                              ivy_cbb_games_all,
                                              nfl_games_all,
                                              cfb_games_all,
                                              nba_games_all,
                                              home_money_analysis,
                                              away_money_analysis,
                                              home_sharp_analysis,
                                              away_sharp_analysis,
                                              home_public_analysis,
                                              away_public_analysis,
                                              favorite_money_analysis,
                                              underdog_money_analysis,
                                              favorite_sharp_analysis,
                                              underdog_sharp_analysis,
                                              favorite_public_analysis,
                                              underdog_public_analysis,
                                              year,
                                              nfl_week,
                                              cfb_week)
  
  todays_games <- todays_games_list[[1]]
  todays_games$est_time <- todays_games$est_time-3600
  todays_games <- todays_games[todays_games$est_time>=Sys.Date()&todays_games$est_time<(Sys.Date()+1),]
  todays_games_udf <- todays_games_list[[2]]
  
  
  todays_games_udf <- todays_games_udf[todays_games_udf$est_time>=Sys.Date()&todays_games_udf$est_time<(Sys.Date()+1),]
  
  todays_games$gamecode <- ""
  todays_games_udf$gamecode <- ""
  for(b in 1:length(todays_games$away_team)){
    if(todays_games$away_team[b]==todays_games_udf$favorite[b]){
      todays_games$gamecode[b] <- paste0(todays_games$away_team[b],todays_games$home_team[b])
      todays_games_udf$gamecode[b] <- paste0(todays_games_udf$favorite[b],todays_games_udf$underdog[b])
    }else{
      todays_games$gamecode[b] <- paste0(todays_games$away_team[b],todays_games$home_team[b])
      todays_games_udf$gamecode[b] <- paste0(todays_games_udf$underdog[b],todays_games_udf$favorite[b])
    }
  }
  
  todays_games_udf <- todays_games_udf[order(todays_games_udf$gamecode),]
  todays_games <- todays_games[order(todays_games$gamecode),]
  todays_games_udf <- todays_games_udf[order(todays_games_udf$est_time),]
  todays_games <- todays_games[order(todays_games$est_time),]
  
  for(a in 1:length(todays_games$away_team)){
    
    if(a==length(todays_games$away_team)){
      
    }else{
      if(todays_games$away_team[a]==todays_games$away_team[a+1]){
        todays_games$statements_away_ml[a] <- paste0(todays_games$statements_away_ml[a]," \n\n",todays_games$statements_away_ml[a+1])
        todays_games$statements_away_spread[a] <- paste0(todays_games$statements_away_spread[a]," \n\n",todays_games$statements_away_spread[a+1])
        todays_games_udf$statements_favorite_ml[a] <- paste0(todays_games$statements_favorite_ml[a]," \n\n",todays_games$statements_favorite_ml[a+1])
        todays_games_udf$statements_favorite_spread[a] <- paste0(todays_games$statements_favorite_spread[a]," \n\n",todays_games$statements_favorite_spread[a+1])
        todays_games$statements_home_ml[a] <- paste0(todays_games$statements_home_ml[a]," \n\n",todays_games$statements_home_ml[a+1])
        todays_games$statements_home_spread[a] <- paste0(todays_games$statements_home_spread[a]," \n\n",todays_games$statements_home_spread[a+1])
        todays_games_udf$statements_underdog_ml[a] <- paste0(todays_games$statements_underdog_ml[a]," \n\n",todays_games$statements_underdog_ml[a+1])
        todays_games_udf$statements_underdog_spread[a] <- paste0(todays_games$statements_underdog_spread[a]," \n\n",todays_games$statements_underdog_spread[a+1])
      }else{
        if(a==1){
          
        }else{
          if(todays_games$away_team[a]==todays_games$away_team[a-1]){
            next()
          }else{
            
          }
        }
      }
    }
    
    
    if(todays_games$away_team[a]==todays_games_udf$favorite[a]){
        print(paste0(todays_games$statements_away_ml[a]," ",todays_games$statements_away_spread[a]," ",
              todays_games_udf$statements_favorite_ml[a]," ",todays_games_udf$statements_favorite_spread[a]))
      print(paste0(todays_games$statements_home_ml[a]," ",todays_games$statements_home_spread[a]," ",
                   todays_games_udf$statements_underdog_ml[a]," ",todays_games_udf$statements_underdog_spread[a]))
    }else{
      print(paste0(todays_games$statements_away_ml[a]," ",todays_games$statements_away_spread[a]," ",
            todays_games_udf$statements_underdog_ml[a]," ",todays_games_udf$statements_underdog_spread[a]))
      print(paste0(todays_games$statements_home_ml[a]," ",todays_games$statements_home_spread[a]," ",
                   todays_games_udf$statements_favorite_ml[a]," ",todays_games_udf$statements_favorite_spread[a]))
    }
  
    
    if(todays_games$est_time[a]>(Sys.time()-(0*600))&todays_games$est_time[a]<(Sys.time()+(1.5*600))){
      
    }else{
      next()
    }
    
    s_units <- -1
    spread_bet_team <- -1
    ml_units <- -1
    ml_bet_team <- -1
    
    if(todays_games$spread_bets_away[a]>0 | todays_games$spread_bets_home[a]>0){
      if(todays_games$spread_bets_away[a]>todays_games$spread_bets_home[a]){
        s_units <- todays_games$spread_bets_away[a]-todays_games$spread_bets_home[a]
        spread_bet_team <- todays_games$away_team[a]
      }else{
        s_units <- todays_games$spread_bets_home[a]-todays_games$spread_bets_away[a]
        spread_bet_team <- todays_games$home_team[a]
      }
    }
    
    if(todays_games$ml_bets_away[a]>0 | todays_games$ml_bets_home[a]>0){
      if(todays_games$ml_bets_away[a]>todays_games$ml_bets_home[a]){
        ml_units <- todays_games$ml_bets_away[a]-todays_games$ml_bets_home[a]
        ml_bet_team <- todays_games$away_team[a]
      }else{
        ml_units <- todays_games$ml_bets_home[a]-todays_games$ml_bets_away[a]
        ml_bet_team <- todays_games$home_team[a]
      }
    }
    
    if(todays_games_udf$spread_bets_underdog[a]>0 | todays_games_udf$spread_bets_favorite[a]>0){
      if(todays_games_udf$spread_bets_underdog[a]>todays_games_udf$spread_bets_favorite[a]){
        s_units <- todays_games_udf$spread_bets_underdog[a]-todays_games_udf$spread_bets_favorite[a]
        spread_bet_team <- todays_games_udf$underdog[a]
      }else{
        s_units <- todays_games_udf$spread_bets_favorite[a]-todays_games_udf$spread_bets_underdog[a]
        spread_bet_team <- todays_games_udf$favorite[a]
      }
    }
    
    if(todays_games_udf$ml_bets_underdog[a]>0 | todays_games_udf$ml_bets_favorite[a]>0){
      if(todays_games_udf$ml_bets_underdog[a]>todays_games_udf$ml_bets_favorite[a]){
        ml_units <- todays_games_udf$ml_bets_underdog[a]-todays_games_udf$ml_bets_favorite[a]
        ml_bet_team <- todays_games_udf$underdog[a]
      }else{
        ml_units <- todays_games_udf$ml_bets_favorite[a]-todays_games_udf$ml_bets_underdog[a]
        ml_bet_team <- todays_games_udf$favorite[a]
      }
    }
    
    if(todays_games$away_team[a]==todays_games_udf$favorite[a]){
      if(s_units>-1){
        
        email <- envelope()
        
        email <- email %>%
          from("br0wnsw0rdb3ts@gmail.com") %>%
          to("br0wnsw0rd4ct10n@gmail.com") %>% 
          subject(paste0("At ",gsub(":","",gsub("-","",todays_games$est_time[a])),", bet on the ",spread_bet_team," on the spread")) %>% 
          text(paste0(todays_games$statements_away_spread[a], " \n",todays_games_udf$statements_favorite_spread[a],
                      " \n",todays_games$statements_home_spread[a], " \n",todays_games_udf$statements_underdog_spread[a]))
        
        
        smtp <- server(host = "smtp.gmail.com",
                       port = 465,
                       username = "br0wnsw0rdb3ts@gmail.com",
                       password = "password")
        smtp(email, verbose = TRUE)
        
      }
      
      if(ml_units>-1){
        
        email <- envelope()
        
        email <- email %>%
          from("br0wnsw0rdb3ts@gmail.com") %>%
          to("br0wnsw0rd4ct10n@gmail.com") %>% 
          subject(paste0("At ",gsub(":","",gsub("-","",todays_games$est_time[a])),", bet on the ",ml_bet_team," on the moneyline.")) %>% 
          text(paste0(todays_games$statements_away_ml[a], " \n",todays_games_udf$statements_favorite_ml[a],
                      " \n",todays_games$statements_home_ml[a], " \n",todays_games_udf$statements_underdog_ml[a]))
        
        
        smtp <- server(host = "smtp.gmail.com",
                       port = 465,
                       username = "br0wnsw0rdb3ts@gmail.com",
                       password = "password")
        smtp(email, verbose = TRUE)
        
      }
    }else{
      if(s_units>-1){
        
        email <- envelope()
        
        email <- email %>%
          from("br0wnsw0rdb3ts@gmail.com") %>%
          to("br0wnsw0rd4ct10n@gmail.com") %>% 
          subject(paste0("At ",gsub(":","",gsub("-","",todays_games$est_time[a])),", bet on the ",spread_bet_team," on the spread")) %>% 
          text(paste0(todays_games$statements_away_spread[a], " \n",todays_games_udf$statements_underdog_spread[a],
                      " \n",todays_games$statements_home_spread[a], " \n",todays_games_udf$statements_favorite_spread[a]))
        
        
        smtp <- server(host = "smtp.gmail.com",
                       port = 465,
                       username = "br0wnsw0rdb3ts@gmail.com",
                       password = "password")
        smtp(email, verbose = TRUE)
        
      }
      
      if(ml_units>-1){
        
        email <- envelope()
        
        email <- email %>%
          from("br0wnsw0rdb3ts@gmail.com") %>%
          to("br0wnsw0rd4ct10n@gmail.com") %>% 
          subject(paste0("At ",gsub(":","",gsub("-","",todays_games$est_time[a])),", bet on the ",ml_bet_team," on the moneyline.")) %>% 
          text(paste0(todays_games$statements_away_ml[a], " \n",todays_games_udf$statements_underdog_ml[a],
                      " \n",todays_games$statements_home_ml[a], " \n",todays_games_udf$statements_favorite_ml[a]))
        
        
        smtp <- server(host = "smtp.gmail.com",
                       port = 465,
                       username = "br0wnsw0rdb3ts@gmail.com",
                       password = "password")
        smtp(email, verbose = TRUE)
        
      }
    }
        
    
    
    
    
    
  }
  write.csv(todays_games,"daily_schedule.csv",row.names = F)
  Sys.sleep(30)
  
}


#



##### Testing ####

action_picks <- fromJSON("https://www.actionnetwork.com/_next/data/Kyt7BzXy4ec4iO04tboDy/picks.json")

for(i in 1:length(action_picks$pageProps$experts$picks)){
  
  tdf <- data.frame(action_picks$pageProps$experts$picks[[i]])
  if(length(tdf)==0){
    next()
  }
  tdf <- tdf[,c(1:18,20:24)]
  if(i==1){
    test_df <- tdf
  }else{
    test_df <- rbind(tdf,test_df,fill = TRUE)
  }
  
}

data.frame(roi = action_picks[["pageProps"]][["experts"]][["record"]][["roi"]],name = action_picks[["pageProps"]][["experts"]][["name"]])

action_systems <- fromJSON("https://api.actionnetwork.com/web/v1/scoreboard/gameprojections/all?bookIds=15,30,68,75,69,76,71,79,247,123&date=20211110")

action_picks <- fromJSON("https://api.actionnetwork.com/web/v1/scoreboard/picks/all?date=20211110")


