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
big12_cbb_backtest <- function(){
  days <- c(1:31)
  years <- c(2018:2021)
  months <- c(1:12)
  action_all_list <-  vector("list",length(years))
  for(i in 1:length(years)){
    action_df_list <-  vector("list",length(months))
    for(j in 1:length(months)){
      action_day_list <-  vector("list",length(days))
      for(l in 1:length(days)){
        action_url <- paste0("https://api.actionnetwork.com/web/v1/scoreboard/ncaab?bookIds=123,75&date=",years[i],
                             ifelse(nchar(months[j])==1,paste0(0,months[j]),months[j]),ifelse(nchar(days[l])==1,paste0(0,days[l]),days[l]),"&conference=BIG12&tournament=0")
        t <- GET(action_url)
        if(t$status_code==400){
          next()
        }else{
          action_odds <- read_json(action_url)
        }
        
        if(length(action_odds$games)==0){
          next()
        }
        if(action_odds$games[[1]]$type!="reg"){
          next()
        }
        action_df <- data.frame(matrix(data = "",nrow = length(action_odds$games), ncol = 25))
        colnames(action_df) <- c("away_team","home_team","away_spread","home_spread","home_spread_line","away_spread_line",
                                 "away_ml","home_ml","time","away_score","home_score","total",
                                 "money_home","money_away","money_over","money_under","money_home_ml","money_away_ml",
                                 "public_home","public_away","public_over","public_under","public_home_ml","public_away_ml","book_id") 
        
        for(u in 1:length(action_odds$games)){
          if(action_odds$games[[u]]$status=="cancelled"){
            action_df$away_team[u] <- ""
            action_df$home_team[u] <- ""
            action_df$away_spread[u] <- ""
            action_df$home_spread[u] <- ""
            action_df$away_ml[u] <- ""
            action_df$home_ml[u] <- ""
            action_df$time[u] <- ""
            action_df$home_score[u] <- ""
            action_df$away_score[u] <- ""
            action_df$total[u] <- ""
          }else{
            if(action_odds$games[[u]]$away_team_id==action_odds$games[[u]]$teams[[1]]$id){
              action_df$away_team[u] <- action_odds$games[[u]]$teams[[1]]$display_name
              action_df$home_team[u] <- action_odds$games[[u]]$teams[[2]]$display_name
            }else{
              action_df$away_team[u] <- action_odds$games[[u]]$teams[[2]]$display_name
              action_df$home_team[u] <- action_odds$games[[u]]$teams[[1]]$display_name
            }
            
            if(length(action_odds$games[[u]]$boxscore)==0){
              action_df$home_score[u] <- 0
              action_df$away_score[u] <- 0
            }else{
              action_df$home_score[u] <- action_odds$games[[u]]$boxscore$total_home_points
              action_df$away_score[u] <- action_odds$games[[u]]$boxscore$total_away_points
            }
            action_df$time[u] <- action_odds$games[[u]]$start_time
            if(length(action_odds$games[[u]]$odds)==0){
              action_df$away_spread[u] <- "Offline"
              action_df$home_spread[u] <- "Offline"
              action_df$away_ml[u] <- "Offline"
              action_df$home_ml[u] <- "Offline"
            }else{
              
              for(k in 1:length(action_odds$games[[u]]$odds)){
                if(length(action_odds$games[[u]]$odds[[k]]$type)==0){
                  next()
                }
                if((action_odds$games[[u]]$odds[[k]]$book_id==123 && action_odds$games[[u]]$odds[[k]]$type=="game") | (action_odds$games[[u]]$odds[[k]]$book_id==75 && action_odds$games[[u]]$odds[[k]]$type=="game")){
                  action_df$book_id[u] <- action_odds$games[[u]]$odds[[k]]$book_id
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home)==0){
                    action_df$away_ml[u] <- "Offline"
                    action_df$home_ml[u] <- "Offline"
                  }else{
                    action_df$away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away
                    action_df$home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home
                  }
                  
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home)==0){
                    action_df$away_spread[u] <- "Offline"
                    action_df$home_spread[u] <- "Offline"
                  }else{
                    action_df$away_spread[u] <- action_odds$games[[u]]$odds[[k]]$spread_away
                    action_df$home_spread[u] <- action_odds$games[[u]]$odds[[k]]$spread_home
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_line)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_line)==0){
                    action_df$away_spread_line[u] <- "Offline"
                    action_df$home_spread_line[u] <- "Offline"
                  }else{
                    action_df$away_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_line
                    action_df$home_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_line
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away_money)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home_money)==0){
                    action_df$money_away[u] <- "Offline"
                    action_df$money_home[u] <- "Offline"
                  }else{
                    action_df$money_away[u] <- action_odds$games[[u]]$odds[[k]]$spread_away_money
                    action_df$money_home[u] <- action_odds$games[[u]]$odds[[k]]$spread_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total_over_money)==0 || length(action_odds$games[[u]]$odds[[k]]$total_under_money)==0){
                    action_df$money_over[u] <- "Offline"
                    action_df$money_under[u] <- "Offline"
                  }else{
                    action_df$money_over[u] <- action_odds$games[[u]]$odds[[k]]$total_over_money
                    action_df$money_under[u] <- action_odds$games[[u]]$odds[[k]]$total_under_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away_money)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home_money)==0){
                    action_df$money_away_ml[u] <- "Offline"
                    action_df$money_home_ml[u] <- "Offline"
                  }else{
                    action_df$money_away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away_money
                    action_df$money_home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away_public)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home_public)==0){
                    action_df$public_away[u] <- "Offline"
                    action_df$public_home[u] <- "Offline"
                  }else{
                    action_df$public_away[u] <- action_odds$games[[u]]$odds[[k]]$spread_away_public
                    action_df$public_home[u] <- action_odds$games[[u]]$odds[[k]]$spread_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total_over_public)==0 || length(action_odds$games[[u]]$odds[[k]]$total_under_public)==0){
                    action_df$public_over[u] <- "Offline"
                    action_df$public_under[u] <- "Offline"
                  }else{
                    action_df$public_over[u] <- action_odds$games[[u]]$odds[[k]]$total_over_public
                    action_df$public_under[u] <- action_odds$games[[u]]$odds[[k]]$total_under_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away_public)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home_public)==0){
                    action_df$public_away_ml[u] <- "Offline"
                    action_df$public_home_ml[u] <- "Offline"
                  }else{
                    action_df$public_away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away_public
                    action_df$public_home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total)==0 || length(action_odds$games[[u]]$odds[[k]]$total)==0){
                    action_df$total[u] <- "Offline"
                  }else{
                    action_df$total[u] <- action_odds$games[[u]]$odds[[k]]$total
                  }
                }else{
                  action_df$book_id[u] <- action_odds$games[[u]]$odds[[1]]$book_id
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home)==0){
                    action_df$away_ml[u] <- "Offline"
                    action_df$home_ml[u] <- "Offline"
                  }else{
                    action_df$away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away
                    action_df$home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home
                  }
                  
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home)==0){
                    action_df$away_spread[u] <- "Offline"
                    action_df$home_spread[u] <- "Offline"
                  }else{
                    action_df$away_spread[u] <- action_odds$games[[u]]$odds[[1]]$spread_away
                    action_df$home_spread[u] <- action_odds$games[[u]]$odds[[1]]$spread_home
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_line)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_line)==0){
                    action_df$away_spread_line[u] <- "Offline"
                    action_df$home_spread_line[u] <- "Offline"
                  }else{
                    action_df$away_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_line
                    action_df$home_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_line
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_money)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_money)==0){
                    action_df$money_away[u] <- "Offline"
                    action_df$money_home[u] <- "Offline"
                  }else{
                    action_df$money_away[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_money
                    action_df$money_home[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total_over_money)==0 || length(action_odds$games[[u]]$odds[[1]]$total_under_money)==0){
                    action_df$money_over[u] <- "Offline"
                    action_df$money_under[u] <- "Offline"
                  }else{
                    action_df$money_over[u] <- action_odds$games[[u]]$odds[[1]]$total_over_money
                    action_df$money_under[u] <- action_odds$games[[u]]$odds[[1]]$total_under_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away_money)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home_money)==0){
                    action_df$money_away_ml[u] <- "Offline"
                    action_df$money_home_ml[u] <- "Offline"
                  }else{
                    action_df$money_away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away_money
                    action_df$money_home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_public)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_public)==0){
                    action_df$public_away[u] <- "Offline"
                    action_df$public_home[u] <- "Offline"
                  }else{
                    action_df$public_away[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_public
                    action_df$public_home[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total_over_public)==0 || length(action_odds$games[[u]]$odds[[1]]$total_under_public)==0){
                    action_df$public_over[u] <- "Offline"
                    action_df$public_under[u] <- "Offline"
                  }else{
                    action_df$public_over[u] <- action_odds$games[[u]]$odds[[1]]$total_over_public
                    action_df$public_under[u] <- action_odds$games[[u]]$odds[[1]]$total_under_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away_public)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home_public)==0){
                    action_df$public_away_ml[u] <- "Offline"
                    action_df$public_home_ml[u] <- "Offline"
                  }else{
                    action_df$public_away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away_public
                    action_df$public_home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total)==0 || length(action_odds$games[[u]]$odds[[1]]$total)==0){
                    action_df$total[u] <- "Offline"
                  }else{
                    action_df$total[u] <- action_odds$games[[u]]$odds[[1]]$total
                  }
                }
              }
              
              
            }
            
          }
          
          
        }
        action_day_list[[l]] <- action_df
      }
      action_df_list[[j]] <- rbindlist(action_day_list)
      print(paste0("Done with month ",months[j]," ",years[i]))
    }
    action_all_list[[i]] <- rbindlist(action_df_list)
    print(paste0("Done with year ",years[i]))
  }
  
  
  action_all_df <- rbindlist(action_all_list)
  action_all_df <- action_all_df[-which(action_all_df$away_ml=="Offline"),]
  #action_all_df <- action_all_df[-which(action_all_df$total=="Offline"),]
  table(action_all_df$book_id)
  action_all_df$home_spread <- as.numeric(action_all_df$home_spread)
  action_all_df$total <- as.numeric(action_all_df$total)
  action_all_df$away_spread <- as.numeric(action_all_df$away_spread)
  action_all_df$home_score <- as.numeric(action_all_df$home_score)
  action_all_df$away_score <- as.numeric(action_all_df$away_score)
  action_all_df <- action_all_df[-which(is.na(action_all_df$home_score)),]
  
  action_all_df$hmlc <- 99
  action_all_df$amlc <- 99
  action_all_df$hc <- 99
  action_all_df$ac <- 99
  action_all_df$uc <- 99
  action_all_df$oc <- 99
  for(z in 1:length(action_all_df$away_team)){
    
    
    
    if((action_all_df$home_score[z]+action_all_df$away_score[z])==(abs(action_all_df$total[z]))){
      action_all_df$oc[z] <- 0
      action_all_df$uc[z] <- 0
    }else{
      if((action_all_df$home_score[z]+action_all_df$away_score[z])>(abs(action_all_df$total[z]))){
        action_all_df$oc[z] <- .954545
        action_all_df$uc[z] <- -1
      }else{
        action_all_df$uc[z] <- .954545
        action_all_df$oc[z] <- -1
      }
    }
    
    if((action_all_df$home_score[z]==action_all_df$away_score[z])){
      action_all_df$hmlc[z] <- -1
      action_all_df$amlc[z] <- -1
    }else{
      if((action_all_df$home_score[z]>action_all_df$away_score[z])){
        if(as.numeric(action_all_df$home_ml[z])>0){
          action_all_df$hmlc[z] <- 1*(as.numeric(action_all_df$home_ml[z])/100)
        }else{
          action_all_df$hmlc[z] <- 1*(100/abs(as.numeric(action_all_df$home_ml[z])))
        }
        
        action_all_df$amlc[z] <- -1
      }else{
        action_all_df$hmlc[z] <- -1
        if(as.numeric(action_all_df$away_ml[z])>0){
          action_all_df$amlc[z] <- 1*(as.numeric(action_all_df$away_ml[z])/100)
        }else{
          action_all_df$amlc[z] <- 1*(100/abs(as.numeric(action_all_df$away_ml[z])))
        }
      }
    }
    
    if(is.na(action_all_df$home_spread[z]) | is.na(action_all_df$away_spread[z])){
      next()
    }
    
    
    
    if(as.numeric(action_all_df$home_spread[z])<0){
      if((action_all_df$home_score[z]-action_all_df$away_score[z])==(abs(action_all_df$home_spread[z]))){
        action_all_df$hc[z] <- 0
      }else{
        if((action_all_df$home_score[z]-action_all_df$away_score[z])>(abs(action_all_df$home_spread[z]))){
          action_all_df$hc[z] <- 1*ifelse(as.numeric(action_all_df$home_spread_line[z])<0,
                                          (100/abs(as.numeric(action_all_df$home_spread_line[z]))),
                                          (as.numeric(action_all_df$home_spread_line[z])/100))
        }else{
          action_all_df$hc[z] <- -1
        }
      }
      
      
    }else{
      if(as.numeric(action_all_df$home_spread[z])>=0){
        if((action_all_df$away_score[z]-action_all_df$home_score[z])==(abs(action_all_df$home_spread[z]))){
          action_all_df$hc[z] <- 0
        }else{
          if((action_all_df$away_score[z]-action_all_df$home_score[z])<(abs(action_all_df$home_spread[z]))){
            action_all_df$hc[z] <- 1*ifelse(as.numeric(action_all_df$home_spread_line[z])<0,
                                            (100/abs(as.numeric(action_all_df$home_spread_line[z]))),
                                            (as.numeric(action_all_df$home_spread_line[z])/100))
          }else{
            action_all_df$hc[z] <- -1
          }
        }
        
      }
    }
    
    if(as.numeric(action_all_df$away_spread[z])<0){
      if((action_all_df$away_score[z]-action_all_df$home_score[z])==(abs(action_all_df$away_spread[z]))){
        action_all_df$ac[z] <- 0
      }else{
        if((action_all_df$away_score[z]-action_all_df$home_score[z])>(abs(action_all_df$away_spread[z]))){
          action_all_df$ac[z] <- 1*ifelse(as.numeric(action_all_df$away_spread_line[z])<0,
                                          (100/abs(as.numeric(action_all_df$away_spread_line[z]))),
                                          (as.numeric(action_all_df$away_spread_line[z])/100))
        }else{
          action_all_df$ac[z] <- -1
        }
      }
      
      
    }else{
      if(as.numeric(action_all_df$away_spread[z])>=0){
        if((action_all_df$home_score[z]-action_all_df$away_score[z])==(abs(action_all_df$away_spread[z]))){
          action_all_df$ac[z] <- 0
        }else{
          if((action_all_df$home_score[z]-action_all_df$away_score[z])<(abs(action_all_df$away_spread[z]))){
            action_all_df$ac[z] <- 1*ifelse(as.numeric(action_all_df$away_spread_line[z])<0,
                                            (100/abs(as.numeric(action_all_df$away_spread_line[z]))),
                                            (as.numeric(action_all_df$away_spread_line[z])/100))
          }else{
            action_all_df$ac[z] <- -1
          }
        }
        
      }
    }
    
    
  }
  
  
  if(length(which(is.na(action_all_df$home_spread)))==0){
    
  }else{
    action_all_df <- action_all_df[-which(is.na(action_all_df$home_spread)),]
  }
  
  action_all_df <- action_all_df[-which(action_all_df$money_home=="Offline"),]
  #action_all_df <- action_all_df[-which(action_all_df$home_spread_line=="Offline"),]
  action_all_df$hsd_ml <- as.numeric(action_all_df$money_home_ml)-as.numeric(action_all_df$public_home_ml)
  action_all_df$asd_ml <- as.numeric(action_all_df$money_away_ml)-as.numeric(action_all_df$public_away_ml)
  action_all_df$hsd_s <- as.numeric(action_all_df$money_home)-as.numeric(action_all_df$public_home)
  action_all_df$asd_s <- as.numeric(action_all_df$money_away)-as.numeric(action_all_df$public_away)
  
  nba_all_games <- action_all_df
  
}


big12_cbb_udf_analysis <- function(nba_all_games){
  
  
  
  nba_all_games$favorite <- ifelse(nba_all_games$home_spread<0,nba_all_games$home_team,
                                   ifelse(nba_all_games$home_spread>0,nba_all_games$away_team,
                                          NA))
  nba_all_games$underdog <- ifelse(nba_all_games$home_spread<0,nba_all_games$away_team,
                                   ifelse(nba_all_games$home_spread>0,nba_all_games$home_team,
                                          NA))
  
  nba_all_games$favorite_score <- ifelse(nba_all_games$home_spread<0,nba_all_games$home_score,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$away_score,
                                                NA))
  nba_all_games$underdog_score <- ifelse(nba_all_games$home_spread<0,nba_all_games$away_score,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$home_score,
                                                NA))
  
  nba_all_games$favorite_spread <- ifelse(nba_all_games$home_spread<0,nba_all_games$home_spread,
                                          ifelse(nba_all_games$home_spread>0,nba_all_games$away_spread,
                                                 NA))
  nba_all_games$underdog_spread <- ifelse(nba_all_games$home_spread<0,nba_all_games$away_spread,
                                          ifelse(nba_all_games$home_spread>0,nba_all_games$home_spread,
                                                 NA))
  
  nba_all_games$favorite_ml <- ifelse(nba_all_games$home_spread<0,nba_all_games$home_ml,
                                      ifelse(nba_all_games$home_spread>0,nba_all_games$away_ml,
                                             NA))
  nba_all_games$underdog_ml <- ifelse(nba_all_games$home_spread<0,nba_all_games$away_ml,
                                      ifelse(nba_all_games$home_spread>0,nba_all_games$home_ml,
                                             NA))
  
  nba_all_games$favorite_spread_line <- ifelse(nba_all_games$home_spread<0,nba_all_games$home_spread_line,
                                               ifelse(nba_all_games$home_spread>0,nba_all_games$away_spread_line,
                                                      NA))
  nba_all_games$underdog_spread_line <- ifelse(nba_all_games$home_spread<0,nba_all_games$away_spread_line,
                                               ifelse(nba_all_games$home_spread>0,nba_all_games$home_spread_line,
                                                      NA))
  
  nba_all_games$favorite_public <- ifelse(nba_all_games$home_spread<0,nba_all_games$public_home,
                                          ifelse(nba_all_games$home_spread>0,nba_all_games$public_away,
                                                 NA))
  nba_all_games$underdog_public <- ifelse(nba_all_games$home_spread<0,nba_all_games$public_away,
                                          ifelse(nba_all_games$home_spread>0,nba_all_games$public_home,
                                                 NA))
  
  nba_all_games$favorite_money <- ifelse(nba_all_games$home_spread<0,nba_all_games$money_home,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$money_away,
                                                NA))
  nba_all_games$underdog_money <- ifelse(nba_all_games$home_spread<0,nba_all_games$money_away,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$money_home,
                                                NA))
  
  nba_all_games$favorite_sharp <- ifelse(nba_all_games$home_spread<0,nba_all_games$hsd_s,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$asd_s,
                                                NA))
  nba_all_games$underdog_sharp <- ifelse(nba_all_games$home_spread<0,nba_all_games$asd_s,
                                         ifelse(nba_all_games$home_spread>0,nba_all_games$hsd_s,
                                                NA))
  
  nba_all_games$mlfavorite_public <- ifelse(nba_all_games$home_spread<0,nba_all_games$public_home_ml,
                                            ifelse(nba_all_games$home_spread>0,nba_all_games$public_away_ml,
                                                   NA))
  nba_all_games$mlunderdog_public <- ifelse(nba_all_games$home_spread<0,nba_all_games$public_away_ml,
                                            ifelse(nba_all_games$home_spread>0,nba_all_games$public_home_ml,
                                                   NA))
  
  nba_all_games$mlfavorite_money <- ifelse(nba_all_games$home_spread<0,nba_all_games$money_home_ml,
                                           ifelse(nba_all_games$home_spread>0,nba_all_games$money_away_ml,
                                                  NA))
  nba_all_games$mlunderdog_money <- ifelse(nba_all_games$home_spread<0,nba_all_games$money_away_ml,
                                           ifelse(nba_all_games$home_spread>0,nba_all_games$money_home_ml,
                                                  NA))
  
  nba_all_games$mlfavorite_sharp <- ifelse(nba_all_games$home_spread<0,nba_all_games$hsd_ml,
                                           ifelse(nba_all_games$home_spread>0,nba_all_games$asd_ml,
                                                  NA))
  nba_all_games$mlunderdog_sharp <- ifelse(nba_all_games$home_spread<0,nba_all_games$asd_ml,
                                           ifelse(nba_all_games$home_spread>0,nba_all_games$hsd_ml,
                                                  NA))
  
  
  
  nba_all_games$fmlc <- 99
  nba_all_games$udmlc <- 99
  nba_all_games$fc <- 99
  nba_all_games$udc <- 99
  for(z in 1:length(nba_all_games$underdog)){
    
    if(is.na(nba_all_games$underdog_score[z])){
      next()
    }
    
    if((nba_all_games$favorite_score[z]==nba_all_games$underdog_score[z])){
      nba_all_games$fmlc[z] <- -1
      nba_all_games$udmlc[z] <- -1
    }else{
      if((nba_all_games$favorite_score[z]>nba_all_games$underdog_score[z])){
        if(as.numeric(nba_all_games$favorite_ml[z])>0){
          nba_all_games$fmlc[z] <- 1*(as.numeric(nba_all_games$favorite_ml[z])/100)
        }else{
          nba_all_games$fmlc[z] <- 1*(100/abs(as.numeric(nba_all_games$favorite_ml[z])))
        }
        
        nba_all_games$udmlc[z] <- -1
      }else{
        nba_all_games$fmlc[z] <- -1
        if(as.numeric(nba_all_games$underdog_ml[z])>0){
          nba_all_games$udmlc[z] <- 1*(as.numeric(nba_all_games$underdog_ml[z])/100)
        }else{
          nba_all_games$udmlc[z] <- 1*(100/abs(as.numeric(nba_all_games$underdog_ml[z])))
        }
      }
    }
    
    if(is.na(nba_all_games$favorite_spread[z]) | is.na(nba_all_games$underdog_spread[z])){
      next()
    }
    
    
    
    if(as.numeric(nba_all_games$favorite_spread[z])<0){
      if((nba_all_games$favorite_score[z]-nba_all_games$underdog_score[z])==(abs(nba_all_games$favorite_spread[z]))){
        nba_all_games$fc[z] <- 0
      }else{
        if((nba_all_games$favorite_score[z]-nba_all_games$underdog_score[z])>(abs(nba_all_games$favorite_spread[z]))){
          nba_all_games$fc[z] <- 1*ifelse(as.numeric(nba_all_games$favorite_spread_line[z])<0,
                                          (100/abs(as.numeric(nba_all_games$favorite_spread_line[z]))),
                                          (as.numeric(nba_all_games$favorite_spread_line[z])/100))
        }else{
          nba_all_games$fc[z] <- -1
        }
      }
      
      
    }else{
      if(as.numeric(nba_all_games$favorite_spread[z])>=0){
        if((nba_all_games$underdog_score[z]-nba_all_games$favorite_score[z])==(abs(nba_all_games$favorite_spread[z]))){
          nba_all_games$fc[z] <- 0
        }else{
          if((nba_all_games$underdog_score[z]-nba_all_games$favorite_score[z])<(abs(nba_all_games$favorite_spread[z]))){
            nba_all_games$fc[z] <- 1*ifelse(as.numeric(nba_all_games$favorite_spread_line[z])<0,
                                            (100/abs(as.numeric(nba_all_games$favorite_spread_line[z]))),
                                            (as.numeric(nba_all_games$favorite_spread_line[z])/100))
          }else{
            nba_all_games$fc[z] <- -1
          }
        }
        
      }
    }
    
    if(as.numeric(nba_all_games$underdog_spread[z])<0){
      if((nba_all_games$underdog_score[z]-nba_all_games$favorite_score[z])==(abs(nba_all_games$underdog_spread[z]))){
        nba_all_games$udc[z] <- 0
      }else{
        if((nba_all_games$underdog_score[z]-nba_all_games$favorite_score[z])>(abs(nba_all_games$underdog_spread[z]))){
          nba_all_games$udc[z] <- 1*ifelse(as.numeric(nba_all_games$underdog_spread_line[z])<0,
                                           (100/abs(as.numeric(nba_all_games$underdog_spread_line[z]))),
                                           (as.numeric(nba_all_games$underdog_spread_line[z])/100))
        }else{
          nba_all_games$udc[z] <- -1
        }
      }
      
      
    }else{
      if(as.numeric(nba_all_games$underdog_spread[z])>=0){
        if((nba_all_games$favorite_score[z]-nba_all_games$underdog_score[z])==(abs(nba_all_games$underdog_spread[z]))){
          nba_all_games$udc[z] <- 0
        }else{
          if((nba_all_games$favorite_score[z]-nba_all_games$underdog_score[z])<(abs(nba_all_games$underdog_spread[z]))){
            nba_all_games$udc[z] <- 1*ifelse(as.numeric(nba_all_games$underdog_spread_line[z])<0,
                                             (100/abs(as.numeric(nba_all_games$underdog_spread_line[z]))),
                                             (as.numeric(nba_all_games$underdog_spread_line[z])/100))
          }else{
            nba_all_games$udc[z] <- -1
          }
        }
        
      }
    }
    
    
  }
  
  cbb_udf_games <- nba_all_games[,c("time","favorite","underdog","favorite_score", "underdog_score", "favorite_spread","underdog_spread","favorite_ml",   
                                    "underdog_ml",    "favorite_spread_line" ,"underdog_spread_line", 
                                    "favorite_public","underdog_public","favorite_money", "underdog_money", "favorite_sharp", "underdog_sharp", "mlfavorite_public",    "mlunderdog_public"  , 
                                    "mlfavorite_money" ,    "mlunderdog_money"    , "mlfavorite_sharp"    , "mlunderdog_sharp"   ,  "fmlc",    "udmlc",   "fc","udc")]
  
}

big12_todays_cbb_analysis <- function(action_all_df){
  
  days <- strsplit(as.character(Sys.Date()),"-")[[1]][3]
  years <- strsplit(as.character(Sys.Date()),"-")[[1]][1]
  months <- strsplit(as.character(Sys.Date()),"-")[[1]][2]
  action_all_list <-  vector("list",length(years))
  for(i in 1:length(years)){
    action_df_list <-  vector("list",length(months))
    for(j in 1:length(months)){
      action_day_list <-  vector("list",length(days))
      for(l in 1:length(days)){
        action_url <- paste0("https://api.actionnetwork.com/web/v1/scoreboard/ncaab?bookIds=123,75&date=",years[i],
                             ifelse(nchar(months[j])==1,paste0(0,months[j]),months[j]),ifelse(nchar(days[l])==1,paste0(0,days[l]),days[l]),"&conference=BIG12&tournament=0")
        t <- GET(action_url)
        if(t$status_code==400){
          next()
        }else{
          action_odds <- read_json(action_url)
        }
        
        if(length(action_odds$games)==0){
          next()
        }
        #if(action_odds$games[[1]]$type!="reg"){
        #  next()
        #}
        action_df <- data.frame(matrix(data = "",nrow = length(action_odds$games), ncol = 25))
        colnames(action_df) <- c("away_team","home_team","away_spread","home_spread","home_spread_line","away_spread_line",
                                 "away_ml","home_ml","time","away_score","home_score","total",
                                 "money_home","money_away","money_over","money_under","money_home_ml","money_away_ml",
                                 "public_home","public_away","public_over","public_under","public_home_ml","public_away_ml","book_id") 
        
        for(u in 1:length(action_odds$games)){
          if(action_odds$games[[u]]$status=="cancelled"){
            action_df$away_team[u] <- ""
            action_df$home_team[u] <- ""
            action_df$away_spread[u] <- ""
            action_df$home_spread[u] <- ""
            action_df$away_ml[u] <- ""
            action_df$home_ml[u] <- ""
            action_df$time[u] <- ""
            action_df$home_score[u] <- ""
            action_df$away_score[u] <- ""
            action_df$total[u] <- ""
          }else{
            if(action_odds$games[[u]]$away_team_id==action_odds$games[[u]]$teams[[1]]$id){
              action_df$away_team[u] <- action_odds$games[[u]]$teams[[1]]$display_name
              action_df$home_team[u] <- action_odds$games[[u]]$teams[[2]]$display_name
            }else{
              action_df$away_team[u] <- action_odds$games[[u]]$teams[[2]]$display_name
              action_df$home_team[u] <- action_odds$games[[u]]$teams[[1]]$display_name
            }
            
            if(length(action_odds$games[[u]]$boxscore)==0){
              action_df$home_score[u] <- 0
              action_df$away_score[u] <- 0
            }else{
              action_df$home_score[u] <- action_odds$games[[u]]$boxscore$total_home_points
              action_df$away_score[u] <- action_odds$games[[u]]$boxscore$total_away_points
            }
            action_df$time[u] <- action_odds$games[[u]]$start_time
            if(length(action_odds$games[[u]]$odds)==0){
              action_df$away_spread[u] <- "Offline"
              action_df$home_spread[u] <- "Offline"
              action_df$away_ml[u] <- "Offline"
              action_df$home_ml[u] <- "Offline"
            }else{
              
              for(k in 1:length(action_odds$games[[u]]$odds)){
                if((action_odds$games[[u]]$odds[[k]]$book_id==123 && action_odds$games[[u]]$odds[[k]]$type=="game") | (action_odds$games[[u]]$odds[[k]]$book_id==75 && action_odds$games[[u]]$odds[[k]]$type=="game")){
                  action_df$book_id[u] <- action_odds$games[[u]]$odds[[k]]$book_id
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home)==0){
                    action_df$away_ml[u] <- "Offline"
                    action_df$home_ml[u] <- "Offline"
                  }else{
                    action_df$away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away
                    action_df$home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home
                  }
                  
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home)==0){
                    action_df$away_spread[u] <- "Offline"
                    action_df$home_spread[u] <- "Offline"
                  }else{
                    action_df$away_spread[u] <- action_odds$games[[u]]$odds[[k]]$spread_away
                    action_df$home_spread[u] <- action_odds$games[[u]]$odds[[k]]$spread_home
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_line)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_line)==0){
                    action_df$away_spread_line[u] <- "Offline"
                    action_df$home_spread_line[u] <- "Offline"
                  }else{
                    action_df$away_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_line
                    action_df$home_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_line
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away_money)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home_money)==0){
                    action_df$money_away[u] <- "Offline"
                    action_df$money_home[u] <- "Offline"
                  }else{
                    action_df$money_away[u] <- action_odds$games[[u]]$odds[[k]]$spread_away_money
                    action_df$money_home[u] <- action_odds$games[[u]]$odds[[k]]$spread_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total_over_money)==0 || length(action_odds$games[[u]]$odds[[k]]$total_under_money)==0){
                    action_df$money_over[u] <- "Offline"
                    action_df$money_under[u] <- "Offline"
                  }else{
                    action_df$money_over[u] <- action_odds$games[[u]]$odds[[k]]$total_over_money
                    action_df$money_under[u] <- action_odds$games[[u]]$odds[[k]]$total_under_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away_money)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home_money)==0){
                    action_df$money_away_ml[u] <- "Offline"
                    action_df$money_home_ml[u] <- "Offline"
                  }else{
                    action_df$money_away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away_money
                    action_df$money_home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$spread_away_public)==0 || length(action_odds$games[[u]]$odds[[k]]$spread_home_public)==0){
                    action_df$public_away[u] <- "Offline"
                    action_df$public_home[u] <- "Offline"
                  }else{
                    action_df$public_away[u] <- action_odds$games[[u]]$odds[[k]]$spread_away_public
                    action_df$public_home[u] <- action_odds$games[[u]]$odds[[k]]$spread_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total_over_public)==0 || length(action_odds$games[[u]]$odds[[k]]$total_under_public)==0){
                    action_df$public_over[u] <- "Offline"
                    action_df$public_under[u] <- "Offline"
                  }else{
                    action_df$public_over[u] <- action_odds$games[[u]]$odds[[k]]$total_over_public
                    action_df$public_under[u] <- action_odds$games[[u]]$odds[[k]]$total_under_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$ml_away_public)==0 || length(action_odds$games[[u]]$odds[[k]]$ml_home_public)==0){
                    action_df$public_away_ml[u] <- "Offline"
                    action_df$public_home_ml[u] <- "Offline"
                  }else{
                    action_df$public_away_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_away_public
                    action_df$public_home_ml[u] <- action_odds$games[[u]]$odds[[k]]$ml_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[k]]$total)==0 || length(action_odds$games[[u]]$odds[[k]]$total)==0){
                    action_df$total[u] <- "Offline"
                  }else{
                    action_df$total[u] <- action_odds$games[[u]]$odds[[k]]$total
                  }
                }else{
                  action_df$book_id[u] <- action_odds$games[[u]]$odds[[1]]$book_id
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home)==0){
                    action_df$away_ml[u] <- "Offline"
                    action_df$home_ml[u] <- "Offline"
                  }else{
                    action_df$away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away
                    action_df$home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home
                  }
                  
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home)==0){
                    action_df$away_spread[u] <- "Offline"
                    action_df$home_spread[u] <- "Offline"
                  }else{
                    action_df$away_spread[u] <- action_odds$games[[u]]$odds[[1]]$spread_away
                    action_df$home_spread[u] <- action_odds$games[[u]]$odds[[1]]$spread_home
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_line)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_line)==0){
                    action_df$away_spread_line[u] <- "Offline"
                    action_df$home_spread_line[u] <- "Offline"
                  }else{
                    action_df$away_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_line
                    action_df$home_spread_line[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_line
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_money)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_money)==0){
                    action_df$money_away[u] <- "Offline"
                    action_df$money_home[u] <- "Offline"
                  }else{
                    action_df$money_away[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_money
                    action_df$money_home[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total_over_money)==0 || length(action_odds$games[[u]]$odds[[1]]$total_under_money)==0){
                    action_df$money_over[u] <- "Offline"
                    action_df$money_under[u] <- "Offline"
                  }else{
                    action_df$money_over[u] <- action_odds$games[[u]]$odds[[1]]$total_over_money
                    action_df$money_under[u] <- action_odds$games[[u]]$odds[[1]]$total_under_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away_money)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home_money)==0){
                    action_df$money_away_ml[u] <- "Offline"
                    action_df$money_home_ml[u] <- "Offline"
                  }else{
                    action_df$money_away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away_money
                    action_df$money_home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home_money
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$spread_away_public)==0 || length(action_odds$games[[u]]$odds[[1]]$spread_home_public)==0){
                    action_df$public_away[u] <- "Offline"
                    action_df$public_home[u] <- "Offline"
                  }else{
                    action_df$public_away[u] <- action_odds$games[[u]]$odds[[1]]$spread_away_public
                    action_df$public_home[u] <- action_odds$games[[u]]$odds[[1]]$spread_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total_over_public)==0 || length(action_odds$games[[u]]$odds[[1]]$total_under_public)==0){
                    action_df$public_over[u] <- "Offline"
                    action_df$public_under[u] <- "Offline"
                  }else{
                    action_df$public_over[u] <- action_odds$games[[u]]$odds[[1]]$total_over_public
                    action_df$public_under[u] <- action_odds$games[[u]]$odds[[1]]$total_under_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$ml_away_public)==0 || length(action_odds$games[[u]]$odds[[1]]$ml_home_public)==0){
                    action_df$public_away_ml[u] <- "Offline"
                    action_df$public_home_ml[u] <- "Offline"
                  }else{
                    action_df$public_away_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_away_public
                    action_df$public_home_ml[u] <- action_odds$games[[u]]$odds[[1]]$ml_home_public
                  }
                  if(length(action_odds$games[[u]]$odds[[1]]$total)==0 || length(action_odds$games[[u]]$odds[[1]]$total)==0){
                    action_df$total[u] <- "Offline"
                  }else{
                    action_df$total[u] <- action_odds$games[[u]]$odds[[1]]$total
                  }
                }
              }
              
              
            }
            
          }
          
          
        }
        action_day_list[[l]] <- action_df
      }
      action_df_list[[j]] <- rbindlist(action_day_list)
      print(paste0("Done with month ",months[j]," ",years[i]))
    }
    action_all_list[[i]] <- rbindlist(action_df_list)
    print(paste0("Done with year ",years[i]))
  }
  
  action_all_df_today <- rbindlist(action_all_list)
  
  
  action_all_df_today$est_time <- as.POSIXct(action_all_df_today$time, format = "%Y-%m-%dT%H:%M:%OSZ")-(60*60*4)
  action_all_df_today$hsd_ml <- as.numeric(action_all_df_today$money_home_ml)-as.numeric(action_all_df_today$public_home_ml)
  action_all_df_today$asd_ml <- as.numeric(action_all_df_today$money_away_ml)-as.numeric(action_all_df_today$public_away_ml)
  action_all_df_today$hsd_s <- as.numeric(action_all_df_today$money_home)-as.numeric(action_all_df_today$public_home)
  action_all_df_today$asd_s <- as.numeric(action_all_df_today$money_away)-as.numeric(action_all_df_today$public_away)
  
  action_all_df_today$money_home_ml <- as.numeric(action_all_df_today$money_home_ml)
  action_all_df_today$money_home <- as.numeric(action_all_df_today$money_home)
  action_all_df_today$money_away_ml <- as.numeric(action_all_df_today$money_away_ml)
  action_all_df_today$money_away <- as.numeric(action_all_df_today$money_away)
  action_all_df_today$public_home_ml <- as.numeric(action_all_df_today$public_home_ml)
  action_all_df_today$public_home <- as.numeric(action_all_df_today$public_home)
  action_all_df_today$public_away_ml <- as.numeric(action_all_df_today$public_away_ml)
  action_all_df_today$public_away <- as.numeric(action_all_df_today$public_away)
  
  action_all_df_today$favorite <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$home_team,
                                         ifelse(action_all_df_today$home_spread>0,action_all_df_today$away_team,
                                                action_all_df_today$home_team))
  action_all_df_today$underdog <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$away_team,
                                         ifelse(action_all_df_today$home_spread>0,action_all_df_today$home_team,
                                                action_all_df_today$away_team))
  
  action_all_df_today$favorite_score <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$home_score,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$away_score,
                                                      action_all_df_today$home_score))
  action_all_df_today$underdog_score <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$away_score,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$home_score,
                                                      action_all_df_today$away_score))
  
  action_all_df_today$favorite_spread <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$home_spread,
                                                ifelse(action_all_df_today$home_spread>0,action_all_df_today$away_spread,
                                                       action_all_df_today$home_spread))
  action_all_df_today$underdog_spread <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$away_spread,
                                                ifelse(action_all_df_today$home_spread>0,action_all_df_today$home_spread,
                                                       action_all_df_today$away_spread))
  
  action_all_df_today$favorite_ml <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$home_ml,
                                            ifelse(action_all_df_today$home_spread>0,action_all_df_today$away_ml,
                                                   action_all_df_today$home_ml))
  action_all_df_today$underdog_ml <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$away_ml,
                                            ifelse(action_all_df_today$home_spread>0,action_all_df_today$home_ml,
                                                   action_all_df_today$away_ml))
  
  action_all_df_today$favorite_spread_line <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$home_spread_line,
                                                     ifelse(action_all_df_today$home_spread>0,action_all_df_today$away_spread_line,
                                                            action_all_df_today$home_spread_line))
  action_all_df_today$underdog_spread_line <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$away_spread_line,
                                                     ifelse(action_all_df_today$home_spread>0,action_all_df_today$home_spread_line,
                                                            action_all_df_today$away_spread_line))
  
  action_all_df_today$favorite_public <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$public_home,
                                                ifelse(action_all_df_today$home_spread>0,action_all_df_today$public_away,
                                                       action_all_df_today$public_home))
  action_all_df_today$underdog_public <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$public_away,
                                                ifelse(action_all_df_today$home_spread>0,action_all_df_today$public_home,
                                                       action_all_df_today$public_away))
  
  action_all_df_today$favorite_money <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$money_home,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$money_away,
                                                      action_all_df_today$money_home))
  action_all_df_today$underdog_money <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$money_away,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$money_home,
                                                      action_all_df_today$money_away))
  
  action_all_df_today$favorite_sharp <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$hsd_s,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$asd_s,
                                                      action_all_df_today$hsd_s))
  action_all_df_today$underdog_sharp <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$asd_s,
                                               ifelse(action_all_df_today$home_spread>0,action_all_df_today$hsd_s,
                                                      action_all_df_today$asd_s))
  
  action_all_df_today$mlfavorite_public <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$public_home_ml,
                                                  ifelse(action_all_df_today$home_spread>0,action_all_df_today$public_away_ml,
                                                         action_all_df_today$public_home_ml))
  action_all_df_today$mlunderdog_public <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$public_away_ml,
                                                  ifelse(action_all_df_today$home_spread>0,action_all_df_today$public_home_ml,
                                                         action_all_df_today$public_away_ml))
  
  action_all_df_today$mlfavorite_money <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$money_home_ml,
                                                 ifelse(action_all_df_today$home_spread>0,action_all_df_today$money_away_ml,
                                                        action_all_df_today$money_home_ml))
  action_all_df_today$mlunderdog_money <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$money_away_ml,
                                                 ifelse(action_all_df_today$home_spread>0,action_all_df_today$money_home_ml,
                                                        action_all_df_today$money_away_ml))
  
  action_all_df_today$mlfavorite_sharp <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$hsd_ml,
                                                 ifelse(action_all_df_today$home_spread>0,action_all_df_today$asd_ml,
                                                        action_all_df_today$hsd_ml))
  action_all_df_today$mlunderdog_sharp <- ifelse(action_all_df_today$home_spread<0,action_all_df_today$asd_ml,
                                                 ifelse(action_all_df_today$home_spread>0,action_all_df_today$hsd_ml,
                                                        action_all_df_today$asd_ml))
  
  aroi1 <- 0
  an1 <- 0
  hroi1 <- 0
  hn1 <- 0
  mlaroi1 <- 0
  mlan1 <- 0
  mlhroi1 <- 0
  mlhn1 <- 0
  aroi2 <- 0
  an2 <- 0
  hroi2 <- 0
  hn2 <- 0
  mlaroi2 <- 0
  mlan2 <- 0
  mlhroi2 <- 0
  mlhn2 <- 0
  aroi3 <- 0
  an3 <- 0
  hroi3 <- 0
  hn3 <- 0
  mlaroi3 <- 0
  mlan3 <- 0
  mlhroi3 <- 0
  mlhn3 <- 0
  aroi4 <- 0
  an4 <- 0
  hroi4 <- 0
  hn4 <- 0
  mlaroi4 <- 0
  mlan4 <- 0
  mlhroi4 <- 0
  mlhn4 <- 0
  aroi5 <- 0
  an5 <- 0
  hroi5 <- 0
  hn5 <- 0
  mlaroi5 <- 0
  mlan5 <- 0
  mlhroi5 <- 0
  mlhn5 <- 0
  aroi6 <- 0
  an6 <- 0
  hroi6 <- 0
  hn6 <- 0
  mlaroi6 <- 0
  mlan6 <- 0
  mlhroi6 <- 0
  mlhn6 <- 0
  aroi7 <- 0
  an7 <- 0
  hroi7 <- 0
  hn7 <- 0
  mlaroi7 <- 0
  mlan7 <- 0
  mlhroi7 <- 0
  mlhn7 <- 0
  aroi8 <- 0
  an8 <- 0
  hroi8 <- 0
  hn8 <- 0
  mlaroi8 <- 0
  mlan8 <- 0
  mlhroi8 <- 0
  mlhn8 <- 0
  aroi9 <- 0
  an9 <- 0
  hroi9 <- 0
  hn9 <- 0
  mlaroi9 <- 0
  mlan9 <- 0
  mlhroi9 <- 0
  mlhn9 <- 0
  aroi10 <- 0
  an10 <- 0
  hroi10 <- 0
  hn10 <- 0
  mlaroi10 <- 0
  mlan10 <- 0
  mlhroi10 <- 0
  mlhn10 <- 0
  aroi11 <- 0
  an11 <- 0
  hroi11 <- 0
  hn11 <- 0
  mlaroi11 <- 0
  mlan11 <- 0
  mlhroi11 <- 0
  mlhn11 <- 0
  aroi12 <- 0
  an12 <- 0
  hroi12 <- 0
  hn12 <- 0
  mlaroi12 <- 0
  mlan12 <- 0
  mlhroi12 <- 0
  mlhn12 <- 0
  aroi <- vector("list",length(action_all_df_today$away_team))
  aroin <- vector("list",length(action_all_df_today$away_team))
  hroi <- vector("list",length(action_all_df_today$away_team))
  hroin <- vector("list",length(action_all_df_today$away_team))
  mlaroi <- vector("list",length(action_all_df_today$away_team))
  mlaroin <- vector("list",length(action_all_df_today$away_team))
  mlhroi <- vector("list",length(action_all_df_today$away_team))
  mlhroin <- vector("list",length(action_all_df_today$away_team))
  for(p in 1:length(action_all_df_today$away_team)){
    
    asd_ml_df <- action_all_df[which(as.numeric(action_all_df$asd_ml)>action_all_df_today$asd_ml[p]),]
    aroi1[p] <- round(sum(asd_ml_df$ac)/length(asd_ml_df$ac),4)
    an1[p] <- length(asd_ml_df$ac)
    hroi1[p] <- round(sum(asd_ml_df$hc)/length(asd_ml_df$hc),4)
    hn1[p] <- length(asd_ml_df$hc)
    mlaroi1[p] <- round(sum(asd_ml_df$amlc)/length(asd_ml_df$amlc),4)
    mlan1[p] <- length(asd_ml_df$amlc)
    mlhroi1[p] <- round(sum(asd_ml_df$hmlc)/length(asd_ml_df$hmlc),4)
    mlhn1[p] <- length(asd_ml_df$hmlc)
    
    hsd_ml_df <- action_all_df[which(as.numeric(action_all_df$hsd_ml)>action_all_df_today$hsd_ml[p]),]
    aroi2[p] <- round(sum(hsd_ml_df$ac)/length(hsd_ml_df$ac),4)
    an2[p] <- length(hsd_ml_df$ac)
    hroi2[p] <- round(sum(hsd_ml_df$hc)/length(hsd_ml_df$hc),4)
    hn2[p] <- length(hsd_ml_df$hc)
    mlaroi2[p] <- round(sum(hsd_ml_df$amlc)/length(hsd_ml_df$amlc),4)
    mlan2[p] <- length(hsd_ml_df$amlc)
    mlhroi2[p] <- round(sum(hsd_ml_df$hmlc)/length(hsd_ml_df$hmlc),4)
    mlhn2[p] <- length(hsd_ml_df$hmlc)
    
    asd_s_df <- action_all_df[which(as.numeric(action_all_df$asd_s)>action_all_df_today$asd_s[p]),]
    aroi3[p] <- round(sum(asd_s_df$ac)/length(asd_s_df$ac),4)
    an3[p] <- length(asd_s_df$ac)
    hroi3[p] <- round(sum(asd_s_df$hc)/length(asd_s_df$hc),4)
    hn3[p] <- length(asd_s_df$hc)
    mlaroi3[p] <- round(sum(asd_s_df$amlc)/length(asd_s_df$amlc),4)
    mlan3[p] <- length(asd_s_df$amlc)
    mlhroi3[p] <- round(sum(asd_s_df$hmlc)/length(asd_s_df$hmlc),4)
    mlhn3[p] <- length(asd_s_df$hmlc)
    
    hsd_s_df <- action_all_df[which(as.numeric(action_all_df$hsd_s)>action_all_df_today$hsd_s[p]),]
    aroi4[p] <- round(sum(hsd_s_df$ac)/length(hsd_s_df$ac),4)
    an4[p] <- length(hsd_s_df$ac)
    hroi4[p] <- round(sum(hsd_s_df$hc)/length(hsd_s_df$hc),4)
    hn4[p] <- length(hsd_s_df$hc)
    mlaroi4[p] <- round(sum(hsd_s_df$amlc)/length(hsd_s_df$amlc),4)
    mlan4[p] <- length(hsd_s_df$amlc)
    mlhroi4[p] <- round(sum(hsd_s_df$hmlc)/length(hsd_s_df$hmlc),4)
    mlhn4[p] <- length(hsd_s_df$hmlc)
    
    ap_s_df <- action_all_df[which(as.numeric(action_all_df$public_away)>action_all_df_today$public_away[p]),]
    aroi5[p] <- round(sum(ap_s_df$ac)/length(ap_s_df$ac),4)
    an5[p] <- length(ap_s_df$ac)
    hroi5[p] <- round(sum(ap_s_df$hc)/length(ap_s_df$hc),4)
    hn5[p] <- length(ap_s_df$hc)
    mlaroi5[p] <- round(sum(ap_s_df$amlc)/length(ap_s_df$amlc),4)
    mlan5[p] <- length(ap_s_df$amlc)
    mlhroi5[p] <- round(sum(ap_s_df$hmlc)/length(ap_s_df$hmlc),4)
    mlhn5[p] <- length(ap_s_df$hmlc)
    
    hp_s_df <- action_all_df[which(as.numeric(action_all_df$public_home)>action_all_df_today$public_home[p]),]
    aroi6[p] <- round(sum(hp_s_df$ac)/length(hp_s_df$ac),4)
    an6[p] <- length(hp_s_df$ac)
    hroi6[p] <- round(sum(hp_s_df$hc)/length(hp_s_df$hc),4)
    hn6[p] <- length(hp_s_df$hc)
    mlaroi6[p] <- round(sum(hp_s_df$amlc)/length(hp_s_df$amlc),4)
    mlan6[p] <- length(hp_s_df$amlc)
    mlhroi6[p] <- round(sum(hp_s_df$hmlc)/length(hp_s_df$hmlc),4)
    mlhn6[p] <- length(hp_s_df$hmlc)
    
    am_s_df <- action_all_df[which(as.numeric(action_all_df$money_away)>action_all_df_today$money_away[p]),]
    aroi7[p] <- round(sum(am_s_df$ac)/length(am_s_df$ac),4)
    an7[p] <- length(am_s_df$ac)
    hroi7[p] <- round(sum(am_s_df$hc)/length(am_s_df$hc),4)
    hn7[p] <- length(am_s_df$hc)
    mlaroi7[p] <- round(sum(am_s_df$amlc)/length(am_s_df$amlc),4)
    mlan7[p] <- length(am_s_df$amlc)
    mlhroi7[p] <- round(sum(am_s_df$hmlc)/length(am_s_df$hmlc),4)
    mlhn7[p] <- length(am_s_df$hmlc)
    
    hm_s_df <- action_all_df[which(as.numeric(action_all_df$money_home)>action_all_df_today$money_home[p]),]
    aroi8[p] <- round(sum(hm_s_df$ac)/length(hm_s_df$ac),4)
    an8[p] <- length(hm_s_df$ac)
    hroi8[p] <- round(sum(hm_s_df$hc)/length(hm_s_df$hc),4)
    hn8[p] <- length(hm_s_df$hc)
    mlaroi8[p] <- round(sum(hm_s_df$amlc)/length(hm_s_df$amlc),4)
    mlan8[p] <- length(hm_s_df$amlc)
    mlhroi8[p] <- round(sum(hm_s_df$hmlc)/length(hm_s_df$hmlc),4)
    mlhn8[p] <- length(hm_s_df$hmlc)
    
    ap_ml_df <- action_all_df[which(as.numeric(action_all_df$public_away_ml)>action_all_df_today$public_away_ml[p]),]
    aroi9[p] <- round(sum(ap_ml_df$ac)/length(ap_ml_df$ac),4)
    an9[p] <- length(ap_ml_df$ac)
    hroi9[p] <- round(sum(ap_ml_df$hc)/length(ap_ml_df$hc),4)
    hn9[p] <- length(ap_ml_df$hc)
    mlaroi9[p] <- round(sum(ap_ml_df$amlc)/length(ap_ml_df$amlc),4)
    mlan9[p] <- length(ap_ml_df$amlc)
    mlhroi9[p] <- round(sum(ap_ml_df$hmlc)/length(ap_ml_df$hmlc),4)
    mlhn9[p] <- length(ap_ml_df$hmlc)
    
    hp_ml_df <- action_all_df[which(as.numeric(action_all_df$public_home_ml)>action_all_df_today$public_home_ml[p]),]
    aroi10[p] <- round(sum(hp_ml_df$ac)/length(hp_ml_df$ac),4)
    an10[p] <- length(hp_ml_df$ac)
    hroi10[p] <- round(sum(hp_ml_df$hc)/length(hp_ml_df$hc),4)
    hn10[p] <- length(hp_ml_df$hc)
    mlaroi10[p] <- round(sum(hp_ml_df$amlc)/length(hp_ml_df$amlc),4)
    mlan10[p] <- length(hp_ml_df$amlc)
    mlhroi10[p] <- round(sum(hp_ml_df$hmlc)/length(hp_ml_df$hmlc),4)
    mlhn10[p] <- length(hp_ml_df$hmlc)
    
    am_ml_df <- action_all_df[which(as.numeric(action_all_df$money_away_ml)>action_all_df_today$money_away_ml[p]),]
    aroi11[p] <- round(sum(am_ml_df$ac)/length(am_ml_df$ac),4)
    an11[p] <- length(am_ml_df$ac)
    hroi11[p] <- round(sum(am_ml_df$hc)/length(am_ml_df$hc),4)
    hn11[p] <- length(am_ml_df$hc)
    mlaroi11[p] <- round(sum(am_ml_df$amlc)/length(am_ml_df$amlc),4)
    mlan11[p] <- length(am_ml_df$amlc)
    mlhroi11[p] <- round(sum(am_ml_df$hmlc)/length(am_ml_df$hmlc),4)
    mlhn11[p] <- length(am_ml_df$hmlc)
    
    hm_ml_df <- action_all_df[which(as.numeric(action_all_df$money_home_ml)>action_all_df_today$money_home_ml[p]),]
    aroi12[p] <- round(sum(hm_ml_df$ac)/length(hm_ml_df$ac),4)
    an12[p] <- length(hm_ml_df$ac)
    hroi12[p] <- round(sum(hm_ml_df$hc)/length(hm_ml_df$hc),4)
    hn12[p] <- length(hm_ml_df$hc)
    mlaroi12[p] <- round(sum(hm_ml_df$amlc)/length(hm_ml_df$amlc),4)
    mlan12[p] <- length(hm_ml_df$amlc)
    mlhroi12[p] <- round(sum(hm_ml_df$hmlc)/length(hm_ml_df$hmlc),4)
    mlhn12[p] <- length(hm_ml_df$hmlc)
    
    aroi[[p]] <- c(aroi1[p],aroi2[p],aroi3[p],aroi4[p],
                   aroi5[p],aroi6[p],aroi9[p],aroi10[p],aroi7[p],aroi8[p],
                   aroi11[p],aroi12[p])
    aroin[[p]] <- c(an1[p],an2[p],an3[p],an4[p],
                    an5[p],an6[p],an9[p],an10[p],an7[p],an8[p],
                    an11[p],an12[p])
    hroi[[p]] <- c(hroi1[p],hroi2[p],hroi3[p],hroi4[p],
                   hroi5[p],hroi6[p],hroi9[p],hroi10[p],hroi7[p],hroi8[p],
                   hroi11[p],hroi12[p])
    hroin[[p]] <- c(hn1[p],hn2[p],hn3[p],hn4[p],
                    hn5[p],hn6[p],hn9[p],hn10[p],hn7[p],hn8[p],
                    hn11[p],hn12[p])
    mlaroi[[p]] <- c(mlaroi1[p],mlaroi2[p],mlaroi3[p],mlaroi4[p],
                     mlaroi5[p],mlaroi6[p], mlaroi9[p],mlaroi10[p],mlaroi7[p],mlaroi8[p],
                     mlaroi11[p],mlaroi12[p])
    mlaroin[[p]] <- c(mlan1[p],mlan2[p],mlan3[p],mlan4[p],
                      mlan5[p],mlan6[p],mlan9[p],mlan10[p],mlan7[p],mlan8[p],
                      mlan11[p],mlan12[p])
    mlhroi[[p]] <- c(mlhroi1[p],mlhroi2[p],mlhroi3[p],mlhroi4[p],
                     mlhroi5[p],mlhroi6[p],mlhroi9[p],mlhroi10[p],mlhroi7[p],mlhroi8[p],
                     mlhroi11[p],mlhroi12[p])
    mlhroin[[p]] <- c(mlhn1[p],mlhn2[p],mlhn3[p],mlhn4[p],
                      mlhn5[p],mlhn6[p],mlhn9[p],mlhn10[p],mlhn7[p],mlhn8[p],
                      mlhn11[p],mlhn12[p])
    
  }
  
  
  nba_analysis <- list(aroi = aroi,
                       aroin  = aroin,
                       hroi  = hroi,
                       hroin  = hroin,
                       mlaroi  = mlaroi,
                       mlaroin  = mlaroin,
                       mlhroi  = mlhroi,
                       mlhroin  = mlhroin)
  
  sagame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","hsd_s","asd_s","hsd_ml","asd_ml")]
  pagame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","public_home","public_away","public_home_ml","public_away_ml")]
  magame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","money_home","money_away","money_home_ml","money_away_ml")]
  
  sagame_df$aroiMLASHARP <- 0
  sagame_df$aroinMLASHARP <- 0
  sagame_df$aroiMLHSHARP <- 0
  sagame_df$aroinMLHSHARP<- 0
  sagame_df$aroiSASHARP <- 0
  sagame_df$aroinSASHARP<- 0
  sagame_df$aroiSHSHARP <- 0
  sagame_df$aroinSHSHARP<- 0
  pagame_df$aroiAPUBLIC <- 0
  pagame_df$aroinAPUBLIC<- 0
  pagame_df$aroiHPUBLIC <- 0
  pagame_df$aroinHPUBLIC<- 0
  pagame_df$aroiAMLPUBLIC <- 0
  pagame_df$aroinAMLPUBLIC<- 0
  pagame_df$aroiHMLPUBLIC <- 0
  pagame_df$aroinHMLPUBLIC<- 0
  magame_df$aroiAMONEY <- 0
  magame_df$aroinAMONEY<- 0
  magame_df$aroiHMONEY <- 0
  magame_df$aroinHMONEY<- 0
  magame_df$aroiAMLMONEY <- 0
  magame_df$aroinAMLMONEY<- 0
  magame_df$aroiHMLMONEY <- 0
  magame_df$aroinHMLMONEY <- 0
  
  
  sagame_df$mlaroiMLASHARP <- 0
  sagame_df$mlaroinMLASHARP <- 0
  sagame_df$mlaroiMLHSHARP <- 0
  sagame_df$mlaroinMLHSHARP<- 0
  sagame_df$mlaroiSASHARP <- 0
  sagame_df$mlaroinSASHARP<- 0
  sagame_df$mlaroiSHSHARP <- 0
  sagame_df$mlaroinSHSHARP<- 0
  pagame_df$mlaroiAPUBLIC <- 0
  pagame_df$mlaroinAPUBLIC<- 0
  pagame_df$mlaroiHPUBLIC <- 0
  pagame_df$mlaroinHPUBLIC<- 0
  pagame_df$mlaroiAMLPUBLIC <- 0
  pagame_df$mlaroinAMLPUBLIC<- 0
  pagame_df$mlaroiHMLPUBLIC <- 0
  pagame_df$mlaroinHMLPUBLIC<- 0
  magame_df$mlaroiAMONEY <- 0
  magame_df$mlaroinAMONEY<- 0
  magame_df$mlaroiHMONEY <- 0
  magame_df$mlaroinHMONEY<- 0
  magame_df$mlaroiAMLMONEY <- 0
  magame_df$mlaroinAMLMONEY<- 0
  magame_df$mlaroiHMLMONEY <- 0
  magame_df$mlaroinHMLMONEY <- 0
  
  
  for(g in 1:length(nba_analysis$aroi)){
    sagame_df$aroiMLASHARP[g] <- nba_analysis$aroi[[g]][1]
    sagame_df$aroiMLHSHARP[g] <- nba_analysis$aroi[[g]][2]
    sagame_df$aroiSASHARP[g] <- nba_analysis$aroi[[g]][3]
    sagame_df$aroiSHSHARP[g] <- nba_analysis$aroi[[g]][4]
    pagame_df$aroiAPUBLIC[g] <- nba_analysis$aroi[[g]][5]
    pagame_df$aroiHPUBLIC[g] <- nba_analysis$aroi[[g]][6]
    pagame_df$aroiAMLPUBLIC[g] <- nba_analysis$aroi[[g]][7]
    pagame_df$aroiHMLPUBLIC[g] <- nba_analysis$aroi[[g]][8]
    magame_df$aroiAMONEY[g] <- nba_analysis$aroi[[g]][9]
    magame_df$aroiHMONEY[g] <- nba_analysis$aroi[[g]][10]
    magame_df$aroiAMLMONEY[g] <- nba_analysis$aroi[[g]][11]
    magame_df$aroiHMLMONEY[g] <- nba_analysis$aroi[[g]][12]
    
    sagame_df$aroinMLASHARP[g] <- nba_analysis$aroin[[g]][1]
    sagame_df$aroinMLHSHARP[g] <- nba_analysis$aroin[[g]][2]
    sagame_df$aroinSASHARP[g] <- nba_analysis$aroin[[g]][3]
    sagame_df$aroinSHSHARP[g] <- nba_analysis$aroin[[g]][4]
    pagame_df$aroinAPUBLIC[g] <- nba_analysis$aroin[[g]][5]
    pagame_df$aroinHPUBLIC[g] <- nba_analysis$aroin[[g]][6]
    pagame_df$aroinAMLPUBLIC[g] <- nba_analysis$aroin[[g]][7]
    pagame_df$aroinHMLPUBLIC[g] <- nba_analysis$aroin[[g]][8]
    magame_df$aroinAMONEY[g] <- nba_analysis$aroin[[g]][9]
    magame_df$aroinHMONEY[g] <- nba_analysis$aroin[[g]][10]
    magame_df$aroinAMLMONEY[g] <- nba_analysis$aroin[[g]][11]
    magame_df$aroinHMLMONEY[g] <- nba_analysis$aroin[[g]][12]
    
    sagame_df$mlaroiMLASHARP[g] <- nba_analysis$mlaroi[[g]][1]
    sagame_df$mlaroiMLHSHARP[g] <- nba_analysis$mlaroi[[g]][2]
    sagame_df$mlaroiSASHARP[g] <- nba_analysis$mlaroi[[g]][3]
    sagame_df$mlaroiSHSHARP[g] <- nba_analysis$mlaroi[[g]][4]
    pagame_df$mlaroiAPUBLIC[g] <- nba_analysis$mlaroi[[g]][5]
    pagame_df$mlaroiHPUBLIC[g] <- nba_analysis$mlaroi[[g]][6]
    pagame_df$mlaroiAMLPUBLIC[g] <- nba_analysis$mlaroi[[g]][7]
    pagame_df$mlaroiHMLPUBLIC[g] <- nba_analysis$mlaroi[[g]][8]
    magame_df$mlaroiAMONEY[g] <- nba_analysis$mlaroi[[g]][9]
    magame_df$mlaroiHMONEY[g] <- nba_analysis$mlaroi[[g]][10]
    magame_df$mlaroiAMLMONEY[g] <- nba_analysis$mlaroi[[g]][11]
    magame_df$mlaroiHMLMONEY[g] <- nba_analysis$mlaroi[[g]][12]
    
    sagame_df$mlaroinMLASHARP[g] <- nba_analysis$mlaroin[[g]][1]
    sagame_df$mlaroinMLHSHARP[g] <- nba_analysis$mlaroin[[g]][2]
    sagame_df$mlaroinSASHARP[g] <- nba_analysis$mlaroin[[g]][3]
    sagame_df$mlaroinSHSHARP[g] <- nba_analysis$mlaroin[[g]][4]
    pagame_df$mlaroinAPUBLIC[g] <- nba_analysis$mlaroin[[g]][5]
    pagame_df$mlaroinHPUBLIC[g] <- nba_analysis$mlaroin[[g]][6]
    pagame_df$mlaroinAMLPUBLIC[g] <- nba_analysis$mlaroin[[g]][7]
    pagame_df$mlaroinHMLPUBLIC[g] <- nba_analysis$mlaroin[[g]][8]
    magame_df$mlaroinAMONEY[g] <- nba_analysis$mlaroin[[g]][9]
    magame_df$mlaroinHMONEY[g] <- nba_analysis$mlaroin[[g]][10]
    magame_df$mlaroinAMLMONEY[g] <- nba_analysis$mlaroin[[g]][11]
    magame_df$mlaroinHMLMONEY[g] <- nba_analysis$mlaroin[[g]][12]
  }
  
  shgame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","hsd_s","asd_s","hsd_ml","asd_ml")]
  phgame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","public_home","public_away","public_home_ml","public_away_ml")]
  mhgame_df <- action_all_df_today[,c("away_team","home_team","away_spread","home_spread","away_ml","home_ml","away_spread_line","home_spread_line","est_time","money_home","money_away","money_home_ml","money_away_ml")]
  
  shgame_df$hroiMLASHARP <- 0
  shgame_df$hroinMLASHARP <- 0
  shgame_df$hroiMLHSHARP <- 0
  shgame_df$hroinMLHSHARP<- 0
  shgame_df$hroiSASHARP <- 0
  shgame_df$hroinSASHARP<- 0
  shgame_df$hroiSHSHARP <- 0
  shgame_df$hroinSHSHARP<- 0
  phgame_df$hroiAPUBLIC <- 0
  phgame_df$hroinAPUBLIC<- 0
  phgame_df$hroiHPUBLIC <- 0
  phgame_df$hroinHPUBLIC<- 0
  phgame_df$hroiAMLPUBLIC <- 0
  phgame_df$hroinAMLPUBLIC<- 0
  phgame_df$hroiHMLPUBLIC <- 0
  phgame_df$hroinHMLPUBLIC<- 0
  mhgame_df$hroiAMONEY <- 0
  mhgame_df$hroinAMONEY<- 0
  mhgame_df$hroiHMONEY <- 0
  mhgame_df$hroinHMONEY<- 0
  mhgame_df$hroiAMLMONEY <- 0
  mhgame_df$hroinAMLMONEY<- 0
  mhgame_df$hroiHMLMONEY <- 0
  mhgame_df$hroinHMLMONEY <- 0
  
  
  shgame_df$mlhroiMLASHARP <- 0
  shgame_df$mlhroinMLASHARP <- 0
  shgame_df$mlhroiMLHSHARP <- 0
  shgame_df$mlhroinMLHSHARP<- 0
  shgame_df$mlhroiSASHARP <- 0
  shgame_df$mlhroinSASHARP<- 0
  shgame_df$mlhroiSHSHARP <- 0
  shgame_df$mlhroinSHSHARP<- 0
  phgame_df$mlhroiAPUBLIC <- 0
  phgame_df$mlhroinAPUBLIC<- 0
  phgame_df$mlhroiHPUBLIC <- 0
  phgame_df$mlhroinHPUBLIC<- 0
  phgame_df$mlhroiAMLPUBLIC <- 0
  phgame_df$mlhroinAMLPUBLIC<- 0
  phgame_df$mlhroiHMLPUBLIC <- 0
  phgame_df$mlhroinHMLPUBLIC<- 0
  mhgame_df$mlhroiAMONEY <- 0
  mhgame_df$mlhroinAMONEY<- 0
  mhgame_df$mlhroiHMONEY <- 0
  mhgame_df$mlhroinHMONEY<- 0
  mhgame_df$mlhroiAMLMONEY <- 0
  mhgame_df$mlhroinAMLMONEY<- 0
  mhgame_df$mlhroiHMLMONEY <- 0
  mhgame_df$mlhroinHMLMONEY <- 0
  
  
  for(g in 1:length(nba_analysis$hroi)){
    shgame_df$hroiMLASHARP[g] <- nba_analysis$hroi[[g]][1]
    shgame_df$hroiMLHSHARP[g] <- nba_analysis$hroi[[g]][2]
    shgame_df$hroiSASHARP[g] <- nba_analysis$hroi[[g]][3]
    shgame_df$hroiSHSHARP[g] <- nba_analysis$hroi[[g]][4]
    phgame_df$hroiAPUBLIC[g] <- nba_analysis$hroi[[g]][5]
    phgame_df$hroiHPUBLIC[g] <- nba_analysis$hroi[[g]][6]
    phgame_df$hroiAMLPUBLIC[g] <- nba_analysis$hroi[[g]][7]
    phgame_df$hroiHMLPUBLIC[g] <- nba_analysis$hroi[[g]][8]
    mhgame_df$hroiAMONEY[g] <- nba_analysis$hroi[[g]][9]
    mhgame_df$hroiHMONEY[g] <- nba_analysis$hroi[[g]][10]
    mhgame_df$hroiAMLMONEY[g] <- nba_analysis$hroi[[g]][11]
    mhgame_df$hroiHMLMONEY[g] <- nba_analysis$hroi[[g]][12]
    
    shgame_df$hroinMLASHARP[g] <- nba_analysis$hroin[[g]][1]
    shgame_df$hroinMLHSHARP[g] <- nba_analysis$hroin[[g]][2]
    shgame_df$hroinSASHARP[g] <- nba_analysis$hroin[[g]][3]
    shgame_df$hroinSHSHARP[g] <- nba_analysis$hroin[[g]][4]
    phgame_df$hroinAPUBLIC[g] <- nba_analysis$hroin[[g]][5]
    phgame_df$hroinHPUBLIC[g] <- nba_analysis$hroin[[g]][6]
    phgame_df$hroinAMLPUBLIC[g] <- nba_analysis$hroin[[g]][7]
    phgame_df$hroinHMLPUBLIC[g] <- nba_analysis$hroin[[g]][8]
    mhgame_df$hroinAMONEY[g] <- nba_analysis$hroin[[g]][9]
    mhgame_df$hroinHMONEY[g] <- nba_analysis$hroin[[g]][10]
    mhgame_df$hroinAMLMONEY[g] <- nba_analysis$hroin[[g]][11]
    mhgame_df$hroinHMLMONEY[g] <- nba_analysis$hroin[[g]][12]
    
    shgame_df$mlhroiMLASHARP[g] <- nba_analysis$mlhroi[[g]][1]
    shgame_df$mlhroiMLHSHARP[g] <- nba_analysis$mlhroi[[g]][2]
    shgame_df$mlhroiSASHARP[g] <- nba_analysis$mlhroi[[g]][3]
    shgame_df$mlhroiSHSHARP[g] <- nba_analysis$mlhroi[[g]][4]
    phgame_df$mlhroiAPUBLIC[g] <- nba_analysis$mlhroi[[g]][5]
    phgame_df$mlhroiHPUBLIC[g] <- nba_analysis$mlhroi[[g]][6]
    phgame_df$mlhroiAMLPUBLIC[g] <- nba_analysis$mlhroi[[g]][7]
    phgame_df$mlhroiHMLPUBLIC[g] <- nba_analysis$mlhroi[[g]][8]
    mhgame_df$mlhroiAMONEY[g] <- nba_analysis$mlhroi[[g]][9]
    mhgame_df$mlhroiHMONEY[g] <- nba_analysis$mlhroi[[g]][10]
    mhgame_df$mlhroiAMLMONEY[g] <- nba_analysis$mlhroi[[g]][11]
    mhgame_df$mlhroiHMLMONEY[g] <- nba_analysis$mlhroi[[g]][12]
    
    shgame_df$mlhroinMLASHARP[g] <- nba_analysis$mlhroin[[g]][1]
    shgame_df$mlhroinMLHSHARP[g] <- nba_analysis$mlhroin[[g]][2]
    shgame_df$mlhroinSASHARP[g] <- nba_analysis$mlhroin[[g]][3]
    shgame_df$mlhroinSHSHARP[g] <- nba_analysis$mlhroin[[g]][4]
    phgame_df$mlhroinAPUBLIC[g] <- nba_analysis$mlhroin[[g]][5]
    phgame_df$mlhroinHPUBLIC[g] <- nba_analysis$mlhroin[[g]][6]
    phgame_df$mlhroinAMLPUBLIC[g] <- nba_analysis$mlhroin[[g]][7]
    phgame_df$mlhroinHMLPUBLIC[g] <- nba_analysis$mlhroin[[g]][8]
    mhgame_df$mlhroinAMONEY[g] <- nba_analysis$mlhroin[[g]][9]
    mhgame_df$mlhroinHMONEY[g] <- nba_analysis$mlhroin[[g]][10]
    mhgame_df$mlhroinAMLMONEY[g] <- nba_analysis$mlhroin[[g]][11]
    mhgame_df$mlhroinHMLMONEY[g] <- nba_analysis$mlhroin[[g]][12]
  }
  
  
  udf_games_today <- action_all_df_today[,c("time","favorite","underdog","favorite_score", "underdog_score", "favorite_spread","underdog_spread","favorite_ml",   
                                            "underdog_ml",    "favorite_spread_line" ,"underdog_spread_line", 
                                            "favorite_public","underdog_public","favorite_money", "underdog_money", "favorite_sharp", "underdog_sharp", "mlfavorite_public",    "mlunderdog_public"  , 
                                            "mlfavorite_money" ,    "mlunderdog_money"    , "mlfavorite_sharp"    , "mlunderdog_sharp")]
  
  nba_games <- list(sagame_df,
                    pagame_df,
                    magame_df,
                    shgame_df,
                    phgame_df,
                    mhgame_df,
                    udf_games_today)
  
}




