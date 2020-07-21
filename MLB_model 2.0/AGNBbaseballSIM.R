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
paste0()



stats <- stats_function()
depth_chart_list <- depth_charts()
lineups_df <- todays_lineups()
awayteam <- lineups_df$team[7]
hometeam <- lineups_df$team[8]
Catcherhome <- Catcherfx(hometeam,lineups_df,stats)
Catcheraway <- Catcherfx(awayteam,lineups_df,stats)
Pitcherhome <- SPfx(hometeam,lineups_df,stats)
Pitcheraway <- SPfx(awayteam,lineups_df,stats) ###############NEED TO FIX 
BattingOrderhome <- BOfx(hometeam, lineups_df, Pitcheraway)
BattingOrderaway <- BOfx(awayteam, lineups_df, Pitcherhome)
