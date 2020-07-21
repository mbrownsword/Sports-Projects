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
setwd("C:/Users/mattb.EMR/Desktop/Old Desktop/MLB_model 2.0")
####### Zipped-up Steamer ##########

stats_function <- function(){
  rD <- rsDriver(port = 4431L, browser="chrome", chromever = "80.0.3987.16")
  remDr <- rD$client
  
  
  proj <- 0
  proj[1] <- "zips"
  proj[2] <- "uzips"
  proj[3] <- "steamer"
  proj[4] <- "steameru"
  
  for(a in 1:length(proj)){
    fangraphs_projurl <- paste0("https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=",proj[a],"&team=0&lg=all&players=0")
    remDr$navigate(fangraphs_projurl)
    Sys.sleep(25)
    the_stuffing_proj <- read_html(remDr$getPageSource()[[1]])
    proj_table <- the_stuffing_proj %>% 
      html_nodes(xpath='//table[@class="rgMasterTable"]') %>% 
      html_table()
    if(length(proj_table[[1]][,1])==1){
      elem_clicktodownload <- remDr$findElement(using = "xpath", '//a[@id="ProjectionBoard1_cmdCSV"]')
      elem_clicktodownload$clickElement()
      if(a==1){
        zips_batters <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==2){
        zips_updated_batters <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==3){
        steamer_batters <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==4){
        steamer_updated_batters <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      next()
    }else{
      elem_clicktodownload <- remDr$findElement(using = "xpath", '//a[@id="ProjectionBoard1_cmdCSV"]')
      elem_clicktodownload$clickElement()
      setwd("C:/Users/mattb.EMR/Downloads")
      if(a==1){
        Sys.sleep(15)
        zips_batters <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==2){
        Sys.sleep(15)
        zips_updated_batters <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==3){
        Sys.sleep(15)
        steamer_batters <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==4){
        Sys.sleep(15)
        steamer_updated_batters <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
    }
    
  }
  
  
  for(a in 1:length(proj)){
    fangraphs_projurl <- paste0("https://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=",proj[a],"&team=0&lg=all&players=0")
    remDr$navigate(fangraphs_projurl)
    Sys.sleep(25)
    the_stuffing_proj <- read_html(remDr$getPageSource()[[1]])
    proj_table <- the_stuffing_proj %>% 
      html_nodes(xpath='//table[@class="rgMasterTable"]') %>% 
      html_table()
    if(length(proj_table[[1]][,1])==1){
      elem_clicktodownload <- remDr$findElement(using = "xpath", '//a[@id="ProjectionBoard1_cmdCSV"]')
      elem_clicktodownload$clickElement()
      if(a==1){
        zips_pitchers <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==2){
        zips_updated_pitchers <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==3){
        steamer_pitchers <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==4){
        steamer_updated_pitchers <- proj_table[[1]]
        Sys.sleep(5)
        unlink("FanGraphs Leaderboard.csv")
      }
      next()
    }else{
      elem_clicktodownload <- remDr$findElement(using = "xpath", '//a[@id="ProjectionBoard1_cmdCSV"]')
      elem_clicktodownload$clickElement()
      setwd("C:/Users/mattb.EMR/Downloads")
      if(a==1){
        Sys.sleep(15)
        zips_pitchers <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==2){
        Sys.sleep(15)
        zips_updated_pitchers <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==3){
        Sys.sleep(15)
        steamer_pitchers <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
      if(a==4){
        Sys.sleep(15)
        steamer_updated_pitchers <- read.csv(paste0("FanGraphs Leaderboard.csv"))
        unlink("FanGraphs Leaderboard.csv")
      }
    }
    
  }
  
  
  remDr$close()
  rD$server$stop()
  
  stats <- list(ZUB = zips_updated_batters,
                ZB = zips_batters,
                SUB = steamer_updated_batters,
                SB = steamer_batters,
                ZUP = zips_updated_pitchers,
                ZP = zips_pitchers,
                SUP = steamer_updated_pitchers,
                SP = steamer_pitchers)
}

stats <- stats_function()
######## mlb depth charts ###########

depth_charts <- function(){
  espn_url <- paste0("https://www.espn.com/mlb/teams")
  the_stuffing_mlbteams <- read_html(espn_url)
  mlb_depthchartlinks <- the_stuffing_mlbteams %>% 
    html_nodes(xpath='//span[@class="TeamLinks__Link n9 nowrap"][4]//a//@href') %>% 
    html_text()
  depth_chart_list <- vector("list",30)
  depth_charts <- 0
  teams_espn <- 0
  for(z in 1:length(mlb_depthchartlinks)){ 
    teams_espn[z] <- strsplit(mlb_depthchartlinks[z],"/mlb/team/depth/_/name/")[[1]][2] 
  }
  
  for(a in 1:length(mlb_depthchartlinks)){
    dc_url <- paste0("https://www.espn.com",mlb_depthchartlinks[a])
    the_stuffing_mlbdc <- read_html(dc_url)
    depthchart_tables <- the_stuffing_mlbdc %>% 
      html_nodes(xpath='//table[@class="Table"]') %>% 
      html_table()
    rownames_dc <- the_stuffing_mlbdc %>% 
      html_nodes(xpath='//table[@class="Table Table--fixed Table--fixed-left"]') %>% 
      html_table()
    depth_chart_list[[a]]$team <- teams_espn[a]
    depth_chart_list[[a]]$depthchart <- data.frame(depthchart_tables)
    rownames(depth_chart_list[[a]]$depthchart) <- unlist(rownames_dc[[1]])
  }
  depth_chart_list <- depth_chart_list
  
}

depth_chart_list <- depth_charts()

####### the days lineups ##########
mlb_teams <- read_xlsx("mlb_library.xlsx")

todays_lineups <- function(){
  depth_chart_list <- depth_charts()
  lineups_df <- data.frame(matrix(as.character(""),nrow = length(mlb_teams_today),ncol = 31), stringsAsFactors = FALSE)
  colnames(lineups_df) <- c("lineupstatus","team","startingpitcher","pitcherhand",
                            "batter1","batter2","batter3","batter4","batter5","batter6","batter7","batter8","batter9",
                            "batter1hand","batter2hand","batter3hand","batter4hand","batter5hand","batter6hand","batter7hand","batter8hand","batter9hand",
                            "batter1position","batter2position","batter3position","batter4position","batter5position","batter6position","batter7position","batter8position","batter9position")
  
  mlbdotcom_url <- paste0("https://www.rotowire.com/baseball/daily-lineups.php")
  the_stuffing_mlblineups <- read_html(mlbdotcom_url)
  mlb_teams_today <- the_stuffing_mlblineups %>% 
    html_nodes(xpath='//div[@class="lineup__abbr"]') %>% 
    html_text()
  starting_pitchers <- the_stuffing_mlblineups %>% 
    html_nodes(xpath='//li[@class="lineup__player-highlight mb-0"]//a') %>% 
    html_text()
  pitcher_position <- the_stuffing_mlblineups %>% 
    html_nodes(xpath='//li[@class="lineup__player-highlight mb-0"]//span') %>% 
    html_text()
  
  status_lineup <- the_stuffing_mlblineups %>% 
    html_nodes(xpath='//div[@class="lineup__main"]//ul//li[2]') %>% 
    html_text()
  for(a in 1:length(status_lineup)){
    status_lineup[a] <- trimws(status_lineup[a])
  }
  q <- 1
  lineup_players <- vector("list",1)
  bats <- vector("list",1)
  position <- vector("list",1)
  for(b in 1:length(mlb_teams_today)){
    team <- mlb_teams$espn_name[mlb_teams$roto_name==mlb_teams_today[b]]
    if(status_lineup[b]=="Unknown Lineup"){
      print("error")
      break()
    }else{
      if(status_lineup[b]=="Expected Lineup"){
        if((b %% 2) == 0){
          xpath_start <- paste0('//div[@class=\"lineup__main\"]//ul[@class="lineup__list is-home"]//li[@class="lineup__player"]')
          players <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,"//a//@title")) %>% 
            html_text()
          lineup_players[[b]] <- players[(((q-1)*9)+1):(q*9)]
          bats_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//span[@class="lineup__bats"]')) %>% 
            html_text()
          bats[[b]]  <- bats_[(((q-1)*9)+1):(q*9)]
          position_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//div[@class="lineup__pos"]')) %>% 
            html_text()
          position[[b]] <- position_[(((q-1)*9)+1):(q*9)]
        }else{
          xpath_start <- paste0('//div[@class=\"lineup__main\"]//ul[@class="lineup__list is-visit"]//li[@class="lineup__player"]')
          players <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,"//a//@title")) %>% 
            html_text()
          lineup_players[[b]] <- players[(((q-1)*9)+1):(q*9)]
          bats_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//span[@class="lineup__bats"]')) %>% 
            html_text()
          bats[[b]]  <- bats_[(((q-1)*9)+1):(q*9)]
          position_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//div[@class="lineup__pos"]')) %>% 
            html_text()
          position[[b]] <- position_[(((q-1)*9)+1):(q*9)]
        }
      }else{
        if((b %% 2) == 0){
          xpath_start <- paste0('//div[@class=\"lineup__main\"]//ul[@class="lineup__list is-home"]//li[@class="lineup__player"]')
          players <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,"//a//@title")) %>% 
            html_text()
          lineup_players[[b]] <- players[(((q-1)*9)+1):(q*9)]
          bats_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//span[@class="lineup__bats"]')) %>% 
            html_text()
          bats[[b]]  <- bats_[(((q-1)*9)+1):(q*9)]
          position_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//div[@class="lineup__pos"]')) %>% 
            html_text()
          position[[b]] <- position_[(((q-1)*9)+1):(q*9)]
        }else{
          xpath_start <- paste0('//div[@class=\"lineup__main\"]//ul[@class="lineup__list is-visit"]//li[@class="lineup__player"]')
          players <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,"//a//@title")) %>% 
            html_text()
          lineup_players[[b]] <- players[(((q-1)*9)+1):(q*9)]
          bats_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//span[@class="lineup__bats"]')) %>% 
            html_text()
          bats[[b]]  <- bats_[(((q-1)*9)+1):(q*9)]
          position_ <- the_stuffing_mlblineups %>% 
            html_nodes(xpath=paste0(xpath_start,'//div[@class="lineup__pos"]')) %>% 
            html_text()
          position[[b]] <- position_[(((q-1)*9)+1):(q*9)]
        }
      }
    }
    if(b==1){
      q <- q
    }else{
      if((b %% 2 == 0)){
        q <- q + 1
      }else{
        q <- q
      }
    }
  }
  
  
   
   for(d in 1:length(mlb_teams_today)){
    lineups_df$lineupstatus[d] <- status_lineup[d]
    lineups_df$team[d] <- mlb_teams_today[d]
    lineups_df$startingpitcher[d] <- starting_pitchers[d]
    lineups_df$pitcherhand[d] <- pitcher_position[d]
    lineups_df[d,5:13] <- unlist(lineup_players[[d]])
    lineups_df[d,14:22] <- unlist(bats[[d]])
    lineups_df[d,23:31] <- unlist(position[[d]])
  }
  lineups_df <- lineups_df
}
lineups_df <- todays_lineups()




######## Catcher ##########
team <- awayteam
lineups_df

Catcherfx <- function(team, lineups_df,stats){
  team_number <- which(lineups_df$team==team)
  catcher_name <- lineups_df[team_number,(which(lineups_df[team_number,]=="C")-18)]
  if(length(stats$ZUB$ï..Name[stats$ZUB$ï..Name==catcher_name])>0){
    catcher_id <- as.numeric(as.character(stats$ZUB$playerid[stats$ZUB$ï..Name==catcher_name]))
  }else{
    if(length(stats$SUB$ï..Name[stats$SUB$ï..Name==catcher_name])>0){
      catcher_id <- as.numeric(as.character(stats$SUB$playerid[stats$SUB$ï..Name==catcher_name]))
    }else{
      if(length(stats$ZB$ï..Name[stats$ZB$ï..Name==catcher_name])>0){
        catcher_id <- as.numeric(as.character(stats$ZB$playerid[stats$ZB$ï..Name==catcher_name]))
      }else{
        catcher_id <- as.numeric(as.character(stats$SB$playerid[stats$SB$ï..Name==catcher_name]))
      }
    }
  }
  Catcher <- data.frame(catcher_name,catcher_id)
  url <- paste0("https://www.fangraphs.com/statss.aspx?playerid=",Catcher$catcher_id)
  webpager <- read_html(url)
  tbls <- webpager %>%
    html_nodes(xpath='//table[@id="SeasonStats1_dgSeason8_ctl00"]') %>% 
    html_table(fill =TRUE)
  Fldngtbl <- tbls[[1]]
  Fldngtbl[is.na(Fldngtbl)] <- 0
  
  one <- matrix(0, 5, 1)
  dimnames(one)[[1]] <- c("SB", "stay", "CS",
                          "PB", "pitch")
  dimnames(one)[[2]] <- 1
  SB1 <- max(Fldngtbl$SB[Fldngtbl$Season=="2019"&Fldngtbl$Pos=="C"])
  CS1 <- max(Fldngtbl$CS[Fldngtbl$Season=="2019"&Fldngtbl$Pos=="C"])
  PB1 <- max(Fldngtbl$PB[Fldngtbl$Season=="2019"&Fldngtbl$Pos=="C"])
  stay1 <- max(Fldngtbl$Inn[Fldngtbl$Season=="2019"&Fldngtbl$Pos=="C"])*1.44319
  pitch1 <- max(Fldngtbl$Inn[Fldngtbl$Season=="2019"&Fldngtbl$Pos=="C"])*16.58
  
  one["SB",] <- SB1/(SB1+CS1+stay1)
  one["CS",] <- CS1/(SB1+CS1+stay1)
  one["stay",] <- stay1/(SB1+CS1+stay1)
  one["PB",] <- PB1/(PB1+pitch1)
  one["pitch",] <- pitch1/(PB1+pitch1)
  
  if(any(is.na(one))){
    one["SB",] <- .025
    one["CS",] <- .025
    one["stay",] <- .95
    one["PB",] <- .000005
    one["pitch",] <- .999995
  }
  
  
  Catcher <- list(one=one)
}


############### Starting Pitcher ##################
team <- hometeam
lineups_df
SPfx <- function(team,lineups_df,stats){
  pitcher_name <- lineups_df$startingpitcher[lineups_df$team==team]
  if(length(stats$ZUP$ï..Name[stats$ZUP$ï..Name==pitcher_name])>1){
    pitcher_id <- zas.numeric(as.character(stats$ZUP$playerid[stats$ZUP$ï..Name==pitcher_name]))
  }else{
    if(length(stats$SUP$ï..Name[stats$SUP$ï..Name==pitcher_name])>1){
      pitcher_id <- as.numeric(as.character(stats$SUP$playerid[stats$SUP$ï..Name==pitcher_name]))
    }else{
      if(length(stats$ZP$ï..Name[stats$ZP$ï..Name==pitcher_name])>1){
        pitcher_id <- as.numeric(as.character(stats$ZP$playerid[stats$ZP$ï..Name==pitcher_name]))
      }else{
        pitcher_id <- as.numeric(as.character(stats$SP$playerid[stats$SP$ï..Name==pitcher_name]))
      }
    }
  }
  throws <- substring(lineups_df$pitcherhand[lineups_df$team==team],1,1)
  StartingPitcher <- data.frame(pitcher_name,pitcher_id, throws)
  
  
  url <- paste0("https://www.fangraphs.com/statsplits.aspx?playerid=",StartingPitcher$pitcher_id,"&position=P&season=0")
  webpagers <- read_html(url)
  tbls <- webpagers %>%
    html_nodes(xpath='//table[@id="SeasonSplits1_dgSeason1_ctl00"]') %>% 
    html_table(fill =TRUE)
  splits <- tbls[[1]]
  splits[is.na(splits)] <- 0
  Sys.sleep(3.5)
  
  
  url2 <- paste0("https://www.fangraphs.com/statss.aspx?playerid=",StartingPitcher$pitcher_id,"&position=P")
  webpagers2 <- read_html(url2)
  tbls2 <- webpagers2 %>%
    html_nodes(xpath='//table[@id="SeasonStats1_dgSeason8_ctl00"]') %>% 
    html_table(fill =TRUE)
  fielding <- tbls2[[1]]
  fielding[is.na(fielding)] <- 0
  Sys.sleep(3.5)
  
  
  url3 <- paste0("https://www.fangraphs.com/statsplits.aspx?playerid=",StartingPitcher$pitcher_id,"&position=P&season=0")
  webpagers3 <- read_html(url3)
  tbls3<- webpagers3 %>%
    html_nodes(xpath='//table[@id="SeasonSplits1_dgSeason3_ctl00"]') %>% 
    html_table(fill =TRUE)
  pitches <- tbls3[[1]]
  pitches[is.na(pitches)] <- 0
  Sys.sleep(3.5)
  
  
  url4 <- paste0("https://www.fangraphs.com/statss.aspx?playerid=",StartingPitcher$pitcher_id,"&position=P")
  webpagers4 <- read_html(url4)
  tbls4<- webpagers4 %>%
    html_nodes(xpath='//table[@id="SeasonStats1_dgSeason1_ctl00"]') %>% 
    html_table(fill =TRUE)
  pitching <- tbls4[[1]]
  pitching[is.na(pitching)] <- 0
  
  
  pitches$Pitches <- as.numeric(pitches$Pitches)
  
  
  splits$TBF <- as.numeric(splits$TBF)
  splits$BB <- as.numeric(splits$BB)
  splits$SO <- as.numeric(splits$SO)
  splits$`2B` <- as.numeric(splits$`2B`)
  splits$`3B` <- as.numeric(splits$`3B`)
  splits$HR <- as.numeric(splits$HR)
  splits$H <- as.numeric(splits$H)
  splits$`1B` <- splits$H - splits$HR - splits$`3B` - splits$`2B`
  pitching$H <- as.numeric(pitching$H)
  pitching$SO <- as.numeric(pitching$SO)
  pitching$HR <- as.numeric(pitching$HR)
  pitching$WP <- as.numeric(pitching$WP)
  pitching$TBF <- as.numeric(pitching$TBF)
  pitching$BB <- as.numeric(pitching$BB)
  pitching$IBB <- as.numeric(pitching$IBB)
  pitching$HBP <- as.numeric(pitching$HBP)
  pitching$IP <- as.numeric(pitching$IP)
  
  
  one <- matrix(0, 15, 1)
  dimnames(one)[[1]] <- c("SB", "stay", "CS", "WP",
                          "pitch", "K", "contact","out","hit","BB","1B","2B","3B","HR","Inn/G")
  dimnames(one)[[2]] <- c(1)
  
  two <- matrix(0, 15, 1)
  dimnames(two)[[1]] <- c("SB", "stay", "CS", "WP",
                          "pitch", "K", "contact","out","hit","BB","1B","2B","3B","HR","Inn/G")
  dimnames(two)[[2]] <- c(1)
  
  one <- list(one,two)
  
  weight <- c(splits$TBF[splits$Handedness=="vs L"]/((splits$TBF[splits$Handedness=="vs L"])+(splits$TBF[splits$Handedness=="vs R"])),
              splits$TBF[splits$Handedness=="vs R"]/((splits$TBF[splits$Handedness=="vs L"])+(splits$TBF[splits$Handedness=="vs R"])))
  
  
  so_split <- c(splits$SO[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                   splits$SO[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  k_split <- 0
  if(weight==0 ||  (so_split[1] == 0 || so_split[2] == 0)  || length(weight)==0){
    k_split[1] <- 1
    k_split[2] <- 1
  }else{
    k_split[1] <- (so_split[1])/((weight[2]*so_split[2])+(weight[1]*so_split[1]))
    k_split[2] <- (so_split[2])/((weight[1]*so_split[1])+(weight[2]*so_split[2]))
  }
  
  if(k_split[1] >= 1.25 ||  k_split[2] >=1.25){
    if(k_split[1] >= 1.25){
      k_split[1] <- 1.25
      k_split[2] <- .75
    }else{
      k_split[2] <- 1.25
      k_split[1] <- .75
    }
  }else{
    k_split[1]<- k_split[1]
    k_split[2]<- k_split[2]
  }
  
  oneb_split <- c(splits$`1B`[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                splits$`1B`[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  single_split <- 0
  if(weight==0 ||  (oneb_split[1] == 0 || oneb_split[2] == 0)  || length(weight)==0){
    single_split[1] <- 1
    single_split[2] <- 1
  }else{
    single_split[1] <- (oneb_split[1])/((weight[2]*oneb_split[2])+(weight[1]*oneb_split[1]))
    single_split[2] <- (oneb_split[2])/((weight[1]*oneb_split[1])+(weight[2]*oneb_split[2]))
  }
  
  if(single_split[1] >= 1.25 ||  single_split[2] >=1.25){
    if(single_split[1] >= 1.25){
      single_split[1] <- 1.25
      single_split[2] <- .75
    }else{
      single_split[2] <- 1.25
      single_split[1] <- .75
    }
  }else{
    single_split[1]<- single_split[1]
    single_split[2]<- single_split[2]
  }
  
  twob_split <- c(splits$`2B`[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                  splits$`2B`[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  double_split <- 0
  if(weight==0 ||  (twob_split[1] == 0 || twob_split[2] == 0)  || length(weight)==0){
    single_split[1] <- 1
    single_split[2] <- 1
  }else{
    double_split[1] <- (twob_split[1])/((weight[2]*twob_split[2])+(weight[1]*twob_split[1]))
    double_split[2] <- (twob_split[2])/((weight[1]*twob_split[1])+(weight[2]*twob_split[2]))
  }
  
  if(double_split[1] >= 1.25 ||  double_split[2] >=1.25){
    if(double_split[1] >= 1.25){
      double_split[1] <- 1.25
      double_split[2] <- .75
    }else{
      double_split[2] <- 1.25
      double_split[1] <- .75
    }
  }else{
    double_split[1]<- double_split[1]
    double_split[2]<- double_split[2]
  }
  
  threeb_split <- c(splits$`3B`[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                  splits$`3B`[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  triple_split <- 0
  if(weight==0 ||  (threeb_split[1] == 0 || threeb_split[2] == 0)  || length(weight)==0){
    triple_split[1] <- 1
    triple_split[2] <- 1
  }else{
    triple_split[1] <- (threeb_split[1])/((weight[2]*threeb_split[2])+(weight[1]*threeb_split[1]))
    triple_split[2] <- (threeb_split[2])/((weight[1]*threeb_split[1])+(weight[2]*threeb_split[2]))
  }
  
  if(triple_split[1] >= 1.25 ||  triple_split[2] >=1.25){
    if(triple_split[1] >= 1.25){
      triple_split[1] <- 1.25
      triple_split[2] <- .75
    }else{
      triple_split[2] <- 1.25
      triple_split[1] <- .75
    }
  }else{
    triple_split[1]<- triple_split[1]
    triple_split[2]<- triple_split[2]
  }
  
  HR_split <- c(splits$`HR`[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                    splits$`HR`[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  homer_split <- 0
  if(weight==0 ||  (HR_split[1] == 0 || HR_split[2] == 0)  || length(weight)==0){
    homer_split[1] <- 1
    homer_split[2] <- 1
  }else{
    homer_split[1] <- (HR_split[1])/((weight[2]*HR_split[2])+(weight[1]*HR_split[1]))
    homer_split[2] <- (HR_split[2])/((weight[1]*HR_split[1])+(weight[2]*HR_split[2]))
  }
  
  if(homer_split[1] >= 1.25 ||  homer_split[2] >=1.25){
    if(homer_split[1] >= 1.25){
      homer_split[1] <- 1.25
      homer_split[2] <- .75
    }else{
      homer_split[2] <- 1.25
      homer_split[1] <- .75
    }
  }else{
    homer_split[1]<- homer_split[1]
    homer_split[2]<- homer_split[2]
  }
  
  
  BB_split <- c(splits$`BB`[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                splits$`BB`[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  walk_split <- 0
  if(weight==0 ||  (BB_split[1] == 0 || BB_split[2] == 0)  || length(weight)==0){
    walk_split[1] <- 1
    walk_split[2] <- 1
  }else{
    walk_split[1] <- (BB_split[1])/((weight[2]*BB_split[2])+(weight[1]*BB_split[1]))
    walk_split[2] <- (BB_split[2])/((weight[1]*BB_split[1])+(weight[2]*BB_split[2]))
  }
  
  if(walk_split[1] >= 1.25 ||  walk_split[2] >=1.25){
    if(walk_split[1] >= 1.25){
      walk_split[1] <- 1.25
      walk_split[2] <- .75
    }else{
      walk_split[2] <- 1.25
      walk_split[1] <- .75
    }
  }else{
    walk_split[1]<- walk_split[1]
    walk_split[2]<- walk_split[2]
  }
  
  H_split <- c(splits$H[splits$Handedness=="vs L"]/(splits$TBF[splits$Handedness=="vs L"]),
                splits$H[splits$Handedness=="vs R"]/(splits$TBF[splits$Handedness=="vs R"]))
  hit_split <- 0
  if(weight==0 ||  (H_split[1] == 0 || H_split[2] == 0)  || length(weight)==0){
    hit_split[1] <- 1
    hit_split[2] <- 1
  }else{
    hit_split[1] <- (H_split[1])/((weight[2]*H_split[2])+(weight[1]*H_split[1]))
    hit_split[2] <- (H_split[2])/((weight[1]*H_split[1])+(weight[2]*H_split[2]))
  }
  
  if(hit_split[1] >= 1.25 ||  hit_split[2] >=1.25){
    if(hit_split[1] >= 1.25){
      hit_split[1] <- 1.25
      hit_split[2] <- .75
    }else{
      hit_split[2] <- 1.25
      hit_split[1] <- .75
    }
  }else{
    hit_split[1]<- hit_split[1]
    hit_split[2]<- hit_split[2]
  }
  
  
  SB1 <- fielding$SB[fielding$Season=="Total"&fielding$Pos=="P"]
  CS1 <- fielding$CS[fielding$Season=="Total"&fielding$Pos=="P"]
  stay1 <- fielding$Inn[fielding$Season=="Total"&fielding$Pos=="P"]*1.44319
  WP1 <- pitching$WP[pitching$Team=="Steamer"]
  pitch1 <- ((pitches$Pitches[pitches$Handedness=="vs L"]+pitches$Pitches[pitches$Handedness=="vs R"])/
               pitching$IP[pitching$Season=="Total"])*ifelse(length(pitching$WP[pitching$Team=="ZiPS (R)"])==1,
                                                             pitching$IP[pitching$Team=="ZiPS (R)"],
                                                             pitching$IP[pitching$Team=="Steamer"])
  SB2 <- fielding$SB[fielding$Season=="Total"&fielding$Pos=="P"]
  CS2 <- fielding$CS[fielding$Season=="Total"&fielding$Pos=="P"]
  stay2 <- fielding$Inn[fielding$Season=="Total"&fielding$Pos=="P"]*1.44319
  WP2 <- pitching$WP[pitching$Team=="Steamer"]
  pitch2 <- ((pitches$Pitches[pitches$Handedness=="vs L"]+pitches$Pitches[pitches$Handedness=="vs R"])/
               pitching$IP[pitching$Season=="Total"])*ifelse(length(pitching$WP[pitching$Team=="ZiPS (R)"])==1,
                                                             pitching$IP[pitching$Team=="ZiPS (R)"],
                                                             pitching$IP[pitching$Team=="Steamer"])
  
  
  
  if(length(stats$ZUP[,1])>1){
    K1 <- stats$ZUP$SO[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[1]
    BB1 <- (stats$ZUP$B[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
              pitching$IBB[pitching$Team=="ZiPS (R)"] + 
              pitching$HBP[pitching$Team=="ZiPS (R)"]) * walk_split[1]
    HR1 <- stats$ZUP$HR[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[1]
    hit1 <- stats$ZUP$H[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
      ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
             1, 
             hit_split[1])+BB1
    contact1 <- ((pitching$TBF[pitching$Team=="Steamer (R)"]/pitching$IP[pitching$Team=="Steamer (R)"])*stats$ZUP$IP[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)])
 - K1
    out1 <- contact1 - hit1
    K2 <- stats$ZUP$SO[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[2]
    BB2 <- (stats$ZUP$BB[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
              pitching$IBB[pitching$Team=="ZiPS (R)"] + 
              pitching$HBP[pitching$Team=="ZiPS (R)"]) * walk_split[2]
    HR2 <- stats$ZUP$HR[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[2]
    hit2 <- stats$ZUP$H[as.character(stats$ZUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
      ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
             1, 
             hit_split[2])+BB2
    contact2 <-  ((pitching$TBF[pitching$Team=="Steamer (R)"]/pitching$IP[pitching$Team=="Steamer (R)"])*stats$ZUP$IP[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)])
 - K2
    out2 <- contact2 - hit2
  }else{
    if(length(stats$SUP[,1])>1){
      K1 <- stats$SUP$SO[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[1]
      BB1 <- (stats$SUP$BB[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                pitching$IBB[pitching$Team=="Steamer (R)"] + 
                pitching$HBP[pitching$Team=="Steamer (R)"]) * walk_split[1]
      HR1 <- stats$SUP$HR[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[1]
      hit1 <- stats$SUP$H[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
        ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
               1, 
               hit_split[1]) + BB1
      contact1 <- pitching$TBF[pitching$Team=="Steamer (R)"] - K1
      out1 <- contact1 - hit1
      K2 <- stats$SUP$SO[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[2]
      BB2 <- (stats$SUP$BB[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                pitching$IBB[pitching$Team=="Steamer (R)"] + 
                pitching$HBP[pitching$Team=="Steamer (R)"]) * walk_split[2]
      HR2 <- stats$SUP$HR[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[2]
      hit2 <- stats$SUP$H[as.character(stats$SUP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
        ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
               1, 
               hit_split[2])  + BB2  
      contact2 <-  pitching$TBF[pitching$Team=="Steamer (R)"] - K2 
      out2 <- contact2 - hit2
    }else{
      if(length(stats$ZP[,1])>1){
        K1 <- stats$ZP$SO[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[1]
        BB1 <- (stats$ZP$BB[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                  pitching$IBB[pitching$Team=="ZiPS"] + 
                  pitching$HBP[pitching$Team=="ZiPS"]) * walk_split[1]
        HR1 <- stats$ZP$HR[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[1]
        hit1 <- stats$ZP$H[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
          ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
                 1, 
                 hit_split[1])+ BB1
        contact1 <- ((pitching$TBF[pitching$Team=="Steamer"]/pitching$IP[pitching$Team=="Steamer"])*stats$ZP$IP[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)])
                        - K1 
        out1 <- contact1 - hit1
        K2 <- stats$ZP$SO[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[2]
        BB2 <- (stats$ZP$BB[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                  pitching$IBB[pitching$Team=="ZiPS"] + 
                  pitching$HBP[pitching$Team=="ZiPS"]) * walk_split[2]
        HR2 <- stats$ZP$HR[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[2]
        hit2 <- stats$ZP$H[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
          ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
                 1, 
                 hit_split[2])    + BB2
        contact2 <-  ((pitching$TBF[pitching$Team=="Steamer"]/pitching$IP[pitching$Team=="Steamer"])*stats$ZP$IP[as.character(stats$ZP$ï..Name)==as.character(StartingPitcher$pitcher_name)])
 - K2 
        out2 <- contact2 - hit2
      }else{
        K1 <- stats$SP$SO[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[1]
        BB1 <- (stats$SP$BB[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                  pitching$IBB[pitching$Team=="Steamer"] + 
                  pitching$HBP[pitching$Team=="Steamer"]) * walk_split[1]
        HR1 <- stats$SP$HR[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[1]
        hit1 <- stats$SP$H[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
          ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
                 1, 
                 hit_split[1])+ BB1
        contact1 <- pitching$TBF[pitching$Team=="Steamer"] - K1 
        out1 <- contact1 - hit1
        K2 <- stats$SP$SO[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*k_split[2]
        BB2 <- (stats$SP$BB[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)] + 
                  pitching$IBB[pitching$Team=="Steamer"] + 
                  pitching$HBP[pitching$Team=="Steamer"]) * walk_split[2]
        HR2 <- stats$SP$HR[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*homer_split[2]
        hit2 <- stats$SP$H[as.character(stats$SP$ï..Name)==as.character(StartingPitcher$pitcher_name)]*
          ifelse(single_split[1] == 1 || double_split[1] == 1 || single_split[2] == 1 || double_split[2] == 1, 
                 1, 
                 hit_split[2])  + BB2  
        contact2 <-  pitching$TBF[pitching$Team=="Steamer"] - K2
        out2 <- contact2 - hit2
      }
    }
  }
  
  
  
  if (HR1==0){
    twoB1 <- .20
    threeB1 <- .017
    oneB1 <- .62
    HR1 <- .167
  }else{
    twoB1 <- 1.65112+(1.22005*HR1)
    threeB1 <- (hit1 - BB1 - twoB1 - HR1)*.05
    oneB1 <- (hit1 - BB1 - twoB1 - HR1)*.95 
  }
  
  
   
  if (HR2==0){
    twoB2 <- .20
    threeB2 <- .017
    oneB2 <- .62
    HR2 <- .167
  }else{
    twoB2 <- 1.65112+(1.22005*HR2)
    threeB2 <- (hit2 - BB2 - twoB2 - HR2)*.05
    oneB2 <- (hit2 - BB2 - twoB2 - HR2)*.95 
  }
  
  
  
  
  
  
  
  one[[1]]["SB",] <- SB1/(CS1+SB1+stay1)
  one[[1]]["CS",] <- CS1/(CS1+SB1+stay1)
  one[[1]]["stay",] <- stay1/(CS1+SB1+stay1)
  one[[1]]["WP",] <- WP1/(pitch1+WP1)
  one[[1]]["pitch",] <- pitch1/(pitch1+WP1)
  one[[1]]["K",] <- K1/(K1+contact1)
  one[[1]]["2B",] <- twoB1/(twoB1+HR1+oneB1+BB1+threeB1)
  one[[1]]["3B",] <- threeB1/(twoB1+HR1+oneB1+BB1+threeB1)
  one[[1]]["HR",] <- HR1/(twoB1+HR1+oneB1+BB1+threeB1)
  one[[1]]["1B",] <- oneB1/(twoB1+HR1+oneB1+BB1+threeB1)
  one[[1]]["BB",] <- BB1/(twoB1+HR1+oneB1+BB1+threeB1)
  one[[1]]["contact",] <-  contact1/(K1+contact1)
  one[[1]]["hit",] <- hit1/(hit1+out1)
  one[[1]]["out",] <- out1/(hit1+out1)
  
  one[[2]]["SB",] <- SB2/(CS2+SB2+stay2)
  one[[2]]["CS",] <- CS2/(CS2+SB2+stay2)
  one[[2]]["stay",] <- stay2/(CS2+SB2+stay2)
  one[[2]]["WP",] <- WP2/(pitch2+WP2)
  one[[2]]["pitch",] <- pitch2/(pitch2+WP2)
  one[[2]]["K",] <- K2/(K2+contact2)
  one[[2]]["2B",] <- twoB2/(twoB2+HR2+oneB2+BB2+threeB2)
  one[[2]]["3B",] <- threeB2/(twoB2+HR2+oneB2+BB2+threeB2)
  one[[2]]["HR",] <- HR2/(twoB2+HR2+oneB2+BB2+threeB2)
  one[[2]]["1B",] <- oneB2/(twoB2+HR2+oneB2+BB2+threeB2)
  one[[2]]["BB",] <- BB2/(twoB2+HR2+oneB2+BB2+threeB2)
  one[[2]]["contact",] <-  contact2/(K2+contact2)
  one[[2]]["hit",] <- hit2/(hit2+out2)
  one[[2]]["out",] <- out2/(hit2+out2)
  
  two <- StartingPitcher$throws
  names(two)[[1]] <- "throws"
  
  
  one <- list(one=one,two=two)
}

######### Batting Order ############
team <- hometeam
lineups_df
OT_starter

BOfx <- function(team,lineups_df, Pitcher){
  
  battinglineups <- unlist(lineups_df[lineups_df$team==team,c(5:13)])
  batter_ids <- 0
  for(a in 1:length(battinglineups)){
    if(length(stats$ZUB$ï..Name[stats$ZUB$ï..Name==battinglineups[a]])>0){
      batter_ids[a] <- as.numeric(as.character(stats$ZUB$playerid[stats$ZUB$ï..Name==battinglineups[a]]))
    }else{
      if(length(stats$SUB$ï..Name[stats$SUB$ï..Name==battinglineups[a]])>0){
        batter_ids[a] <- as.numeric(as.character(stats$SUB$playerid[stats$SUB$ï..Name==battinglineups[a]]))
      }else{
        if(length(stats$ZB$ï..Name[stats$ZB$ï..Name==battinglineups[a]])>0){
          batter_ids[a] <- as.numeric(as.character(stats$ZB$playerid[stats$ZB$ï..Name==battinglineups[a]]))
        }else{
          batter_ids[a] <- as.numeric(as.character(stats$SB$playerid[stats$SB$ï..Name==battinglineups[a]]))
        }
      }
    }
  }
  
  
  pitcher_hand <- as.character(Pitcher$two)
  
  player_bats <- unlist(lineups_df[lineups_df$team==team,c(14:22)])
  
  BattingOrder <- data.frame(battinglineups,batter_ids,player_bats)
  
  
  
  
  
  for (i in 1:length(BattingOrder)){
    
    ifelse(pitcher_hand=="R"&&BattingOrder$player_bats[i]=="S", 
           BattingOrder$player_bats[i] <- "L", 
           BattingOrder$player_bats[i] <- BattingOrder$player_bats[i])
    
    ifelse(pitcher_hand=="L"&&BattingOrder$player_bats[i]=="S", 
           BattingOrder$player_bats[i] <- "R", 
           BattingOrder$player_bats[i] <- BattingOrder$player_bats[i])
    
  }
  
  
  one <- matrix(0, 15, 9)
  dimnames(one)[[1]] <- c("SB", "stay", "CS",
                          "K", "contact","GIDP",
                          "SFSH","outorhit","out",
                          "hit","BB","1B",
                          "2B","3B","HR")
  dimnames(one)[[2]] <- c(1,2,3,4,5,6,7,8,9)
  
  two <- matrix(0, 15, 9)
  dimnames(two)[[1]] <- c("SB", "stay", "CS",
                          "K", "contact","GIDP",
                          "SFSH","outorhit","out",
                          "hit","BB","1B",
                          "2B","3B","HR")
  dimnames(two)[[2]] <- c(1,2,3,4,5,6,7,8,9)
  
  one <- list(one,two)
  
  
  
  for (i in 1:9){
    urlp1 <- paste0("https://www.fangraphs.com/statss.aspx?playerid=",BattingOrder$batter_ids[i],"&position=PB")
    webpagersp1 <- read_html(urlp1)
    tblsp1 <- webpagersp1 %>%
      html_nodes(xpath='//table[@id="SeasonStats1_dgSeason1_ctl00"]') %>% 
      html_table(fill =TRUE)
    StndrdBtbl1 <- tblsp1[[1]]
    StndrdBtbl1[is.na(StndrdBtbl1)] <- 0
    Sys.sleep(3.5)
    
    
    urlp1_split <- paste0("https://www.fangraphs.com/statsplits.aspx?playerid=",BattingOrder$batter_ids[i],"&position=PB&season=0")
    r <- GET(urlp1_split)
    status <- status_code(r)
    
    if (status != 500){
      webpagersp1_split <- read_html(urlp1_split)
      tblsp1_split <- webpagersp1_split %>%
        html_nodes(xpath='//table[@id="SeasonSplits1_dgSeason1_ctl00"]') %>% 
        html_table(fill =TRUE)
      StndrdBtbl1_split <- tblsp1_split[[1]]
      StndrdBtbl1_split[is.na(StndrdBtbl1_split)] <- 0
      StndrdBtbl1_split$SO <- as.numeric(StndrdBtbl1_split$SO)
      StndrdBtbl1_split$`1B` <- as.numeric(StndrdBtbl1_split$`1B`)
      StndrdBtbl1_split$`2B` <- as.numeric(StndrdBtbl1_split$`2B`)
      StndrdBtbl1_split$`3B` <- as.numeric(StndrdBtbl1_split$`3B`)
      StndrdBtbl1_split$H <- as.numeric(StndrdBtbl1_split$H)
      StndrdBtbl1_split$BB <- as.numeric(StndrdBtbl1_split$BB)
      StndrdBtbl1_split$HR <- as.numeric(StndrdBtbl1_split$HR)
      StndrdBtbl1_split$PA <- as.numeric(StndrdBtbl1_split$PA)
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]) || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        SO_split1 <- c(StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        SO_split_1 <- c((StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs R"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))))
      }else{
        SO_split1 <- c(StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        SO_split_1 <- c((StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$SO[StndrdBtbl1_split$Handedness=="vs L"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]))/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]))))  
      }
      
      if(SO_split_1==0 ||  (SO_split1[1] == 0 || SO_split1[2] == 0)  || length(SO_split_1 )==0){
        SO_split1[1] <- 1
        SO_split1[2] <- 1
      }else{
        SO_split1[1]<- SO_split1[1]/SO_split_1
        SO_split1[2]<- SO_split1[2]/SO_split_1
      }
      
      
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])|| length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        oneB_split1 <- c(StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        oneB_split_1 <- c((StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs R"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))))
      }else{
        oneB_split1 <- c(StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        oneB_split_1 <- c((StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$`1B`[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]))*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))))
      }
      if(oneB_split_1==0 ||  (oneB_split1[1] == 0 || oneB_split1[2] == 0)  || length(oneB_split_1 )==0){
        oneB_split1[1] <- 1
        oneB_split1[2] <- 1
      }else{
        oneB_split1[1]<- oneB_split1[1]/oneB_split_1
        oneB_split1[2]<- oneB_split1[2]/oneB_split_1
      }
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])|| length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        twoB_split1 <- c(StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        twoB_split_1 <- c((StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))))
      }else{
        twoB_split1 <- c(StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        twoB_split_1 <- c((StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$`2B`[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))))  
      }
      if(twoB_split_1==0 ||  (twoB_split1[1] == 0 || twoB_split1[2] == 0)  || length(twoB_split_1 )==0){
        twoB_split1[1] <- 1
        twoB_split1[2] <- 1
      }else{
        twoB_split1[1]<- twoB_split1[1]/twoB_split_1
        twoB_split1[2]<- twoB_split1[2]/twoB_split_1
      }
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])|| length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        threeB_split1 <- c(StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        threeB_split_1 <- c((StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))))
      }else{
        threeB_split1 <- c(StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        threeB_split_1 <- c((StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$`3B`[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))))  
      }
      if(threeB_split_1==0 ||  (threeB_split1[1] == 0 || threeB_split1[2] == 0) || length(threeB_split_1 )==0){
        threeB_split1[1] <- 1
        threeB_split1[2] <- 1
      }else{
        threeB_split1[1]<- threeB_split1[1]/threeB_split_1
        threeB_split1[2]<- threeB_split1[2]/threeB_split_1
      }
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])|| length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        HR_split1 <- c(StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        HR_split_1 <- c((StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))))
      }else{
        HR_split1 <- c(StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        HR_split_1 <- c((StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$HR[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))))  
      }
      if(HR_split_1==0 ||  (HR_split1[1] == 0 || HR_split1[2] == 0) || length(HR_split_1 )==0){
        HR_split1[1] <- 1
        HR_split1[2] <- 1
      }else{
        HR_split1[1]<- HR_split1[1]/HR_split_1
        HR_split1[2]<- HR_split1[2]/HR_split_1
      }
      
      
      if((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])>=(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])|| length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs L"])==0 || length(StndrdBtbl1_split[StndrdBtbl1_split$Handedness=="vs R"])==0){
        BB_split1 <- c(StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        BB_split_1 <- c((StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])))))
      }else{
        BB_split1 <- c(StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs L"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]),StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs R"]/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]))
        BB_split_1 <- c((StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$BB[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))/((StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"]+(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"]*(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs R"])/(StndrdBtbl1_split$PA[StndrdBtbl1_split$Handedness=="vs L"])))))  
      }
      if(BB_split_1==0 ||   (BB_split1[1] == 0 || BB_split1[2] == 0)  || length(BB_split_1 )==0){
        BB_split1[1] <- 1
        BB_split1[2] <- 1
      }else{
        BB_split1[1]<- BB_split1[1]/BB_split_1
        BB_split1[2]<- BB_split1[2]/BB_split_1
      }
      
      
      if(SO_split1[1] >= 1.25 ||  SO_split1[2] >=1.25){
        if(SO_split1[1] >= 1.25){
          SO_split1[1] <- 1.25
          SO_split1[2] <- .75
        }else{
          SO_split1[2] <- 1.25
          SO_split1[1] <- .75
        }
      }else{
        SO_split1[1]<- SO_split1[1]
        SO_split1[2]<- SO_split1[2]
      }
      
      
      if(BB_split1[1] >= 1.25 ||  BB_split1[2] >=1.25){
        if(BB_split1[1] >= 1.25){
          BB_split1[1] <- 1.25
          BB_split1[2] <- .75
        }else{
          BB_split1[2] <- 1.25
          BB_split1[1] <- .75
        }
      }else{
        BB_split1[1]<- BB_split1[1]
        BB_split1[2]<- BB_split1[2]
      }
      
      
      if(HR_split1[1] >= 1.25 ||  HR_split1[2] >=1.25){
        if(HR_split1[1] >= 1.25){
          HR_split1[1] <- 1.25
          HR_split1[2] <- .75
        }else{
          HR_split1[2] <- 1.25
          HR_split1[1] <- .75
        }
      }else{
        HR_split1[1]<- HR_split1[1]
        HR_split1[2]<- HR_split1[2]
      }
      
      if(threeB_split1[1] >= 1.25 ||  threeB_split1[2] >=1.25){
        if(threeB_split1[1] >= 1.25){
          threeB_split1[1] <- 1.25
          threeB_split1[2] <- .75
        }else{
          threeB_split1[2] <- 1.25
          threeB_split1[1] <- .75
        }
      }else{
        threeB_split1[1]<- threeB_split1[1]
        threeB_split1[2]<- threeB_split1[2]
      }
      
      
      if(twoB_split1[1] >= 1.25 ||  twoB_split1[2] >=1.25){
        if(twoB_split1[1] >= 1.25){
          twoB_split1[1] <- 1.25
          twoB_split1[2] <- .75
        }else{
          twoB_split1[2] <- 1.25
          twoB_split1[1] <- .75
        }
      }else{
        twoB_split1[1]<- twoB_split1[1]
        twoB_split1[2]<- twoB_split1[2]
      }
      
      
      if(oneB_split1[1] >= 1.25 ||  oneB_split1[2] >=1.25){
        if(oneB_split1[1] >= 1.25){
          oneB_split1[1] <- 1.25
          oneB_split1[2] <- .75
        }else{
          oneB_split1[2] <- 1.25
          oneB_split1[1] <- .75
        }
      }else{
        oneB_split1[1]<- oneB_split1[1]
        oneB_split1[2]<- oneB_split1[2]
      }
    }else{
      oneB_split1[1]<- 1
      oneB_split1[2]<- 1
      twoB_split1[1]<- 1
      twoB_split1[2]<- 1
      threeB_split1[1]<- 1
      threeB_split1[2]<- 1
      HR_split1[1]<- 1
      HR_split1[2]<- 1
      BB_split1[1]<- 1
      BB_split1[2]<- 1
      SO_split1[1]<- 1
      SO_split1[2]<- 1
    }
    
    
    
    StndrdBtbl1$PA <- as.numeric(StndrdBtbl1$PA)
    StndrdBtbl1$AB <- as.numeric(StndrdBtbl1$AB)
    StndrdBtbl1$SO <- as.numeric(StndrdBtbl1$SO)
    StndrdBtbl1$`1B` <- as.numeric(StndrdBtbl1$`1B`)
    StndrdBtbl1$`2B` <- as.numeric(StndrdBtbl1$`2B`)
    StndrdBtbl1$`3B` <- as.numeric(StndrdBtbl1$`3B`)
    StndrdBtbl1$H <- as.numeric(StndrdBtbl1$H)
    StndrdBtbl1$BB <- as.numeric(StndrdBtbl1$BB)
    StndrdBtbl1$IBB <- as.numeric(StndrdBtbl1$IBB)
    StndrdBtbl1$HBP <- as.numeric(StndrdBtbl1$HBP)
    StndrdBtbl1$HR <- as.numeric(StndrdBtbl1$HR)
    StndrdBtbl1$SF <- as.numeric(StndrdBtbl1$SF)
    StndrdBtbl1$SH <- as.numeric(StndrdBtbl1$SH)
    StndrdBtbl1$GDP <- as.numeric(StndrdBtbl1$GDP)
    StndrdBtbl1$SB <- as.numeric(StndrdBtbl1$SB)
    StndrdBtbl1$CS <- as.numeric(StndrdBtbl1$CS)
    
    stats$ZUB$Name <- as.character(stats$ZUB$Name)
    stats$ZB$ï..Name <- as.character(stats$ZB$ï..Name)
    stats$SUB$Name <- as.character(stats$SUB$Name)
    stats$SB$ï..Name <- as.character(stats$SB$ï..Name)
    
   
      if(length(stats$ZUB$ï..Name[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])>0){
        if(pitcher_hand=="R"){
          SB1 <- as.numeric(as.character(stats$ZUB$SB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          CS1 <- as.numeric(as.character(stats$ZUB$CS[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          stay1 <- (as.numeric(as.character(stats$ZUB$AB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*
                      as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
          
          K1 <- as.numeric(as.character(stats$ZUB$SO[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
          
          contact1 <-  as.numeric(as.character(stats$ZUB$PA[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])) - K1
          
          GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS (R)"]
          
          outorhit1 <- contact1 - GIDP1 - SFSH1
          
          twoB1 <- as.numeric(as.character(stats$ZUB$X2B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
          
          threeB1 <- as.numeric(as.character(stats$ZUB$X3B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
          
          HR1 <- as.numeric(as.character(stats$ZUB$HR[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
          
          hit1 <- as.numeric(as.character(stats$ZUB$H[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[2]
          
          BB1 <- as.numeric(as.character(stats$ZUB$BB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
          +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS (R)"]
          +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          hit1 <- oneB1+HR1+twoB1+threeB1+BB1
          
          out1 <- outorhit1 - hit1
          
          SB2 <- as.numeric(as.character(stats$ZUB$SB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          CS2 <- as.numeric(as.character(stats$ZUB$CS[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          stay2 <- (as.numeric(as.character(stats$ZUB$AB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*
                      as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
          
          K2 <- as.numeric(as.character(stats$ZUB$SO[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
          
          contact2 <-  as.numeric(as.character(stats$ZUB$PA[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])) - K2
          
          GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS (R)"]
          
          outorhit2 <- contact2 - GIDP2 - SFSH2
          
          twoB2 <- as.numeric(as.character(stats$ZUB$X2B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
          
          threeB2 <- as.numeric(as.character(stats$ZUB$X3B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
          
          HR2 <- as.numeric(as.character(stats$ZUB$HR[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
          
          hit2 <- as.numeric(as.character(stats$ZUB$H[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[1]
          
          BB2 <- as.numeric(as.character(stats$ZUB$BB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
          +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS (R)"]
          +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          hit2 <- oneB2+HR2+twoB2+threeB2+BB2
          
          out2 <- outorhit2 - hit2
        }else{
          SB1 <- as.numeric(as.character(stats$ZUB$SB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          CS1 <- as.numeric(as.character(stats$ZUB$CS[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          stay1 <- (as.numeric(as.character(stats$ZUB$AB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*
                      as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
          
          K1 <- as.numeric(as.character(stats$ZUB$SO[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
          
          contact1 <-  as.numeric(as.character(stats$ZUB$PA[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])) - K1
          
          GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS (R)"]
          
          outorhit1 <- contact1 - GIDP1 - SFSH1
          
          twoB1 <- as.numeric(as.character(stats$ZUB$X2B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
          
          threeB1 <- as.numeric(as.character(stats$ZUB$X3B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
          
          HR1 <- as.numeric(as.character(stats$ZUB$HR[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
          
          hit1 <- as.numeric(as.character(stats$ZUB$H[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[1]
          
          BB1 <- as.numeric(as.character(stats$ZUB$BB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
          +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS (R)"]
          +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          hit1 <- oneB1+HR1+twoB1+threeB1+BB1
          
          out1 <- outorhit1 - hit1
          
          SB2 <- as.numeric(as.character(stats$ZUB$SB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          CS2 <- as.numeric(as.character(stats$ZUB$CS[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          stay2 <- (as.numeric(as.character(stats$ZUB$AB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*
                      as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
          
          K2 <- as.numeric(as.character(stats$ZUB$SO[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
          
          contact2 <-  as.numeric(as.character(stats$ZUB$PA[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]])) - K2
          
          GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS (R)"]
          
          outorhit2 <- contact2 - GIDP2 - SFSH2
          
          twoB2 <- as.numeric(as.character(stats$ZUB$X2B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
          
          threeB2 <- as.numeric(as.character(stats$ZUB$X3B[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
          
          HR2 <- as.numeric(as.character(stats$ZUB$HR[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
          
          hit2 <- as.numeric(as.character(stats$ZUB$H[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))
          
          oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[2]
          
          BB2 <- as.numeric(as.character(stats$ZUB$BB[stats$ZUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
          +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS (R)"]
          +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS (R)"]
          
          hit2 <- oneB2+HR2+twoB2+threeB2+BB2
          
          out2 <- outorhit2 - hit2
        }
      }else{
        if(length(stats$SUB$ï..Name[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])>0){
          if(pitcher_hand=="R"){
            SB1 <- as.numeric(as.character(stats$SUB$SB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            CS1 <- as.numeric(as.character(stats$SUB$CS[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            stay1 <- (as.numeric(as.character(stats$SUB$AB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*
                        as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
            
            K1 <- as.numeric(as.character(stats$SUB$SO[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
            
            contact1 <-  as.numeric(as.character(stats$SUB$PA[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])) - K1
            
            GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer (R)"]
            
            SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer (R)"]
            
            outorhit1 <- contact1 - GIDP1 - SFSH1
            
            twoB1 <- as.numeric(as.character(stats$SUB$X2B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
            
            threeB1 <- as.numeric(as.character(stats$SUB$X3B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
            
            HR1 <- as.numeric(as.character(stats$SUB$HR[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
            
            hit1 <- as.numeric(as.character(stats$SUB$H[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[2]
            
            BB1 <- as.numeric(as.character(stats$SUB$BB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
            +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer (R)"]
            +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer (R)"]
            
            hit1 <- oneB1+HR1+twoB1+threeB1+BB1
            
            out1 <- outorhit1 - hit1
            
            SB2 <- as.numeric(as.character(stats$SUB$SB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            CS2 <- as.numeric(as.character(stats$SUB$CS[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            stay2 <- (as.numeric(as.character(stats$SUB$AB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*
                        as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
            
            K2 <- as.numeric(as.character(stats$SUB$SO[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
            
            contact2 <-  as.numeric(as.character(stats$SUB$PA[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])) - K2
            
            GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer (R)"]
            
            SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer (R)"]
            
            outorhit2 <- contact2 - GIDP2 - SFSH2
            
            twoB2 <- as.numeric(as.character(stats$SUB$X2B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
            
            threeB2 <- as.numeric(as.character(stats$SUB$X3B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
            
            HR2 <- as.numeric(as.character(stats$SUB$HR[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
            
            hit2 <- as.numeric(as.character(stats$SUB$H[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[1]
            
            BB2 <- as.numeric(as.character(stats$SUB$BB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
            +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer (R)"]
            +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer (R)"]
            
            hit2 <- oneB2+HR2+twoB2+threeB2+BB2
            
            out2 <- outorhit2 - hit2
          }else{
            SB1 <- as.numeric(as.character(stats$SUB$SB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            CS1 <- as.numeric(as.character(stats$SUB$CS[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            stay1 <- (as.numeric(as.character(stats$SUB$AB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*
                        as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
            
            K1 <- as.numeric(as.character(stats$SUB$SO[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
            
            contact1 <-  as.numeric(as.character(stats$SUB$PA[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])) - K1
            
            GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer (R)"]
            
            SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer (R)"]
            
            outorhit1 <- contact1 - GIDP1 - SFSH1
            
            twoB1 <- as.numeric(as.character(stats$SUB$X2B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
            
            threeB1 <- as.numeric(as.character(stats$SUB$X3B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
            
            HR1 <- as.numeric(as.character(stats$SUB$HR[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
            
            hit1 <- as.numeric(as.character(stats$SUB$H[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[1]
            
            BB1 <- as.numeric(as.character(stats$SUB$BB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
            +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer (R)"]
            +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer (R)"]
            
            hit1 <- oneB1+HR1+twoB1+threeB1+BB1
            
            out1 <- outorhit1 - hit1
            
            SB2 <- as.numeric(as.character(stats$SUB$SB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            CS2 <- as.numeric(as.character(stats$SUB$CS[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            stay2 <- (as.numeric(as.character(stats$SUB$AB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*
                        as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
            
            K2 <- as.numeric(as.character(stats$SUB$SO[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
            
            contact2 <-  as.numeric(as.character(stats$SUB$PA[stats$SUB$ï..Name==BattingOrder$battinglineups[i]])) - K2
            
            GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer (R)"]
            
            SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer (R)"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer (R)"]
            
            outorhit2 <- contact2 - GIDP2 - SFSH2
            
            twoB2 <- as.numeric(as.character(stats$SUB$X2B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
            
            threeB2 <- as.numeric(as.character(stats$SUB$X3B[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
            
            HR2 <- as.numeric(as.character(stats$SUB$HR[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
            
            hit2 <- as.numeric(as.character(stats$SUB$H[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))
            
            oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[2]
            
            BB2 <- as.numeric(as.character(stats$SUB$BB[stats$SUB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
            +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer (R)"]
            +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer (R)"]
            
            hit2 <- oneB2+HR2+twoB2+threeB2+BB2
            
            out2 <- outorhit2 - hit2
          }
        }else{
          if(length(stats$ZB$ï..Name[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])>0){
            if(pitcher_hand=="R"){
              SB1 <- as.numeric(as.character(stats$ZB$SB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS1 <- as.numeric(as.character(stats$ZB$CS[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay1 <- (as.numeric(as.character(stats$ZB$AB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
              
              K1 <- as.numeric(as.character(stats$ZB$SO[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
              
              contact1 <-  as.numeric(as.character(stats$ZB$PA[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])) - K1
              
              GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS"]
              
              SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS"]
              
              outorhit1 <- contact1 - GIDP1 - SFSH1
              
              twoB1 <- as.numeric(as.character(stats$ZB$X2B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
              
              threeB1 <- as.numeric(as.character(stats$ZB$X3B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
              
              HR1 <- as.numeric(as.character(stats$ZB$HR[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
              
              hit1 <- as.numeric(as.character(stats$ZB$H[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[2]
              
              BB1 <- as.numeric(as.character(stats$ZB$BB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS"]
              
              hit1 <- oneB1+HR1+twoB1+threeB1+BB1
              
              out1 <- outorhit1 - hit1
              
              SB2 <- as.numeric(as.character(stats$ZB$SB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS2 <- as.numeric(as.character(stats$ZB$CS[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay2 <- (as.numeric(as.character(stats$ZB$AB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
              
              K2 <- as.numeric(as.character(stats$ZB$SO[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
              
              contact2 <-  as.numeric(as.character(stats$ZB$PA[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])) - K2
              
              GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS"]
              
              SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS"]
              
              outorhit2 <- contact2 - GIDP2 - SFSH2
              
              twoB2 <- as.numeric(as.character(stats$ZB$X2B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
              
              threeB2 <- as.numeric(as.character(stats$ZB$X3B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
              
              HR2 <- as.numeric(as.character(stats$ZB$HR[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
              
              hit2 <- as.numeric(as.character(stats$ZB$H[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[1]
              
              BB2 <- as.numeric(as.character(stats$ZB$BB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS"]
              
              hit2 <- oneB2+HR2+twoB2+threeB2+BB2
              
              out2 <- outorhit2 - hit2
            }else{
              SB1 <- as.numeric(as.character(stats$ZB$SB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS1 <- as.numeric(as.character(stats$ZB$CS[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay1 <- (as.numeric(as.character(stats$ZB$AB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
              
              K1 <- as.numeric(as.character(stats$ZB$SO[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
              
              contact1 <-  as.numeric(as.character(stats$ZB$PA[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])) - K1
              
              GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS"]
              
              SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS"]
              
              outorhit1 <- contact1 - GIDP1 - SFSH1
              
              twoB1 <- as.numeric(as.character(stats$ZB$X2B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
              
              threeB1 <- as.numeric(as.character(stats$ZB$X3B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
              
              HR1 <- as.numeric(as.character(stats$ZB$HR[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
              
              hit1 <- as.numeric(as.character(stats$ZB$H[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[1]
              
              BB1 <- as.numeric(as.character(stats$ZB$BB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS"]
              
              hit1 <- oneB1+HR1+twoB1+threeB1+BB1
              
              out1 <- outorhit1 - hit1
              
              SB2 <- as.numeric(as.character(stats$ZB$SB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS2 <- as.numeric(as.character(stats$ZB$CS[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay2 <- (as.numeric(as.character(stats$ZB$AB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
              
              K2 <- as.numeric(as.character(stats$ZB$SO[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
              
              contact2 <-  as.numeric(as.character(stats$ZB$PA[stats$ZB$ï..Name==BattingOrder$battinglineups[i]])) - K2
              
              GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="ZiPS"]
              
              SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="ZiPS"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="ZiPS"]
              
              outorhit2 <- contact2 - GIDP2 - SFSH2
              
              twoB2 <- as.numeric(as.character(stats$ZB$X2B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
              
              threeB2 <- as.numeric(as.character(stats$ZB$X3B[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
              
              HR2 <- as.numeric(as.character(stats$ZB$HR[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
              
              hit2 <- as.numeric(as.character(stats$ZB$H[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[2]
              
              BB2 <- as.numeric(as.character(stats$ZB$BB[stats$ZB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="ZiPS"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="ZiPS"]
              
              hit2 <- oneB2+HR2+twoB2+threeB2+BB2
              
              out2 <- outorhit2 - hit2
            }
          }else{
            if(pitcher_hand=="R"){
              SB1 <- as.numeric(as.character(stats$SB$SB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS1 <- as.numeric(as.character(stats$SB$CS[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay1 <- (as.numeric(as.character(stats$SB$AB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
              
              K1 <- as.numeric(as.character(stats$SB$SO[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
              
              contact1 <-  as.numeric(as.character(stats$SB$PA[stats$SB$ï..Name==BattingOrder$battinglineups[i]])) - K1
              
              GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer"]
              
              SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer"]
              
              outorhit1 <- contact1 - GIDP1 - SFSH1
              
              twoB1 <- as.numeric(as.character(stats$SB$X2B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
              
              threeB1 <- as.numeric(as.character(stats$SB$X3B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
              
              HR1 <- as.numeric(as.character(stats$SB$HR[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
              
              hit1 <- as.numeric(as.character(stats$SB$H[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[2]
              
              BB1 <- as.numeric(as.character(stats$SB$BB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer"]
              
              hit1 <- oneB1+HR1+twoB1+threeB1+BB1
              
              out1 <- outorhit1 - hit1
              
              SB2 <- as.numeric(as.character(stats$SB$SB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS2 <- as.numeric(as.character(stats$SB$CS[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay2 <- (as.numeric(as.character(stats$SB$AB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
              
              K2 <- as.numeric(as.character(stats$SB$SO[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
              
              contact2 <-  as.numeric(as.character(stats$SB$PA[stats$SB$ï..Name==BattingOrder$battinglineups[i]])) - K2
              
              GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer"]
              
              SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer"]
              
              outorhit2 <- contact2 - GIDP2 - SFSH2
              
              twoB2 <- as.numeric(as.character(stats$SB$X2B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
              
              threeB2 <- as.numeric(as.character(stats$SB$X3B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
              
              HR2 <- as.numeric(as.character(stats$SB$HR[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
              
              hit2 <- as.numeric(as.character(stats$SB$H[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[1]
              
              BB2 <- as.numeric(as.character(stats$SB$BB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer"]
              
              hit2 <- oneB2+HR2+twoB2+threeB2+BB2
              
              out2 <- outorhit2 - hit2
            }else{
              SB1 <- as.numeric(as.character(stats$SB$SB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS1 <- as.numeric(as.character(stats$SB$CS[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay1 <- (as.numeric(as.character(stats$SB$AB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==BattingOrder$battinglineups[i]])))-CS1-SB1
              
              K1 <- as.numeric(as.character(stats$SB$SO[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[1]
              
              contact1 <-  as.numeric(as.character(stats$SB$PA[stats$SB$ï..Name==BattingOrder$battinglineups[i]])) - K1
              
              GIDP1 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer"]
              
              SFSH1 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer"]
              
              outorhit1 <- contact1 - GIDP1 - SFSH1
              
              twoB1 <- as.numeric(as.character(stats$SB$X2B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[1]
              
              threeB1 <- as.numeric(as.character(stats$SB$X3B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[1]
              
              HR1 <- as.numeric(as.character(stats$SB$HR[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[1]
              
              hit1 <- as.numeric(as.character(stats$SB$H[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB1 <- (hit1-HR1-threeB1-twoB1)*oneB_split1[1]
              
              BB1 <- as.numeric(as.character(stats$SB$BB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[1]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer"]
              
              hit1 <- oneB1+HR1+twoB1+threeB1+BB1
              
              out1 <- outorhit1 - hit1
              
              SB2 <- as.numeric(as.character(stats$SB$SB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              CS2 <- as.numeric(as.character(stats$SB$CS[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              stay2 <- (as.numeric(as.character(stats$SB$AB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*
                          as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==BattingOrder$battinglineups[i]])))-CS2-SB2
              
              K2 <- as.numeric(as.character(stats$SB$SO[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*SO_split1[2]
              
              contact2 <-  as.numeric(as.character(stats$SB$PA[stats$SB$ï..Name==BattingOrder$battinglineups[i]])) - K2
              
              GIDP2 <- StndrdBtbl1$GDP[StndrdBtbl1$Team=="Steamer"]
              
              SFSH2 <- StndrdBtbl1$SF[StndrdBtbl1$Team=="Steamer"] + StndrdBtbl1$SH[StndrdBtbl1$Team=="Steamer"]
              
              outorhit2 <- contact2 - GIDP2 - SFSH2
              
              twoB2 <- as.numeric(as.character(stats$SB$X2B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*twoB_split1[2]
              
              threeB2 <- as.numeric(as.character(stats$SB$X3B[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*threeB_split1[2]
              
              HR2 <- as.numeric(as.character(stats$SB$HR[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*HR_split1[2]
              
              hit2 <- as.numeric(as.character(stats$SB$H[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))
              
              oneB2 <- (hit2-HR2-threeB2-twoB2)*oneB_split1[2]
              
              BB2 <- as.numeric(as.character(stats$SB$BB[stats$SB$ï..Name==BattingOrder$battinglineups[i]]))*BB_split1[2]
              +StndrdBtbl1$IBB[StndrdBtbl1$Team=="Steamer"]
              +StndrdBtbl1$HBP[StndrdBtbl1$Team=="Steamer"]
              
              hit2 <- oneB2+HR2+twoB2+threeB2+BB2
              
              out2 <- outorhit2 - hit2
            }
          }
        }
      }
    
    
    
    
    
    one[[1]]["SB",i] <- SB1/(CS1+SB1+stay1)
    one[[1]]["CS",i] <- CS1/(CS1+SB1+stay1)
    one[[1]]["stay",i] <- stay1/(CS1+SB1+stay1)
    one[[1]]["GIDP",i] <- GIDP1/(GIDP1+SFSH1+outorhit1)
    one[[1]]["SFSH",i] <- SFSH1/(GIDP1+SFSH1+outorhit1)
    one[[1]]["outorhit",i] <- outorhit1/(GIDP1+SFSH1+outorhit1)
    one[[1]]["K",i] <- K1/(K1+contact1)
    one[[1]]["2B",i] <- twoB1/(twoB1+HR1+oneB1+BB1+threeB1)
    one[[1]]["3B",i] <- threeB1/(twoB1+HR1+oneB1+BB1+threeB1)
    one[[1]]["HR",i] <- HR1/(twoB1+HR1+oneB1+BB1+threeB1)
    one[[1]]["1B",i] <- oneB1/(twoB1+HR1+oneB1+BB1+threeB1)
    one[[1]]["BB",i] <- BB1/(twoB1+HR1+oneB1+BB1+threeB1)
    one[[1]]["contact",i] <-  contact1/(K1+contact1)
    one[[1]]["hit",i] <- hit1/(hit1+out1)
    one[[1]]["out",i] <- out1/(hit1+out1)
    
    one[[2]]["SB",i] <- ifelse(is.na(SB2/(CS2+SB2+stay2)),0,SB2/(CS2+SB2+stay2))
    one[[2]]["CS",i]  <- ifelse(is.na(CS2/(CS2+SB2+stay2)),0,CS2/(CS2+SB2+stay2))
    one[[2]]["stay",i]  <- ifelse(is.na(stay2/(CS2+SB2+stay2)),0,stay2/(CS2+SB2+stay2))
    one[[2]]["GIDP",i]  <- ifelse(is.na(GIDP2/(GIDP2+SFSH2+outorhit2)),0,GIDP2/(GIDP2+SFSH2+outorhit2))
    one[[2]]["SFSH",i]  <- ifelse(is.na(SFSH2/(GIDP2+SFSH2+outorhit2)),0,SFSH2/(GIDP2+SFSH2+outorhit2))
    one[[2]]["outorhit",i]  <- ifelse(is.na(outorhit2/(GIDP2+SFSH2+outorhit2)),0,outorhit2/(GIDP2+SFSH2+outorhit2))
    one[[2]]["K",i]  <- ifelse(is.na(K2/(K2+contact2)),0,K2/(K2+contact2))
    one[[2]]["2B",i]  <- ifelse(is.na(twoB2/(twoB2+HR2+oneB2+BB2+threeB2)),0,twoB2/(twoB2+HR2+oneB2+BB2+threeB2))
    one[[2]]["3B",i]  <- ifelse(is.na(threeB2/(twoB2+HR2+oneB2+BB2+threeB2)),0,threeB2/(twoB2+HR2+oneB2+BB2+threeB2))
    one[[2]]["HR",i]  <- ifelse(is.na(HR2/(twoB2+HR2+oneB2+BB2+threeB2)),0,HR2/(twoB2+HR2+oneB2+BB2+threeB2))
    one[[2]]["1B",i]  <- ifelse(is.na(oneB2/(twoB2+HR2+oneB2+BB2+threeB2)),0,oneB2/(twoB2+HR2+oneB2+BB2+threeB2))
    one[[2]]["BB",i]  <- ifelse(is.na(BB2/(twoB2+HR2+oneB2+BB2+threeB2)),0,BB2/(twoB2+HR2+oneB2+BB2+threeB2))
    one[[2]]["contact",i]  <-  ifelse(is.na(contact2/(K2+contact2)),0,contact2/(K2+contact2))
    one[[2]]["hit",i]  <- ifelse(is.na(hit2/(hit2+out2)),0,hit2/(hit2+out2))
    one[[2]]["out",i]  <- ifelse(is.na(out2/(hit2+out2)),0,out2/(hit2+out2))
  }
  
  for (i in 1:9){
    if (is.na(one[[2]]["K",i])==TRUE|is.na(one[[2]]["BB",i])==TRUE|is.na(one[[2]]["1B",i])==TRUE){
      one[[2]]["SB",i]  <- 0
      one[[2]]["CS",i]  <- 0
      one[[2]]["stay",i]  <- 1
      one[[2]]["GIDP",i]  <- .5
      one[[2]]["SFSH",i]  <- .25
      one[[2]]["outorhit",i]  <- .25
      one[[2]]["K",i]  <- .50
      one[[2]]["2B",i]  <- .30
      one[[2]]["3B",i]  <- .01
      one[[2]]["HR",i] <- .02
      one[[2]]["1B",i] <- .33
      one[[2]]["BB",i] <- .33
      one[[2]]["contact",i]  <- .50
      one[[2]]["hit",i] <- .25 
      one[[2]]["out",i]  <- .75
    }else{
      one[[2]]["SB",i]  <- one[[2]]["SB",i] 
      one[[2]]["CS",i]  <- one[[2]]["CS",i]
      one[[2]]["stay",i]  <- one[[2]]["stay",i]
      one[[2]]["GIDP",i]  <- one[[2]]["GIDP",i]
      one[[2]]["SFSH",i]  <- one[[2]]["SFSH",i]
      one[[2]]["outorhit",i]  <- one[[2]]["outorhit",i] 
      one[[2]]["K",i]  <- one[[2]]["K",i] 
      one[[2]]["2B",i]  <- one[[2]]["2B",i] 
      one[[2]]["3B",i]  <- one[[2]]["3B",i] 
      one[[2]]["HR",i] <- one[[2]]["HR",i]
      one[[2]]["1B",i] <- one[[2]]["1B",i] 
      one[[2]]["BB",i] <- one[[2]]["BB",i]
      one[[2]]["contact",i]  <- one[[2]]["contact",i] 
      one[[2]]["hit",i] <- one[[2]]["hit",i]
      one[[2]]["out",i]  <- one[[2]]["out",i]
    }
  }
  for (i in 1:9){
    if (is.na(one[[1]]["K",i])==TRUE|is.na(one[[1]]["BB",i])==TRUE|is.na(one[[1]]["1B",i])==TRUE){
      one[[1]]["SB",i]  <- 0
      one[[1]]["CS",i]  <- 0
      one[[1]]["stay",i]  <- 1
      one[[1]]["GIDP",i]  <- .5
      one[[1]]["SFSH",i]  <- .25
      one[[1]]["outorhit",i]  <- .25
      one[[1]]["K",i]  <- .50
      one[[1]]["2B",i]  <- .30
      one[[1]]["3B",i]  <- .01
      one[[1]]["HR",i] <- .02
      one[[1]]["1B",i] <- .33
      one[[1]]["BB",i] <- .33
      one[[1]]["contact",i]  <- .50
      one[[1]]["hit",i] <- .25 
      one[[1]]["out",i]  <- .75
    }else{
      one[[1]]["SB",i]  <- one[[1]]["SB",i] 
      one[[1]]["CS",i]  <- one[[1]]["CS",i]
      one[[1]]["stay",i]  <- one[[1]]["stay",i]
      one[[1]]["GIDP",i]  <- one[[1]]["GIDP",i]
      one[[1]]["SFSH",i]  <- one[[1]]["SFSH",i]
      one[[1]]["outorhit",i]  <- one[[1]]["outorhit",i] 
      one[[1]]["K",i]  <- one[[1]]["K",i] 
      one[[1]]["2B",i]  <- one[[1]]["2B",i] 
      one[[1]]["3B",i]  <- one[[1]]["3B",i] 
      one[[1]]["HR",i] <- one[[1]]["HR",i]
      one[[1]]["1B",i] <- one[[1]]["1B",i] 
      one[[1]]["BB",i] <- one[[1]]["BB",i]
      one[[1]]["contact",i]  <- one[[1]]["contact",i] 
      one[[1]]["hit",i] <- one[[1]]["hit",i]
      one[[1]]["out",i]  <- one[[1]]["out",i]
    }
  }
  
  for (i in 1:9){
    if (one[[2]]["K",i]<=0|one[[2]]["BB",i]<=0|one[[2]]["1B",i]<=0){
      one[[2]]["SB",i]  <- 0
      one[[2]]["CS",i]  <- 0
      one[[2]]["stay",i]  <- 1
      one[[2]]["GIDP",i]  <- .5
      one[[2]]["SFSH",i]  <- .25
      one[[2]]["outorhit",i]  <- .25
      one[[2]]["K",i]  <- .50
      one[[2]]["2B",i]  <- .30
      one[[2]]["3B",i]  <- .01
      one[[2]]["HR",i] <- .02
      one[[2]]["1B",i] <- .33
      one[[2]]["BB",i] <- .33
      one[[2]]["contact",i]  <- .50
      one[[2]]["hit",i] <- .25 
      one[[2]]["out",i]  <- .75
    }else{
      one[[2]]["SB",i]  <- one[[2]]["SB",i] 
      one[[2]]["CS",i]  <- one[[2]]["CS",i]
      one[[2]]["stay",i]  <- one[[2]]["stay",i]
      one[[2]]["GIDP",i]  <- one[[2]]["GIDP",i]
      one[[2]]["SFSH",i]  <- one[[2]]["SFSH",i]
      one[[2]]["outorhit",i]  <- one[[2]]["outorhit",i] 
      one[[2]]["K",i]  <- one[[2]]["K",i] 
      one[[2]]["2B",i]  <- one[[2]]["2B",i] 
      one[[2]]["3B",i]  <- one[[2]]["3B",i] 
      one[[2]]["HR",i] <- one[[2]]["HR",i]
      one[[2]]["1B",i] <- one[[2]]["1B",i] 
      one[[2]]["BB",i] <- one[[2]]["BB",i]
      one[[2]]["contact",i]  <- one[[2]]["contact",i] 
      one[[2]]["hit",i] <- one[[2]]["hit",i]
      one[[2]]["out",i]  <- one[[2]]["out",i]
    }
  }
  for (i in 1:9){
    if (one[[1]]["K",i]<=0|one[[1]]["BB",i]<=0|one[[1]]["1B",i]<=0){
      one[[1]]["SB",i]  <- 0
      one[[1]]["CS",i]  <- 0
      one[[1]]["stay",i]  <- 1
      one[[1]]["GIDP",i]  <- .5
      one[[1]]["SFSH",i]  <- .25
      one[[1]]["outorhit",i]  <- .25
      one[[1]]["K",i]  <- .50
      one[[1]]["2B",i]  <- .30
      one[[1]]["3B",i]  <- .01
      one[[1]]["HR",i] <- .02
      one[[1]]["1B",i] <- .33
      one[[1]]["BB",i] <- .33
      one[[1]]["contact",i]  <- .50
      one[[1]]["hit",i] <- .25 
      one[[1]]["out",i]  <- .75
    }else{
      one[[1]]["SB",i]  <- one[[1]]["SB",i] 
      one[[1]]["CS",i]  <- one[[1]]["CS",i]
      one[[1]]["stay",i]  <- one[[1]]["stay",i]
      one[[1]]["GIDP",i]  <- one[[1]]["GIDP",i]
      one[[1]]["SFSH",i]  <- one[[1]]["SFSH",i]
      one[[1]]["outorhit",i]  <- one[[1]]["outorhit",i] 
      one[[1]]["K",i]  <- one[[1]]["K",i] 
      one[[1]]["2B",i]  <- one[[1]]["2B",i] 
      one[[1]]["3B",i]  <- one[[1]]["3B",i] 
      one[[1]]["HR",i] <- one[[1]]["HR",i]
      one[[1]]["1B",i] <- one[[1]]["1B",i] 
      one[[1]]["BB",i] <- one[[1]]["BB",i]
      one[[1]]["contact",i]  <- one[[1]]["contact",i] 
      one[[1]]["hit",i] <- one[[1]]["hit",i]
      one[[1]]["out",i]  <- one[[1]]["out",i]
    }
  }
  
  list(one=one,two=player_bats)
  
}


