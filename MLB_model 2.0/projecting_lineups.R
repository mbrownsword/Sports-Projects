for(t in 1:length(depth_chart_list)){
  t <- 3
  if(depth_chart_list[[t]]$team==mlb_library$espn_name[mlb_library$roto_name==mlb_teams_today[b]]){
    if(mlb_library$league[mlb_library$roto_name==mlb_teams_today[b]]=="american"){
      starters <- depth_chart_list[[t]]$depthchart[c(4:12),1]
      positionz <- row.names(depth_chart_list[[t]]$depthchart[c(4:12),])
      for(u in 1:length(starters)){
        if(grepl("DD",starters[u]) || 
           grepl("60",starters[u]) ||
           grepl("10",starters[u]) ||
           grepl("7",starters[u]) ||
           grepl("SUSP",starters[u]) ||
           grepl("PL",starters[u]) ||
           grepl("BL",starters[u]) ||
           str_sub(starters[u],-2,-1)==" O"){
          replacement <- which(starters[u]==depth_chart_list[[t]]$depthchart[,1])
          replacement_player <- depth_chart_list[[t]]$depthchart[replacement,2]
          if(grepl("DD",replacement_player) || 
             grepl("60",replacement_player) ||
             grepl("10",replacement_player) ||
             grepl("7",replacement_player) ||
             grepl("SUSP",replacement_player) ||
             grepl("PL",replacement_player) ||
             grepl("BL",replacement_player) ||
             str_sub(replacement_player,-2,-1)==" O" || 
             any(grepl(replacement_player,starters))==T){
            replacement_player_one <- depth_chart_list[[t]]$depthchart[replacement,3]
            if(grepl("DD",replacement_player_one) || 
               grepl("60",replacement_player_one) ||
               grepl("10",replacement_player_one) ||
               grepl("7",replacement_player_one) ||
               grepl("SUSP",replacement_player_one) ||
               grepl("PL",replacement_player_one) ||
               grepl("BL",replacement_player_one) ||
               str_sub(replacement_player_one,-2,-1)==" O" || 
               any(grepl(replacement_player_one,starters))==T){
              starters[u] <- depth_chart_list[[t]]$depthchart[replacement,4]
            }else{
              starters[u] <- replacement_player_one
            }
          }else{
            starters[u] <- replacement_player
          }
        }else{
          next()
        }
      }
      obp <- 0
      for(y in 1:length(starters)){
        if(length(stats$ZUB$ï..Name[stats$ZUB$ï..Name==starters[[y]]])>0){
          obp[y] <- as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==starters[[y]]]))
        }else{
          if(length(stats$SUB$ï..Name[stats$SUB$ï..Name==starters[[y]]])>0){
            obp[y] <- as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==starters[[y]]]))
          }else{
            if(length(stats$ZB$ï..Name[stats$ZB$ï..Name==starters[[y]]])>0){
              obp[y] <- as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==starters[[y]]]))
            }else{
              obp[y] <- as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==starters[[y]]]))
            }
          }
        }
      }
      starter_df <- data.frame(starters, obp, positionz)
      starter_df <- starter_df[order(starter_df$obp, decreasing = T),]
      lineupz <- as.character(starter_df$starters)
      positionz <- as.character(starter_df$positionz)
    }else{
      t <- 1
      starters <- depth_chart_list[[t]]$depthchart[c(4:11,1),1]
      positionz <- row.names(depth_chart_list[[t]]$depthchart[c(4:11,1),])
      for(u in 1:length(starters)){
        if(grepl("DD",starters[u]) || 
           grepl("60",starters[u]) ||
           grepl("10",starters[u]) ||
           grepl("7",starters[u]) ||
           grepl("SUSP",starters[u]) ||
           grepl("PL",starters[u]) ||
           grepl("BL",starters[u]) ||
           str_sub(starters[u],-2,-1)==" O"){
          replacement <- which(starters[u]==depth_chart_list[[t]]$depthchart[,1])
          replacement_player <- depth_chart_list[[t]]$depthchart[replacement,2]
          if(grepl("DD",replacement_player) || 
             grepl("60",replacement_player) ||
             grepl("10",replacement_player) ||
             grepl("7",replacement_player) ||
             grepl("SUSP",replacement_player) ||
             grepl("PL",replacement_player) ||
             grepl("BL",replacement_player) ||
             str_sub(replacement_player,-2,-1)==" O" || 
             any(grepl(replacement_player,starters))==T){
            replacement_player_one <- depth_chart_list[[t]]$depthchart[replacement,3]
            if(grepl("DD",replacement_player_one) || 
               grepl("60",replacement_player_one) ||
               grepl("10",replacement_player_one) ||
               grepl("7",replacement_player_one) ||
               grepl("SUSP",replacement_player_one) ||
               grepl("PL",replacement_player_one) ||
               grepl("BL",replacement_player_one) ||
               str_sub(replacement_player_one,-2,-1)==" O" || 
               any(grepl(replacement_player_one,starters))==T){
              starters[u] <- depth_chart_list[[t]]$depthchart[replacement,4]
            }else{
              starters[u] <- replacement_player_one
            }
          }else{
            starters[u] <- replacement_player
          }
        }else{
          next()
        }
      }
      obp <- 0
      for(y in 1:(length(starters)-1)){
        if(length(stats$ZUB$ï..Name[stats$ZUB$ï..Name==starters[[y]]])>0){
          obp[y] <- as.numeric(as.character(stats$ZUB$OBP[stats$ZUB$ï..Name==starters[[y]]]))
        }else{
          if(length(stats$SUB$ï..Name[stats$SUB$ï..Name==starters[[y]]])>0){
            obp[y] <- as.numeric(as.character(stats$SUB$OBP[stats$SUB$ï..Name==starters[[y]]]))
          }else{
            if(length(stats$ZB$ï..Name[stats$ZB$ï..Name==starters[[y]]])>0){
              obp[y] <- as.numeric(as.character(stats$ZB$OBP[stats$ZB$ï..Name==starters[[y]]]))
            }else{
              obp[y] <- as.numeric(as.character(stats$SB$OBP[stats$SB$ï..Name==starters[[y]]]))
            }
          }
        }
      }
      obp[9] <- 0
      starter_df <- data.frame(starters, obp, positionz)
      starter_df <- starter_df[order(starter_df$obp, decreasing = T),]
      lineupz <- as.character(starter_df$starters)
      positionz <- as.character(starter_df$positionz)
     }
  }else{
    next()
  }
}