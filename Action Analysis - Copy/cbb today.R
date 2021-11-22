
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
files_raw <- list.files("C:/Users/CORSAIR GAMING/Documents/Action Analysis")
files_raw <- files_raw[grepl("cbb_",files_raw)]

for(i in 1:length(files_raw)){
  source(files_raw[i])
}

start <- Sys.time()




sec_cbbsharp_analysis_a <- NA
sec_cbbpublic_analysis_a <- NA
sec_cbbmoney_analysis_a <- NA
sec_cbbsharp_analysis_h <-NA
sec_cbbpublic_analysis_h <- NA
sec_cbbmoney_analysis_h <- NA
sec_cbbudf_games_today <- NA
tryCatch({
  sec_cbbgames_today <- sec_todays_cbb_analysis(sec_cbb_games_all)
  
  sec_cbbsharp_analysis_a <- sec_cbbgames_today[[1]]
  sec_cbbpublic_analysis_a <- sec_cbbgames_today[[2]]
  sec_cbbmoney_analysis_a <- sec_cbbgames_today[[3]]
  sec_cbbsharp_analysis_h <- sec_cbbgames_today[[4]]
  sec_cbbpublic_analysis_h <- sec_cbbgames_today[[5]]
  sec_cbbmoney_analysis_h <- sec_cbbgames_today[[6]]
  sec_cbbudf_games_today <- sec_cbbgames_today[[7]]
  
  sec_cbbsharp_analysis_a$x <- 5
  sec_cbbpublic_analysis_a$x <- 5
  sec_cbbmoney_analysis_a$x <- 5
  sec_cbbsharp_analysis_h$x <- 5
  sec_cbbpublic_analysis_h$x <- 5
  sec_cbbmoney_analysis_h$x <- 5
  sec_cbbudf_games_today$x <- 5
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

acc_cbbsharp_analysis_a <- NA
acc_cbbpublic_analysis_a <- NA
acc_cbbmoney_analysis_a <- NA
acc_cbbsharp_analysis_h <-NA
acc_cbbpublic_analysis_h <- NA
acc_cbbmoney_analysis_h <- NA
acc_cbbudf_games_today <- NA
tryCatch({
  acc_cbbgames_today <- acc_todays_cbb_analysis(acc_cbb_games_all)
  
  acc_cbbsharp_analysis_a <- acc_cbbgames_today[[1]]
  acc_cbbpublic_analysis_a <- acc_cbbgames_today[[2]]
  acc_cbbmoney_analysis_a <- acc_cbbgames_today[[3]]
  acc_cbbsharp_analysis_h <- acc_cbbgames_today[[4]]
  acc_cbbpublic_analysis_h <- acc_cbbgames_today[[5]]
  acc_cbbmoney_analysis_h <- acc_cbbgames_today[[6]]
  acc_cbbudf_games_today <- acc_cbbgames_today[[7]]
  
  acc_cbbsharp_analysis_a$x <- 6
  acc_cbbpublic_analysis_a$x <- 6
  acc_cbbmoney_analysis_a$x <- 6
  acc_cbbsharp_analysis_h$x <- 6
  acc_cbbpublic_analysis_h$x <- 6
  acc_cbbmoney_analysis_h$x <- 6
  acc_cbbudf_games_today$x <- 6
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})



big10_cbbsharp_analysis_a <- NA
big10_cbbpublic_analysis_a <- NA
big10_cbbmoney_analysis_a <- NA
big10_cbbsharp_analysis_h <-NA
big10_cbbpublic_analysis_h <- NA
big10_cbbmoney_analysis_h <- NA
big10_cbbudf_games_today <- NA
tryCatch({
  big10_cbbgames_today <- big10_todays_cbb_analysis(big10_cbb_games_all)
  
  big10_cbbsharp_analysis_a <- big10_cbbgames_today[[1]]
  big10_cbbpublic_analysis_a <- big10_cbbgames_today[[2]]
  big10_cbbmoney_analysis_a <- big10_cbbgames_today[[3]]
  big10_cbbsharp_analysis_h <- big10_cbbgames_today[[4]]
  big10_cbbpublic_analysis_h <- big10_cbbgames_today[[5]]
  big10_cbbmoney_analysis_h <- big10_cbbgames_today[[6]]
  big10_cbbudf_games_today <- big10_cbbgames_today[[7]]
  
  big10_cbbsharp_analysis_a$x <- 7
  big10_cbbpublic_analysis_a$x <- 7
  big10_cbbmoney_analysis_a$x <- 7
  big10_cbbsharp_analysis_h$x <- 7
  big10_cbbpublic_analysis_h$x <- 7
  big10_cbbmoney_analysis_h$x <- 7
  big10_cbbudf_games_today$x <- 7
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

big12_cbbsharp_analysis_a <- NA
big12_cbbpublic_analysis_a <- NA
big12_cbbmoney_analysis_a <- NA
big12_cbbsharp_analysis_h <-NA
big12_cbbpublic_analysis_h <- NA
big12_cbbmoney_analysis_h <- NA
big12_cbbudf_games_today <- NA
tryCatch({
  big12_cbbgames_today <- big12_todays_cbb_analysis(big12_cbb_games_all)
  
  big12_cbbsharp_analysis_a <- big12_cbbgames_today[[1]]
  big12_cbbpublic_analysis_a <- big12_cbbgames_today[[2]]
  big12_cbbmoney_analysis_a <- big12_cbbgames_today[[3]]
  big12_cbbsharp_analysis_h <- big12_cbbgames_today[[4]]
  big12_cbbpublic_analysis_h <- big12_cbbgames_today[[5]]
  big12_cbbmoney_analysis_h <- big12_cbbgames_today[[6]]
  big12_cbbudf_games_today <- big12_cbbgames_today[[7]]
  
  big12_cbbsharp_analysis_a$x <- 8
  big12_cbbpublic_analysis_a$x <- 8
  big12_cbbmoney_analysis_a$x <- 8
  big12_cbbsharp_analysis_h$x <- 8
  big12_cbbpublic_analysis_h$x <- 8
  big12_cbbmoney_analysis_h$x <- 8
  big12_cbbudf_games_today$x <- 8
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

pac12_cbbsharp_analysis_a <- NA
pac12_cbbpublic_analysis_a <- NA
pac12_cbbmoney_analysis_a <- NA
pac12_cbbsharp_analysis_h <-NA
pac12_cbbpublic_analysis_h <- NA
pac12_cbbmoney_analysis_h <- NA
pac12_cbbudf_games_today <- NA
tryCatch({
  pac12_cbbgames_today <- pac12_todays_cbb_analysis(pac12_cbb_games_all)
  
  pac12_cbbsharp_analysis_a <- pac12_cbbgames_today[[1]]
  pac12_cbbpublic_analysis_a <- pac12_cbbgames_today[[2]]
  pac12_cbbmoney_analysis_a <- pac12_cbbgames_today[[3]]
  pac12_cbbsharp_analysis_h <- pac12_cbbgames_today[[4]]
  pac12_cbbpublic_analysis_h <- pac12_cbbgames_today[[5]]
  pac12_cbbmoney_analysis_h <- pac12_cbbgames_today[[6]]
  pac12_cbbudf_games_today <- pac12_cbbgames_today[[7]]
  
  pac12_cbbsharp_analysis_a$x <- 9
  pac12_cbbpublic_analysis_a$x <- 9
  pac12_cbbmoney_analysis_a$x <- 9
  pac12_cbbsharp_analysis_h$x <- 9
  pac12_cbbpublic_analysis_h$x <- 9
  pac12_cbbmoney_analysis_h$x <- 9
  pac12_cbbudf_games_today$x <- 9
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

ivy_cbbsharp_analysis_a <- NA
ivy_cbbpublic_analysis_a <- NA
ivy_cbbmoney_analysis_a <- NA
ivy_cbbsharp_analysis_h <-NA
ivy_cbbpublic_analysis_h <- NA
ivy_cbbmoney_analysis_h <- NA
ivy_cbbudf_games_today <- NA
tryCatch({
  ivy_cbbgames_today <- ivy_todays_cbb_analysis(ivy_cbb_games_all)
  
  ivy_cbbsharp_analysis_a <- ivy_cbbgames_today[[1]]
  ivy_cbbpublic_analysis_a <- ivy_cbbgames_today[[2]]
  ivy_cbbmoney_analysis_a <- ivy_cbbgames_today[[3]]
  ivy_cbbsharp_analysis_h <- ivy_cbbgames_today[[4]]
  ivy_cbbpublic_analysis_h <- ivy_cbbgames_today[[5]]
  ivy_cbbmoney_analysis_h <- ivy_cbbgames_today[[6]]
  ivy_cbbudf_games_today <- ivy_cbbgames_today[[7]]
  
  ivy_cbbsharp_analysis_a$x <- 10
  ivy_cbbpublic_analysis_a$x <-10
  ivy_cbbmoney_analysis_a$x <- 10
  ivy_cbbsharp_analysis_h$x <- 10
  ivy_cbbpublic_analysis_h$x <-10
  ivy_cbbmoney_analysis_h$x <- 10
  ivy_cbbudf_games_today$x <- 10
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

bigsky_cbbsharp_analysis_a <- NA
bigsky_cbbpublic_analysis_a <- NA
bigsky_cbbmoney_analysis_a <- NA
bigsky_cbbsharp_analysis_h <-NA
bigsky_cbbpublic_analysis_h <- NA
bigsky_cbbmoney_analysis_h <- NA
bigsky_cbbudf_games_today <- NA
tryCatch({
  bigsky_cbbgames_today <- bigsky_todays_cbb_analysis(bigsky_cbb_games_all)
  
  bigsky_cbbsharp_analysis_a <- bigsky_cbbgames_today[[1]]
  bigsky_cbbpublic_analysis_a <- bigsky_cbbgames_today[[2]]
  bigsky_cbbmoney_analysis_a <- bigsky_cbbgames_today[[3]]
  bigsky_cbbsharp_analysis_h <- bigsky_cbbgames_today[[4]]
  bigsky_cbbpublic_analysis_h <- bigsky_cbbgames_today[[5]]
  bigsky_cbbmoney_analysis_h <- bigsky_cbbgames_today[[6]]
  bigsky_cbbudf_games_today <- bigsky_cbbgames_today[[7]]
  
  bigsky_cbbsharp_analysis_a$x <- 11
  bigsky_cbbpublic_analysis_a$x <-11
  bigsky_cbbmoney_analysis_a$x <- 11
  bigsky_cbbsharp_analysis_h$x <- 11
  bigsky_cbbpublic_analysis_h$x <-11
  bigsky_cbbmoney_analysis_h$x <- 11
  bigsky_cbbudf_games_today$x <- 11
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

bigeast_cbbsharp_analysis_a <- NA
bigeast_cbbpublic_analysis_a <- NA
bigeast_cbbmoney_analysis_a <- NA
bigeast_cbbsharp_analysis_h <-NA
bigeast_cbbpublic_analysis_h <- NA
bigeast_cbbmoney_analysis_h <- NA
bigeast_cbbudf_games_today <- NA
tryCatch({
  bigeast_cbbgames_today <- bigeast_todays_cbb_analysis(bigeast_cbb_games_all)
  
  bigeast_cbbsharp_analysis_a <- bigeast_cbbgames_today[[1]]
  bigeast_cbbpublic_analysis_a <- bigeast_cbbgames_today[[2]]
  bigeast_cbbmoney_analysis_a <- bigeast_cbbgames_today[[3]]
  bigeast_cbbsharp_analysis_h <- bigeast_cbbgames_today[[4]]
  bigeast_cbbpublic_analysis_h <- bigeast_cbbgames_today[[5]]
  bigeast_cbbmoney_analysis_h <- bigeast_cbbgames_today[[6]]
  bigeast_cbbudf_games_today <- bigeast_cbbgames_today[[7]]
  
  bigeast_cbbsharp_analysis_a$x <- 12
  bigeast_cbbpublic_analysis_a$x <-12
  bigeast_cbbmoney_analysis_a$x <- 12
  bigeast_cbbsharp_analysis_h$x <- 12
  bigeast_cbbpublic_analysis_h$x <-12
  bigeast_cbbmoney_analysis_h$x <- 12
  bigeast_cbbudf_games_today$x <- 12
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

a10_cbbsharp_analysis_a <- NA
a10_cbbpublic_analysis_a <- NA
a10_cbbmoney_analysis_a <- NA
a10_cbbsharp_analysis_h <-NA
a10_cbbpublic_analysis_h <- NA
a10_cbbmoney_analysis_h <- NA
a10_cbbudf_games_today <- NA
tryCatch({
  a10_cbbgames_today <- a10_todays_cbb_analysis(a10_cbb_games_all)
  
  a10_cbbsharp_analysis_a <- a10_cbbgames_today[[1]]
  a10_cbbpublic_analysis_a <- a10_cbbgames_today[[2]]
  a10_cbbmoney_analysis_a <- a10_cbbgames_today[[3]]
  a10_cbbsharp_analysis_h <- a10_cbbgames_today[[4]]
  a10_cbbpublic_analysis_h <- a10_cbbgames_today[[5]]
  a10_cbbmoney_analysis_h <- a10_cbbgames_today[[6]]
  a10_cbbudf_games_today <- a10_cbbgames_today[[7]]
  
  a10_cbbsharp_analysis_a$x <- 13
  a10_cbbpublic_analysis_a$x <-13
  a10_cbbmoney_analysis_a$x <- 13
  a10_cbbsharp_analysis_h$x <- 13
  a10_cbbpublic_analysis_h$x <-13
  a10_cbbmoney_analysis_h$x <- 13
  a10_cbbudf_games_today$x <- 13
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

aac_cbbsharp_analysis_a <- NA
aac_cbbpublic_analysis_a <- NA
aac_cbbmoney_analysis_a <- NA
aac_cbbsharp_analysis_h <-NA
aac_cbbpublic_analysis_h <- NA
aac_cbbmoney_analysis_h <- NA
aac_cbbudf_games_today <- NA
tryCatch({
  aac_cbbgames_today <- aac_todays_cbb_analysis(aac_cbb_games_all)
  
  aac_cbbsharp_analysis_a <- aac_cbbgames_today[[1]]
  aac_cbbpublic_analysis_a <- aac_cbbgames_today[[2]]
  aac_cbbmoney_analysis_a <- aac_cbbgames_today[[3]]
  aac_cbbsharp_analysis_h <- aac_cbbgames_today[[4]]
  aac_cbbpublic_analysis_h <- aac_cbbgames_today[[5]]
  aac_cbbmoney_analysis_h <- aac_cbbgames_today[[6]]
  aac_cbbudf_games_today <- aac_cbbgames_today[[7]]
  
  aac_cbbsharp_analysis_a$x <- 14
  aac_cbbpublic_analysis_a$x <-14
  aac_cbbmoney_analysis_a$x <- 14
  aac_cbbsharp_analysis_h$x <- 14
  aac_cbbpublic_analysis_h$x <-14
  aac_cbbmoney_analysis_h$x <- 14
  aac_cbbudf_games_today$x <- 14
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

cusa_cbbsharp_analysis_a <- NA
cusa_cbbpublic_analysis_a <- NA
cusa_cbbmoney_analysis_a <- NA
cusa_cbbsharp_analysis_h <-NA
cusa_cbbpublic_analysis_h <- NA
cusa_cbbmoney_analysis_h <- NA
cusa_cbbudf_games_today <- NA
tryCatch({
  cusa_cbbgames_today <- cusa_todays_cbb_analysis(cusa_cbb_games_all)
  
  cusa_cbbsharp_analysis_a <- cusa_cbbgames_today[[1]]
  cusa_cbbpublic_analysis_a <- cusa_cbbgames_today[[2]]
  cusa_cbbmoney_analysis_a <- cusa_cbbgames_today[[3]]
  cusa_cbbsharp_analysis_h <- cusa_cbbgames_today[[4]]
  cusa_cbbpublic_analysis_h <- cusa_cbbgames_today[[5]]
  cusa_cbbmoney_analysis_h <- cusa_cbbgames_today[[6]]
  cusa_cbbudf_games_today <- cusa_cbbgames_today[[7]]
  
  cusa_cbbsharp_analysis_a$x <- 15
  cusa_cbbpublic_analysis_a$x <-15
  cusa_cbbmoney_analysis_a$x <- 15
  cusa_cbbsharp_analysis_h$x <- 15
  cusa_cbbpublic_analysis_h$x <-15
  cusa_cbbmoney_analysis_h$x <- 15
  cusa_cbbudf_games_today$x <- 15
  print("The NCAAB has games this week.")
  
  
  
}, error=function(e){
  
  print("Nothing for the NCAAB this week.")
})

c("nhl","nba","cfb","nfl",
  "cbb - sec","cbb - acc","cbb - big10","cbb - big12","cbb - pac12","cbb - ivy",
  "cbb - bigsky","cbb - bigeast","cbb - a10","cbb - aac","cbb - cusa",
  "mlb")