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

sec_cbb_games_all <- sec_cbb_backtest()

sec_cbb_games_all$money_home_ml <- as.numeric(sec_cbb_games_all$money_home_ml)
sec_cbb_games_all$money_home <- as.numeric(sec_cbb_games_all$money_home)
sec_cbb_games_all$money_away_ml <- as.numeric(sec_cbb_games_all$money_away_ml)
sec_cbb_games_all$money_away <- as.numeric(sec_cbb_games_all$money_away)
sec_cbb_games_all$public_home_ml <- as.numeric(sec_cbb_games_all$public_home_ml)
sec_cbb_games_all$public_home <- as.numeric(sec_cbb_games_all$public_home)
sec_cbb_games_all$public_away_ml <- as.numeric(sec_cbb_games_all$public_away_ml)
sec_cbb_games_all$public_away <- as.numeric(sec_cbb_games_all$public_away)

sec_cbb_udf_games_all <- sec_cbb_udf_analysis(sec_cbb_games_all)

ivy_cbb_games_all <- ivy_cbb_backtest()

ivy_cbb_games_all$money_home_ml <- as.numeric(ivy_cbb_games_all$money_home_ml)
ivy_cbb_games_all$money_home <- as.numeric(ivy_cbb_games_all$money_home)
ivy_cbb_games_all$money_away_ml <- as.numeric(ivy_cbb_games_all$money_away_ml)
ivy_cbb_games_all$money_away <- as.numeric(ivy_cbb_games_all$money_away)
ivy_cbb_games_all$public_home_ml <- as.numeric(ivy_cbb_games_all$public_home_ml)
ivy_cbb_games_all$public_home <- as.numeric(ivy_cbb_games_all$public_home)
ivy_cbb_games_all$public_away_ml <- as.numeric(ivy_cbb_games_all$public_away_ml)
ivy_cbb_games_all$public_away <- as.numeric(ivy_cbb_games_all$public_away)

ivy_cbb_udf_games_all <- ivy_cbb_udf_analysis(ivy_cbb_games_all)

acc_cbb_games_all <- acc_cbb_backtest()

acc_cbb_games_all$money_home_ml <- as.numeric(acc_cbb_games_all$money_home_ml)
acc_cbb_games_all$money_home <- as.numeric(acc_cbb_games_all$money_home)
acc_cbb_games_all$money_away_ml <- as.numeric(acc_cbb_games_all$money_away_ml)
acc_cbb_games_all$money_away <- as.numeric(acc_cbb_games_all$money_away)
acc_cbb_games_all$public_home_ml <- as.numeric(acc_cbb_games_all$public_home_ml)
acc_cbb_games_all$public_home <- as.numeric(acc_cbb_games_all$public_home)
acc_cbb_games_all$public_away_ml <- as.numeric(acc_cbb_games_all$public_away_ml)
acc_cbb_games_all$public_away <- as.numeric(acc_cbb_games_all$public_away)

acc_cbb_udf_games_all <- acc_cbb_udf_analysis(acc_cbb_games_all)

big10_cbb_games_all <- big10_cbb_backtest()

big10_cbb_games_all$money_home_ml <- as.numeric(big10_cbb_games_all$money_home_ml)
big10_cbb_games_all$money_home <- as.numeric(big10_cbb_games_all$money_home)
big10_cbb_games_all$money_away_ml <- as.numeric(big10_cbb_games_all$money_away_ml)
big10_cbb_games_all$money_away <- as.numeric(big10_cbb_games_all$money_away)
big10_cbb_games_all$public_home_ml <- as.numeric(big10_cbb_games_all$public_home_ml)
big10_cbb_games_all$public_home <- as.numeric(big10_cbb_games_all$public_home)
big10_cbb_games_all$public_away_ml <- as.numeric(big10_cbb_games_all$public_away_ml)
big10_cbb_games_all$public_away <- as.numeric(big10_cbb_games_all$public_away)

big10_cbb_udf_games_all <- big10_cbb_udf_analysis(big10_cbb_games_all)

big12_cbb_games_all <- big12_cbb_backtest()

big12_cbb_games_all$money_home_ml <- as.numeric(big12_cbb_games_all$money_home_ml)
big12_cbb_games_all$money_home <- as.numeric(big12_cbb_games_all$money_home)
big12_cbb_games_all$money_away_ml <- as.numeric(big12_cbb_games_all$money_away_ml)
big12_cbb_games_all$money_away <- as.numeric(big12_cbb_games_all$money_away)
big12_cbb_games_all$public_home_ml <- as.numeric(big12_cbb_games_all$public_home_ml)
big12_cbb_games_all$public_home <- as.numeric(big12_cbb_games_all$public_home)
big12_cbb_games_all$public_away_ml <- as.numeric(big12_cbb_games_all$public_away_ml)
big12_cbb_games_all$public_away <- as.numeric(big12_cbb_games_all$public_away)

big12_cbb_udf_games_all <- big12_cbb_udf_analysis(big12_cbb_games_all)

pac12_cbb_games_all <- pac12_cbb_backtest()

pac12_cbb_games_all$money_home_ml <- as.numeric(pac12_cbb_games_all$money_home_ml)
pac12_cbb_games_all$money_home <- as.numeric(pac12_cbb_games_all$money_home)
pac12_cbb_games_all$money_away_ml <- as.numeric(pac12_cbb_games_all$money_away_ml)
pac12_cbb_games_all$money_away <- as.numeric(pac12_cbb_games_all$money_away)
pac12_cbb_games_all$public_home_ml <- as.numeric(pac12_cbb_games_all$public_home_ml)
pac12_cbb_games_all$public_home <- as.numeric(pac12_cbb_games_all$public_home)
pac12_cbb_games_all$public_away_ml <- as.numeric(pac12_cbb_games_all$public_away_ml)
pac12_cbb_games_all$public_away <- as.numeric(pac12_cbb_games_all$public_away)

pac12_cbb_udf_games_all <- pac12_cbb_udf_analysis(pac12_cbb_games_all)

bigsky_cbb_games_all <- bigsky_cbb_backtest()

bigsky_cbb_games_all$money_home_ml <- as.numeric(bigsky_cbb_games_all$money_home_ml)
bigsky_cbb_games_all$money_home <- as.numeric(bigsky_cbb_games_all$money_home)
bigsky_cbb_games_all$money_away_ml <- as.numeric(bigsky_cbb_games_all$money_away_ml)
bigsky_cbb_games_all$money_away <- as.numeric(bigsky_cbb_games_all$money_away)
bigsky_cbb_games_all$public_home_ml <- as.numeric(bigsky_cbb_games_all$public_home_ml)
bigsky_cbb_games_all$public_home <- as.numeric(bigsky_cbb_games_all$public_home)
bigsky_cbb_games_all$public_away_ml <- as.numeric(bigsky_cbb_games_all$public_away_ml)
bigsky_cbb_games_all$public_away <- as.numeric(bigsky_cbb_games_all$public_away)

bigsky_cbb_udf_games_all <- bigsky_cbb_udf_analysis(bigsky_cbb_games_all)

bigeast_cbb_games_all <- bigeast_cbb_backtest()

bigeast_cbb_games_all$money_home_ml <- as.numeric(bigeast_cbb_games_all$money_home_ml)
bigeast_cbb_games_all$money_home <- as.numeric(bigeast_cbb_games_all$money_home)
bigeast_cbb_games_all$money_away_ml <- as.numeric(bigeast_cbb_games_all$money_away_ml)
bigeast_cbb_games_all$money_away <- as.numeric(bigeast_cbb_games_all$money_away)
bigeast_cbb_games_all$public_home_ml <- as.numeric(bigeast_cbb_games_all$public_home_ml)
bigeast_cbb_games_all$public_home <- as.numeric(bigeast_cbb_games_all$public_home)
bigeast_cbb_games_all$public_away_ml <- as.numeric(bigeast_cbb_games_all$public_away_ml)
bigeast_cbb_games_all$public_away <- as.numeric(bigeast_cbb_games_all$public_away)

bigeast_cbb_udf_games_all <- bigeast_cbb_udf_analysis(bigeast_cbb_games_all)

a10_cbb_games_all <- a10_cbb_backtest()

finish <- Sys.time()

start - finish

a10_cbb_games_all$money_home_ml <- as.numeric(a10_cbb_games_all$money_home_ml)
a10_cbb_games_all$money_home <- as.numeric(a10_cbb_games_all$money_home)
a10_cbb_games_all$money_away_ml <- as.numeric(a10_cbb_games_all$money_away_ml)
a10_cbb_games_all$money_away <- as.numeric(a10_cbb_games_all$money_away)
a10_cbb_games_all$public_home_ml <- as.numeric(a10_cbb_games_all$public_home_ml)
a10_cbb_games_all$public_home <- as.numeric(a10_cbb_games_all$public_home)
a10_cbb_games_all$public_away_ml <- as.numeric(a10_cbb_games_all$public_away_ml)
a10_cbb_games_all$public_away <- as.numeric(a10_cbb_games_all$public_away)

a10_cbb_udf_games_all <- a10_cbb_udf_analysis(a10_cbb_games_all)

aac_cbb_games_all <- aac_cbb_backtest()

aac_cbb_games_all$money_home_ml <- as.numeric(aac_cbb_games_all$money_home_ml)
aac_cbb_games_all$money_home <- as.numeric(aac_cbb_games_all$money_home)
aac_cbb_games_all$money_away_ml <- as.numeric(aac_cbb_games_all$money_away_ml)
aac_cbb_games_all$money_away <- as.numeric(aac_cbb_games_all$money_away)
aac_cbb_games_all$public_home_ml <- as.numeric(aac_cbb_games_all$public_home_ml)
aac_cbb_games_all$public_home <- as.numeric(aac_cbb_games_all$public_home)
aac_cbb_games_all$public_away_ml <- as.numeric(aac_cbb_games_all$public_away_ml)
aac_cbb_games_all$public_away <- as.numeric(aac_cbb_games_all$public_away)

aac_cbb_udf_games_all <- aac_cbb_udf_analysis(aac_cbb_games_all)

cusa_cbb_games_all <- cusa_cbb_backtest()

cusa_cbb_games_all$money_home_ml <- as.numeric(cusa_cbb_games_all$money_home_ml)
cusa_cbb_games_all$money_home <- as.numeric(cusa_cbb_games_all$money_home)
cusa_cbb_games_all$money_away_ml <- as.numeric(cusa_cbb_games_all$money_away_ml)
cusa_cbb_games_all$money_away <- as.numeric(cusa_cbb_games_all$money_away)
cusa_cbb_games_all$public_home_ml <- as.numeric(cusa_cbb_games_all$public_home_ml)
cusa_cbb_games_all$public_home <- as.numeric(cusa_cbb_games_all$public_home)
cusa_cbb_games_all$public_away_ml <- as.numeric(cusa_cbb_games_all$public_away_ml)
cusa_cbb_games_all$public_away <- as.numeric(cusa_cbb_games_all$public_away)

cusa_cbb_udf_games_all <- cusa_cbb_udf_analysis(cusa_cbb_games_all)