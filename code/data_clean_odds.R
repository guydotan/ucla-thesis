## cleaning all the nba betting lines ##

library("readxl")
library("dplyr")
setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data")
teamIDs1 <- read.csv("team_id_map.csv", stringsAsFactors = F)

nba_07 <- read_excel("nba odds 2007-08.xlsx", col_types = "text")
nba_08 <- read_excel("nba odds 2008-09.xlsx", col_types = "text")
nba_09 <- read_excel("nba odds 2009-10.xlsx", col_types = "text")
nba_10 <- read_excel("nba odds 2010-11.xlsx", col_types = "text")
nba_11 <- read_excel("nba odds 2011-12.xlsx", col_types = "text")
nba_12 <- read_excel("nba odds 2012-13.xlsx", col_types = "text")
nba_13 <- read_excel("nba odds 2013-14.xlsx", col_types = "text")
nba_14 <- read_excel("nba odds 2014-15.xlsx", col_types = "text")
nba_15 <- read_excel("nba odds 2015-16.xlsx", col_types = "text")
nba_16 <- read_excel("nba odds 2016-17.xlsx", col_types = "text")
nba_17 <- read_excel("nba odds 2017-18.xlsx", col_types = "text")
nba_18 <- read_excel("nba odds 2018-19.xlsx", col_types = "text")
nba_19 <- read_excel("nba odds 2019-20.xlsx", col_types = "text")

nba_07$Season <- 200708
nba_08$Season <- 200809
nba_09$Season <- 200910
nba_10$Season <- 201011
nba_11$Season <- 201112
nba_12$Season <- 201213
nba_13$Season <- 201314
nba_14$Season <- 201415
nba_15$Season <- 201516
nba_16$Season <- 201617
nba_17$Season <- 201718
nba_18$Season <- 201819
nba_19$Season <- 201920

nba_0720 <- rbind(
  nba_07 ,
  nba_08 ,
  nba_09 ,
  nba_10 ,
  nba_11 ,
  nba_12 ,
  nba_13 ,
  nba_14 ,
  nba_15 ,
  nba_16 ,
  nba_17 ,
  nba_18 ,
  nba_19
)


# fix incorrect dates
nba_0720[809,]$Date <- 1225
nba_0720[810,]$Date <- 1225 
nba_0720[811,]$Date <- 1225
nba_0720[812,]$Date <- 1225
nba_0720[813,]$Date <- 1225
nba_0720[814,]$Date <- 1225
nba_0720[5253,]$Date <- 604
nba_0720[5254,]$Date <- 604
nba_0720[7815,]$Date <- 502
nba_0720[7816,]$Date <- 502
nba_0720[8749,]$Date <- 1225
nba_0720[8750,]$Date <- 1225
nba_0720[8751,]$Date <- 1225
nba_0720[8752,]$Date <- 1225
nba_0720[8753,]$Date <- 1225
nba_0720[8754,]$Date <- 1225
nba_0720[8755,]$Date <- 1225
nba_0720[8756,]$Date <- 1225
nba_0720[8757,]$Date <- 1225
nba_0720[8758,]$Date <- 1225
nba_0720[9282,]$Date <- 129
nba_0720[25747,]$Date <- 507
nba_0720[25748,]$Date <- 507
nba_0720[25749,]$Date <- 507
nba_0720[25750,]$Date <- 507
nba_0720[25751,]$Date <- 507
nba_0720[25752,]$Date <- 507
nba_0720[25753,]$Date <- 506
nba_0720[25754,]$Date <- 506
nba_0720[7815,]$Date <- 502
nba_0720[7816,]$Date <- 502
#nba_0720[2185,]$Date <- 329
#nba_0720[2186,]$Date <- 329


table(nba_0720$Season)
str(nba_0720)

# cases with No Line (NL)
apply(nba_0720, 2, function(x) length(which(x %in% c("NL"))))

# clean up team names
nba_0720$Team <- gsub(" ", "", nba_0720$Team)

nba_0720$Date <- as.numeric(nba_0720$Date)
nba_0720$Rot <- as.numeric(nba_0720$Rot)
#nba_0720$VH
#nba_0720$Team
nba_0720$`1st`  <- as.numeric(nba_0720$`1st`)
nba_0720$`2nd`  <- as.numeric(nba_0720$`2nd`)
nba_0720$`3rd`  <- as.numeric(nba_0720$`3rd`)
nba_0720$`4th`  <- as.numeric(nba_0720$`4th`)
nba_0720$Final  <- as.numeric(nba_0720$`4th`) 
nba_0720$Open   <- as.numeric(nba_0720$Open)
nba_0720$Close  <- as.numeric(nba_0720$Close)
nba_0720$ML     <- as.numeric(nba_0720$ML)
nba_0720$`2H`   <- as.numeric(nba_0720$`2H`)

# cases with NAs
apply(nba_0720, 2, function(x) length(which(is.na(x))))

odds_0720 <- nba_0720[ , c(14,1:13)]

# create common game ID
odds_0720$dateID <- sapply( odds_0720$Date , function(x) ifelse( nchar(x) > 3 , x , paste0(0,x) ) )
odds_0720_id <- left_join( odds_0720 , teamIDs1[ , c("teamID","odds")] , by = c("Team"="odds") )
odds_0720_id <- left_join( odds_0720_id , gamelogs_0720_id[, c("seasonID","dateID","teamID","oppID", "locationGame")] , by = c("Season"="seasonID", "dateID"="dateID", "teamID"="teamID") )

# check NAs
apply(odds_0720_id, 2, function(x) length(which(is.na(x))))

odds_0720_id$teamIDchar <- sapply( odds_0720_id$teamID , function(x) ifelse( nchar(x) == 2 , x , paste0(0,x) ) )
odds_0720_id$oppIDchar <- sapply( odds_0720_id$oppID , function(x) ifelse( nchar(x) == 2 , x , paste0(0,x) ) )


odds_0720_id$newGameID <- paste0( odds_0720_id$Season , "_" ,
                                  odds_0720_id$dateID , "_" ,
                                  ifelse( odds_0720_id$locationGame == 'A' , odds_0720_id$teamIDchar , odds_0720_id$oppIDchar ),  "_" ,
                                  ifelse( odds_0720_id$locationGame == 'A' , odds_0720_id$oppIDchar , odds_0720_id$teamIDchar )
                                  )

# export data
write.csv(odds_0720_id, "nba_odds_clean.csv", na = "", row.names = F)
