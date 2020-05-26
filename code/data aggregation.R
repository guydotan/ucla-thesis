## data aggregation steps ##

setwd("~/Documents/UCLA MAS/Thesis/repo-backup/uclathesis/")
library("dplyr")
library("ggplot2")
library("data.table")
library("zoo")
#options(scipen = 99999)

nba <- read.csv("data/nba_adv_complete.csv", stringsAsFactors = F, na.strings = "")

# add win probability
nba$teamWinProb <- apply(nba[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$team )
nba$oppWinProb <- apply(nba[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$opp )

# add in all-star break variable
asg <- read.csv("data/id-mapping/asg.csv", stringsAsFactors = F, na.strings = "")
asg$Date <- as.Date(asg$Date)
nba$dateGame <- as.Date(nba$dateGame)
nba <- left_join( nba , asg[,c(1,4)], by = c("slugSeason" = "slugSeason"))
nba$asb_prepost <- ifelse(nba$dateGame < nba$Date , "pre", "post")
nba$isLoss <- ifelse(nba$isWin == T, F, T)

# fix B2B variables
nba$isB2BFirst.own <- ifelse(nba$countDaysNextGameTeam.own == 0 & is.na(nba$countDaysNextGameTeam.own) == F , TRUE  , FALSE)
nba$isB2BSecond.own <- ifelse(nba$countDaysRestTeam.own == 0 & is.na(nba$countDaysNextGameTeam.own) == F, TRUE  , FALSE)
nba$isB2B.own <- ifelse(nba$isB2BFirst.own == T | nba$isB2BSecond.own == T , TRUE , FALSE)
nba$isB2BFirst.opp <- ifelse(nba$countDaysNextGameTeam.opp == 0 & is.na(nba$countDaysNextGameTeam.own) == F, TRUE  , FALSE)
nba$isB2BSecond.opp <- ifelse(nba$countDaysRestTeam.opp == 0 & is.na(nba$countDaysNextGameTeam.own) == F, TRUE  , FALSE)
nba$isB2B.opp <- ifelse(nba$isB2BFirst.opp == T | nba$isB2BSecond.opp == T , TRUE , FALSE)

# identify needed variables
model_vars_ids <- c("yearSeason","slugSeason","newGameID","idGame","idTeam","slugTeam","idOpp","slugOpponent.own", "slugMatchup", "dateGame")
model_vars_gm <- c("locationGame", "outcomeGame", "teamGameNumNoPost",
                   "numberGameTeamSeason.own", "isB2B.own", "isB2BFirst.own","isB2BSecond.own", "countDaysRestTeam.own", "countDaysNextGameTeam.own",
                   "numberGameTeamSeason.opp", "isB2B.opp", "isB2BFirst.opp","isB2BSecond.opp", "countDaysRestTeam.opp", "countDaysNextGameTeam.opp",
                   "asb_prepost")

model_vars_own <- c("isWin","isLoss","fgmTeam.own", "fgaTeam.own", "fg3mTeam.own", "fg3aTeam.own", "fg2mTeam.own", "fg2aTeam.own", "minutesTeam.own",
                    "ftmTeam.own", "ftaTeam.own", "orebTeam.own", "drebTeam.own", "trebTeam.own", "astTeam.own", "stlTeam.own",
                    "blkTeam.own", "tovTeam.own", "pfTeam.own", "ptsTeam.own", "possessions.own")
                     #, "pace", "offRtg", "defRtg")

model_vars_opp <- c("fgmTeam.opp", "fgaTeam.opp", "fg3mTeam.opp", "fg3aTeam.opp", "fg2mTeam.opp", "fg2aTeam.opp", "minutesTeam.opp",
                    "ftmTeam.opp", "ftaTeam.opp", "orebTeam.opp", "drebTeam.opp", "trebTeam.opp", "astTeam.opp", "stlTeam.opp",
                    "blkTeam.opp", "tovTeam.opp","pfTeam.opp", "ptsTeam.opp", "possessions.opp")
                
model_vars_bet <- c("teamML", "oppML", "teamWinProb", "oppWinProb")


nba19 <- nba[nba$yearSeason > 0 & nba$typeSeason == "Regular Season",]
nba19 <- nba19[,c(model_vars_ids, model_vars_gm, model_vars_own, model_vars_opp, model_vars_bet)]

#nba19 <- nba19[nba19$slugTeam == 'GSW',]

### YTD STATS ###

calc.ytd <- function(df){
  #if(gm_num) < 
  df <- c(NA, cumsum(df[-length(df)]))
  return (df)
}
#test <- with(test,test[order(ID,YEAR_VISIT),])

nba19.agg <- setDT(nba19)     # converts test to a data.table in place
nba_yearend <- setDT(nba19)
setkey(nba19.agg,idTeam,idGame)
setkey(nba_yearend,idTeam,idGame)

metrics <- c(model_vars_own,model_vars_opp)

#View(nba19.agg[,c(2,3,36,55,79,98)])

# Year-to-date totals
for (i in 1:length(metrics)){
  nba19.agg[,paste0(metrics[i],".agg"):=as.numeric(calc.ytd(get(metrics[i]))),
            by=list(idTeam, slugSeason)]
}
nba19.agg <- data.frame(nba19)

# find end of season stats
for (j in 1:length(metrics)){
  nba_yearend[,paste0(metrics[j],".agg"):=as.numeric(cumsum(get(metrics[j]))),
            by=list(idTeam, slugSeason)]
}
nba_yearend <- data.frame(nba_yearend)


# get last game of each team's season
nba_yearend <- nba_yearend %>%
  group_by(slugSeason, idTeam) %>%
  mutate(lastgame = order(order(idGame, decreasing=TRUE)))
nba_yearend <- nba_yearend[nba_yearend$lastgame == 1,]
nba_yearend <- nba_yearend %>% select(-lastgame)

# merge data of end of season and start of season
allvars <- names(nba19)  
nba_nas <- nba19.agg[(nba19.agg$teamGameNumNoPost == 1 & is.na(nba19.agg$isWin.agg)),]
nba_yearend$yearSeason <- nba_yearend$yearSeason+1
nba_joined <- left_join(nba_nas , nba_yearend , by = c("yearSeason" = "yearSeason", c("idTeam" = "idTeam")), suffix = c(".na", ".ye"))
junk<-nba_joined[,c("yearSeason","idTeam",
                    paste0(model_vars_ids[-c(1,5)],".na"),
                    paste0(model_vars_gm,".na"),
                    paste0(metrics,".na"),
                    paste0(model_vars_bet,".na"),
                    paste0(metrics,".agg",".ye"))]
junk <- junk[,c(1,3,4,5,2,6:length(junk))]
names(junk) <- allvars

# add full dataset to merged year-end and year-start
metrics.agg <- paste0(metrics[3:length(metrics)],".agg")
new_nba <- nba19.agg[!(nba19.agg$teamGameNumNoPost == 1 & is.na(nba19.agg$isWin.agg)),] #data.frame(nba19)
new_nba <- rbind(new_nba,junk)
new_nba <- arrange(new_nba, idGame, idTeam)

for (i in 1:length(metrics.agg)){
  if (i <= 19){
    myvars <- c(metrics.agg[i],"possessions.own.agg")
    new_nba[, paste0(metrics.agg[i],".per100")] <- apply(new_nba[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }else {
    myvars <- c(metrics.agg[i],"possessions.opp.agg")
    new_nba[, paste0(metrics.agg[i],".per100")] <- apply(new_nba[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }
}

new_nba$fgpct.own.agg <- new_nba$fgmTeam.own.agg/new_nba$fgaTeam.own.agg
new_nba$fg3pct.own.agg <- new_nba$fg3mTeam.own.agg/new_nba$fg3aTeam.own.agg
new_nba$fg2pct.own.agg <- new_nba$fg2mTeam.own.agg/new_nba$fg2aTeam.own.agg
new_nba$fgpct.opp.agg <- new_nba$fgmTeam.opp.agg/new_nba$fgaTeam.opp.agg
new_nba$fg3pct.opp.agg <- new_nba$fg3mTeam.opp.agg/new_nba$fg3aTeam.opp.agg
new_nba$fg2pct.opp.agg <- new_nba$fg2mTeam.opp.agg/new_nba$fg2aTeam.opp.agg
new_nba$pace <- apply(new_nba[, c("possessions.own.agg","possessions.opp.agg","minutesTeam.own.agg")], 1, function(x) calc_pace(x[1],x[2],x[3]) )
new_nba$net_rtg <- new_nba$ptsTeam.own.agg.per100 - new_nba$ptsTeam.opp.agg.per100
new_nba$winpct <- new_nba$isWin.agg / (new_nba$isWin.agg + new_nba$isLoss.agg)
#new_nba$winpct <- ifelse(nba19$numberGameTeamSeason.own <= 5, new_nba$isWin.agg / (nba19$numberGameTeamSeason.own-1), new_nba$isWin.agg / (5))

model_vars_per100 <- c("fgmTeam.own.agg.per100","fgaTeam.own.agg.per100","fg3mTeam.own.agg.per100","fg3aTeam.own.agg.per100", "fg2mTeam.own.agg.per100", "fg2aTeam.own.agg.per100",
  "minutesTeam.own.agg.per100", "ftmTeam.own.agg.per100","ftaTeam.own.agg.per100", "orebTeam.own.agg.per100", "drebTeam.own.agg.per100", "trebTeam.own.agg.per100",
  "astTeam.own.agg.per100","stlTeam.own.agg.per100","blkTeam.own.agg.per100","tovTeam.own.agg.per100","pfTeam.own.agg.per100", "ptsTeam.own.agg.per100","possessions.own.agg.per100",
  "fgmTeam.opp.agg.per100","fgaTeam.opp.agg.per100", "fg3mTeam.opp.agg.per100", "fg3aTeam.opp.agg.per100", "fg2mTeam.opp.agg.per100", "fg2aTeam.opp.agg.per100",
  "minutesTeam.opp.agg.per100", "ftmTeam.opp.agg.per100","ftaTeam.opp.agg.per100","orebTeam.opp.agg.per100", "drebTeam.opp.agg.per100", "trebTeam.opp.agg.per100",
  "astTeam.opp.agg.per100","stlTeam.opp.agg.per100", "blkTeam.opp.agg.per100","tovTeam.opp.agg.per100","pfTeam.opp.agg.per100", "ptsTeam.opp.agg.per100",
  "possessions.opp.agg.per100", "fgpct.own.agg", "fg3pct.own.agg", "fg2pct.own.agg", "fgpct.opp.agg", "fg3pct.opp.agg", "fg2pct.opp.agg",
  "pace", "net_rtg", "isWin.agg", "isLoss.agg", "winpct")

nba19_to_model <- new_nba[,c(model_vars_ids, model_vars_gm, model_vars_bet, model_vars_per100)]
nba19_to_model <- arrange(nba19_to_model, idGame, idTeam)

# randomize home & away teams
set.seed(2020)
nba19_to_model$randNum <- runif(nrow(nba19_to_model), 1, 10000)

# add in random select ID
nba19_to_model <- nba19_to_model %>%
  group_by(idGame) %>%
  mutate(randSelect = order(randNum))

# convert data from per-game to per-matchup
nba_matchup <- left_join( nba19_to_model[nba19_to_model$randSelect == 1, -1] ,
                          nba19_to_model[nba19_to_model$randSelect == 2, -c(1,2,3,6,7,8,9,10)],
                          by = c("idGame" = "idGame"), suffix = c(".team1",".team2"))
names(nba_matchup)[7] <- "slugOpp"

# nba_matchup <- left_join( nba19_to_model[nba19_to_model$locationGame == 'H', -1] , 
#                           nba19_to_model[nba19_to_model$locationGame == 'A', -c(1,2,3,6,7,8)], 
#                           by = c("idGame" = "idGame"), suffix = c(".home",".away"))
# names(nba_matchup)[7] <- "slugOpp"

# remove rand columns
nba_matchup <- nba_matchup %>% select(-randNum.team1, -randSelect.team1,
                                      -randNum.team2, -randSelect.team2)

#nba_matchup <- nba_matchup[nba_matchup$locationGame == 'H',]
#nba_matchup <- nba_matchup[nba_matchup$idOpp != nba_matchup$idTeam.home,]
#View(nba19_to_model[nba19_to_model$teamGameNumNoPost==65,])
#View(nba19_to_model[nba19_to_model$slugTeam=="GSW",])

#write.csv(nba_matchup , "data/cleaned-data/new_nba_ytd_matchup.csv", row.names = F)


### X-GAME AGG STATS ###

calc.movagg <- function(bp,n=2){
  #if(is.na(bp[1])) bp[1] <- sum(bp,na.rm=TRUE)
  #bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(NA)
  c(rep(NA,n), rollapply(bp,
                         width=n+1,
                         FUN = function(x) sum(x[-(n+1)], na.rm=T),
                         align="right"))
}

nba_xgms <- setDT(nba19)     # converts test to a data.table in place
#nba_yearend <- setDT(nba19)
setkey(nba_xgms,idTeam,idGame)
setkey(nba_yearend,idTeam,idGame)

metrics <- c(model_vars_own,model_vars_opp)

# X-game totals
for (i in 1:length(metrics)){
  nba_xgms[,paste0(metrics[i],".agg"):=as.numeric(calc.movagg(get(metrics[i]),5)),
            by=list(idTeam)][order(idGame)]
}
new_nba <- data.frame(nba_xgms)

# add in per-possessions data
metrics.agg <- paste0(metrics[3:length(metrics)],".agg")

for (i in 1:length(metrics.agg)){
  if (i <= 19){
    myvars <- c(metrics.agg[i],"possessions.own.agg")
    new_nba[, paste0(metrics.agg[i],".per100")] <- apply(new_nba[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }else {
    myvars <- c(metrics.agg[i],"possessions.opp.agg")
    new_nba[, paste0(metrics.agg[i],".per100")] <- apply(new_nba[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }
}

new_nba$fgpct.own.agg <- new_nba$fgmTeam.own.agg/new_nba$fgaTeam.own.agg
new_nba$fg3pct.own.agg <- new_nba$fg3mTeam.own.agg/new_nba$fg3aTeam.own.agg
new_nba$fg2pct.own.agg <- new_nba$fg2mTeam.own.agg/new_nba$fg2aTeam.own.agg
new_nba$fgpct.opp.agg <- new_nba$fgmTeam.opp.agg/new_nba$fgaTeam.opp.agg
new_nba$fg3pct.opp.agg <- new_nba$fg3mTeam.opp.agg/new_nba$fg3aTeam.opp.agg
new_nba$fg2pct.opp.agg <- new_nba$fg2mTeam.opp.agg/new_nba$fg2aTeam.opp.agg
new_nba$pace <- apply(new_nba[, c("possessions.own.agg","possessions.opp.agg","minutesTeam.own.agg")], 1, function(x) calc_pace(x[1],x[2],x[3]) )
new_nba$net_rtg <- new_nba$ptsTeam.own.agg.per100 - new_nba$ptsTeam.opp.agg.per100
new_nba$winpct <- new_nba$isWin.agg / (new_nba$isWin.agg + new_nba$isLoss.agg)
#new_nba$winpct <- ifelse(nba19$numberGameTeamSeason.own <= 5, new_nba$isWin.agg / (nba19$numberGameTeamSeason.own-1), new_nba$isWin.agg / (5))

model_vars_per100 <- c("fgmTeam.own.agg.per100","fgaTeam.own.agg.per100","fg3mTeam.own.agg.per100","fg3aTeam.own.agg.per100", "fg2mTeam.own.agg.per100", "fg2aTeam.own.agg.per100",
                       "minutesTeam.own.agg.per100", "ftmTeam.own.agg.per100","ftaTeam.own.agg.per100", "orebTeam.own.agg.per100", "drebTeam.own.agg.per100", "trebTeam.own.agg.per100",
                       "astTeam.own.agg.per100","stlTeam.own.agg.per100","blkTeam.own.agg.per100","tovTeam.own.agg.per100","pfTeam.own.agg.per100", "ptsTeam.own.agg.per100","possessions.own.agg.per100",
                       "fgmTeam.opp.agg.per100","fgaTeam.opp.agg.per100", "fg3mTeam.opp.agg.per100", "fg3aTeam.opp.agg.per100", "fg2mTeam.opp.agg.per100", "fg2aTeam.opp.agg.per100",
                       "minutesTeam.opp.agg.per100", "ftmTeam.opp.agg.per100","ftaTeam.opp.agg.per100","orebTeam.opp.agg.per100", "drebTeam.opp.agg.per100", "trebTeam.opp.agg.per100",
                       "astTeam.opp.agg.per100","stlTeam.opp.agg.per100", "blkTeam.opp.agg.per100","tovTeam.opp.agg.per100","pfTeam.opp.agg.per100", "ptsTeam.opp.agg.per100",
                       "possessions.opp.agg.per100", "fgpct.own.agg", "fg3pct.own.agg", "fg2pct.own.agg", "fgpct.opp.agg", "fg3pct.opp.agg", "fg2pct.opp.agg",
                       "pace", "net_rtg", "isWin.agg", "isLoss.agg", "winpct")

nba19_to_model <- new_nba[,c(model_vars_ids, model_vars_gm, model_vars_bet, model_vars_per100)]
nba19_to_model <- arrange(nba19_to_model, idGame, idTeam)

# randomize home & away teams
set.seed(2020)
nba19_to_model$randNum <- runif(nrow(nba19_to_model), 1, 10000)

# add in random select ID
nba19_to_model <- nba19_to_model %>%
  group_by(idGame) %>%
  mutate(randSelect = order(randNum))

# convert data from per-game to per-matchup
nba_matchup <- left_join( nba19_to_model[nba19_to_model$randSelect == 1, -1] ,
                          nba19_to_model[nba19_to_model$randSelect == 2, -c(1,2,3,6,7,8,9,10)],
                          by = c("idGame" = "idGame"), suffix = c(".team1",".team2"))
names(nba_matchup)[7] <- "slugOpp"

# nba_matchup <- left_join( nba19_to_model[nba19_to_model$locationGame == 'H', -1] , 
#                           nba19_to_model[nba19_to_model$locationGame == 'A', -c(1,2,3,6,7,8)], 
#                           by = c("idGame" = "idGame"), suffix = c(".home",".away"))
# names(nba_matchup)[7] <- "slugOpp"

# remove rand columns
nba_matchup <- nba_matchup %>% select(-randNum.team1, -randSelect.team1,
                                      -randNum.team2, -randSelect.team2)

#write.csv(nba_matchup , "data/cleaned-data/new_nba_xgm_matchup.csv", row.names = F)


