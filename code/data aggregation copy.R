## data aggregation steps ##

setwd("~/Documents/UCLA MAS/Thesis/repo-backup/uclathesis/")
library("dplyr")
library("ggplot2")
library("data.table")
library("zoo")
#options(scipen = 99999)

nba <- read.csv("data/nba_adv.csv", stringsAsFactors = F, na.strings = "")

# add in all-star break variable
asg <- read.csv("data/id-mapping/asg.csv", stringsAsFactors = F, na.strings = "")
asg$Date <- as.Date(asg$Date)
nba$dateGame <- as.Date(nba$dateGame)
nba <- left_join( nba , asg[,c(1,4)], by = c("slugSeason" = "slugSeason"))
nba$asb_prepost <- ifelse(nba$dateGame < nba$Date , "pre", "post")

# identify needed variables
model_vars_ids <- c("slugSeason","newGameID","idGame","idTeam","slugTeam","idOpp","slugOpponent.own")
model_vars_gm <- c("locationGame", "outcomeGame", "countDaysRestTeam.own","teamGameNumNoPost", "numberGameTeamSeason.own", "countDaysNextGameTeam.own",
                   "numberGameTeamSeason.opp","countDaysRestTeam.opp","countDaysNextGameTeam.opp", "asb_prepost")

model_vars_own <- c("isWin","fgmTeam.own", "fgaTeam.own", "fg3mTeam.own", "fg3aTeam.own", "fg2mTeam.own", "fg2aTeam.own", "minutesTeam.own",
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

calc.magg <- function(bp,n=2){
  if(is.na(bp[1])) bp[1] <- sum(bp,na.rm=TRUE)
  bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(bp)
  c(bp[1:(n)], rollapply(bp,
                           width=n+1,
                           FUN = function(x) sum(x[-(n+1)], na.rm=T),
                           align="right"))
}

calc.ytd <- function(df){
  df <- c(0, cumsum(df[-length(df)]))
  return (df)
}
#test <- with(test,test[order(ID,YEAR_VISIT),])

nba19.agg <- setDT(nba19)     # converts test to a data.table in place
setkey(nba19.agg,idTeam,teamGameNumNoPost)

metrics <- c(model_vars_own,model_vars_opp)

# X-game totals
for (i in 1:length(metrics)){
  nba19.agg[,paste0(metrics[i],".agg"):=as.numeric(calc.magg(get(metrics[i]),5)),
                 by=list(idTeam, slugSeason)][order(idGame)]
}

# Year-to-date totals
# for (i in 1:length(metrics)){
#   nba19.agg[,paste0(metrics[i],".agg"):=as.numeric(calc.ytd(get(metrics[i]))),
#             by=list(idTeam, slugSeason)]
# }

metrics.agg <- paste0(metrics[2:length(metrics)],".agg")
nba19.agg <- data.frame(nba19)

for (i in 1:length(metrics.agg)){
  if (i <= 19){
    myvars <- c(metrics.agg[i],"possessions.own.agg")
    nba19.agg[, paste0(metrics.agg[i],".per100")] <- apply(nba19.agg[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }else {
    myvars <- c(metrics.agg[i],"possessions.opp.agg")
    nba19.agg[, paste0(metrics.agg[i],".per100")] <- apply(nba19.agg[, myvars], 1, function(x) calc_per100(x[1],x[2]) )
  }
}

nba19.agg$fgpct.own.agg <- nba19.agg$fgmTeam.own.agg/nba19.agg$fgaTeam.own.agg
nba19.agg$fg3pct.own.agg <- nba19.agg$fg3mTeam.own.agg/nba19.agg$fg3aTeam.own.agg
nba19.agg$fg2pct.own.agg <- nba19.agg$fg2mTeam.own.agg/nba19.agg$fg2aTeam.own.agg
nba19.agg$fgpct.opp.agg <- nba19.agg$fgmTeam.opp.agg/nba19.agg$fgaTeam.opp.agg
nba19.agg$fg3pct.opp.agg <- nba19.agg$fg3mTeam.opp.agg/nba19.agg$fg3aTeam.opp.agg
nba19.agg$fg2pct.opp.agg <- nba19.agg$fg2mTeam.opp.agg/nba19.agg$fg2aTeam.opp.agg
nba19.agg$pace <- calc_pace(own_poss = nba19.agg$possessions.own.agg , opp_poss = nba19.agg$possessions.opp.agg, min = nba19.agg$minutesTeam.opp.agg)
nba19.agg$net_rtg <- nba19.agg$ptsTeam.own.agg.per100 - nba19.agg$ptsTeam.opp.agg.per100
nba19.agg$winpct <- nba19.agg$isWin.agg / (nba19.agg$numberGameTeamSeason.own - 1)
#nba19.agg$winpct <- ifelse(nba19$numberGameTeamSeason.own <= 5, nba19.agg$isWin.agg / (nba19$numberGameTeamSeason.own-1), nba19.agg$isWin.agg / (5))

model_vars_per100 <- c("fgmTeam.own.agg.per100","fgaTeam.own.agg.per100","fg3mTeam.own.agg.per100","fg3aTeam.own.agg.per100", "fg2mTeam.own.agg.per100", "fg2aTeam.own.agg.per100",
  "minutesTeam.own.agg.per100", "ftmTeam.own.agg.per100","ftaTeam.own.agg.per100", "orebTeam.own.agg.per100", "drebTeam.own.agg.per100", "trebTeam.own.agg.per100",
  "astTeam.own.agg.per100","stlTeam.own.agg.per100","blkTeam.own.agg.per100","tovTeam.own.agg.per100","pfTeam.own.agg.per100", "ptsTeam.own.agg.per100","possessions.own.agg.per100",
  "fgmTeam.opp.agg.per100","fgaTeam.opp.agg.per100", "fg3mTeam.opp.agg.per100", "fg3aTeam.opp.agg.per100", "fg2mTeam.opp.agg.per100", "fg2aTeam.opp.agg.per100",
  "minutesTeam.opp.agg.per100", "ftmTeam.opp.agg.per100","ftaTeam.opp.agg.per100","orebTeam.opp.agg.per100", "drebTeam.opp.agg.per100", "trebTeam.opp.agg.per100",
  "astTeam.opp.agg.per100","stlTeam.opp.agg.per100", "blkTeam.opp.agg.per100","tovTeam.opp.agg.per100","pfTeam.opp.agg.per100", "ptsTeam.opp.agg.per100",
  "possessions.opp.agg.per100", "fgpct.own.agg", "fg3pct.own.agg", "fg2pct.own.agg", "fgpct.opp.agg", "fg3pct.opp.agg", "fg2pct.opp.agg", "pace", "net_rtg")

nba19_to_model <- nba19.agg[,c(model_vars_ids, model_vars_gm, model_vars_bet, model_vars_per100)]
nba19_to_model <- arrange(nba19_to_model, idGame, idTeam)


# convert data from per-game to per-matchup
nba_matchup <- left_join( nba19_to_model[nba19_to_model$locationGame == 'H',] , 
                          nba19_to_model[nba19_to_model$locationGame == 'A', -c(1,2,5,6,7)], 
                          by = c("idGame" = "idGame"), suffix = c(".home",".away"))
names(nba_matchup)[7] <- "slugOpp"
#nba_matchup <- nba_matchup[nba_matchup$locationGame == 'H',]
#nba_matchup <- nba_matchup[nba_matchup$idOpp != nba_matchup$idTeam.home,]

#View(nba19_to_model[nba19_to_model$teamGameNumNoPost==65,])

write.csv(nba_matchup , "data/cleaned-data/nba_5gm_matchup.csv", row.names = F)
#write.csv(nba_matchup , "data/cleaned-data/nba_ytd_matchup.csv", row.names = F)
