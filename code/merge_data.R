## merge data ##

setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data/")
library("dplyr")
library("ggplot2")
#options(scipen = 99999)

odds <- read.csv("cleaned-data/nba_odds_clean.csv", stringsAsFactors = F, na.strings = "")
logs <- read.csv("cleaned-data/nba_gamelogs_clean.csv", stringsAsFactors = F, na.strings = "")

fullNBA <- left_join( logs , odds[ , c("newGameID", "teamID", "ML") ] , by = c("newGameID"="newGameID", "teamID" = "teamID") )
fullNBA <- left_join( fullNBA , odds[ , c("newGameID", "teamID", "ML") ] , by = c("newGameID"="newGameID", "oppID" = "teamID") )
names(fullNBA)[c(56,57)] <- c("teamML", "oppML")

#apply(fullNBA, 2, function(x) length(which(is.na(x))))
#View(fullNBA[is.na(fullNBA$ML), ])

# rank team game number per season
fullNBA <- fullNBA %>%
                group_by(seasonID, teamID) %>%
                mutate(teamGameNum = order(order(dateGame, decreasing=FALSE)))

#fullNBA2 <- fullNBA[!is.na(fullNBA$teamML),]

# add win probability
fullNBA$teamWinProb <- apply(fullNBA[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$team )
fullNBA$oppWinProb <- apply(fullNBA[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$opp )

write.csv(fullNBA, "nba.csv", row.names = F)