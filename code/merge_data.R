## merge data ##

setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data/")
library("dplyr")
library("ggplot2")
#options(scipen = 99999)

odds <- read.csv("cleaned-data/nba_odds_clean.csv", stringsAsFactors = F, na.strings = "")
logs <- read.csv("cleaned-data/nba_gamelogs_clean_with_opps_050320.csv", stringsAsFactors = F, na.strings = "")
adv_reg <- read.csv("cleaned-data/seasons_reg_adv.csv", stringsAsFactors = F, na.strings = "")
adv_post <- read.csv("cleaned-data/seasons_post_adv.csv", stringsAsFactors = F, na.strings = "")
adv_0708 <- read.csv("cleaned-data/adv_0708.csv", stringsAsFactors = F, na.strings = "")
adv_1112 <- read.csv("cleaned-data/adv_1112.csv", stringsAsFactors = F, na.strings = "")
  
fullNBA <- left_join( logs , odds[ , c("newGameID", "teamID", "ML") ] , by = c("newGameID"="newGameID", "teamID" = "teamID") )
fullNBA <- left_join( fullNBA , odds[ , c("newGameID", "teamID", "ML") ] , by = c("newGameID"="newGameID", "oppID" = "teamID") )
names(fullNBA)[c(length(names(fullNBA))-1,length(names(fullNBA)))] <- c("teamML", "oppML")

#apply(fullNBA, 2, function(x) length(which(is.na(x))))
#View(fullNBA[is.na(fullNBA$ML), ])

# rank team game number per season
fullNBA <- fullNBA %>%
                group_by(seasonID, teamID) %>%
                mutate(teamGameNumNoPost = order(order(dateGame, decreasing=FALSE)))

# add team possessions
adv <- rbind(adv_reg %>% select("SEASON_YEAR","GAME_ID","TEAM_ID","POSS") , 
             adv_post %>% select("SEASON_YEAR","GAME_ID","TEAM_ID","POSS"))
adv <- adv[(adv$SEASON_YEAR != '2011-12'),]
adv <- adv[(adv$SEASON_YEAR != '2007-08'),]
adv_0708$SEASON_YEAR <- '2007-08'
adv_1112$SEASON_YEAR <- '2011-12'
adv_0708 <- adv_0708[,c(4,1:3)]
adv_1112 <- adv_1112[,c(4,1:3)]
names(adv_0708) <- c("SEASON_YEAR","GAME_ID","TEAM_ID","POSS")
names(adv_1112) <- c("SEASON_YEAR","GAME_ID","TEAM_ID","POSS")
full_adv <- rbind( adv , 
                   adv_0708 ,
                   adv_1112 )

fullNBA_adv <- left_join( fullNBA , full_adv[ , c("TEAM_ID", "GAME_ID", "POSS") ] , by = c("idGame"="GAME_ID", "idTeam" = "TEAM_ID") )
fullNBA_adv <- left_join( fullNBA_adv , full_adv[ , c("TEAM_ID", "GAME_ID", "POSS") ] , by = c("idGame"="GAME_ID", "idOpp" = "TEAM_ID") )
names(fullNBA_adv)[c(length(names(fullNBA_adv))-1, length(names(fullNBA_adv)))] <- c("possessions.own", "possessions.opp")

#fullNBA$possessions.own <- calc_poss( fullNBA$fgaTeam.own, fullNBA$ftaTeam.own, fullNBA$orebTeam.own, fullNBA$drebTeam.own, fullNBA$fgmTeam.own, fullNBA$tovTeam.own,
#                                  fullNBA$fgaTeam.opp, fullNBA$ftaTeam.opp, fullNBA$orebTeam.opp, fullNBA$drebTeam.opp, fullNBA$fgmTeam.opp, fullNBA$tovTeam.opp )
#fullNBA$possessions.opp <- calc_poss( fullNBA$fgaTeam.opp, fullNBA$ftaTeam.opp, fullNBA$orebTeam.opp, fullNBA$drebTeam.opp, fullNBA$fgmTeam.opp, fullNBA$tovTeam.opp,
#                                      fullNBA$fgaTeam.own, fullNBA$ftaTeam.own, fullNBA$orebTeam.own, fullNBA$drebTeam.own, fullNBA$fgmTeam.own, fullNBA$tovTeam.own)

# add team pace
fullNBA_adv$pace <- calc_pace( fullNBA_adv$possessions.own , fullNBA_adv$possessions.opp,  fullNBA_adv$minutesTeam.own )

# add team offensive rating
fullNBA_adv$offRtg <- calc_ortg( fullNBA_adv$ptsTeam.own , fullNBA_adv$possessions.own )

# add team defensive rating
fullNBA_adv$defRtg <- calc_drtg( fullNBA_adv$ptsTeam.opp , fullNBA_adv$possessions.opp )

fullNBAtest <- fullNBA_adv[fullNBA_adv$slugMatchup == 'BOS @ IND',]

# add win probability
fullNBA_adv$teamWinProb <- apply(fullNBA_adv[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$team )
fullNBA_adv$oppWinProb <- apply(fullNBA_adv[,c('teamML','oppML')], 1, function(x) act_prob(x[1],x[2])$opp )

write.csv(fullNBA_adv, "nba_adv.csv", row.names = F, na = "")
