#### nbastatR ####

#devtools::install_github("abresler/nbastatR")
#install.packages("nbastatR" , dependencies = T)
#install.packages('rlang')
setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data/")
library(nbastatR)
library("dplyr")
library("tidyr")

teamIDs <- read.csv("id-mapping/nbastatr_to_id_map.csv", stringsAsFactors = F)

## get 2007 season schedule
s07_gamelog <- nbastatR::game_logs(seasons = 2008, league = "NBA", result_types = "team",
                                   season_types = "Regular Season", nest_data = F,
                                   assign_to_environment = TRUE, return_message = TRUE)

# regular & postseason games from 2007-08 - 2018-19
gamelogs_0719 <- nbastatR::game_logs(seasons = c(2008:2019) , league = "NBA", result_types = "team",
                                                  season_types = c("Regular Season", "Playoffs") , nest_data = F,
                                                  assign_to_environment = TRUE, return_message = TRUE)

# regular season games from 2019-20
gamelogs_1920 <- nbastatR::game_logs(seasons = 2020 , league = "NBA", result_types = "team",
                    season_types = "Regular Season" , nest_data = F,
                    assign_to_environment = TRUE, return_message = TRUE)

gamelogs_0720 <- as.data.frame(rbind(gamelogs_0719, gamelogs_1920))

# get advanced box scores
nbaStatRgameIDs08 <- unique(gamelogs_0720[gamelogs_0720$yearSeason == 2008,]$idGame)
nbaStatRgameIDs12 <- unique(gamelogs_0720[gamelogs_0720$yearSeason == 2012,]$idGame)
boxes <- c()
for (i in 1:length(nbaStatRgameIDs12)){
  n <- box_scores(game_ids = nbaStatRgameIDs12[i], box_score_types = 'Advanced',
                  result_types = 'team', join_data = F, assign_to_environment = F, return_message = T) %>%
        unnest() %>%
        select("idGame","idTeam","possessions")
  boxes <- rbind(boxes,n)
}
#write.csv(boxes,"adv_0708.csv", row.names = F)
#write.csv(boxes,"adv_1112.csv", row.names = F)

#gamelogs_0720_box <- left_join( gamelogs_0720 , boxes[,c(1,3,28)], by = c("idGame" = "idGame", "idTeam" = "idTeam"))

# add in team and opponent ID
#gamelogs_0720_id <- left_join( gamelogs_0720 , gamelogs_0720[ , c("idGame","idTeam")], by = c("idGame" = "idGame") )
#gamelogs_0720_id <- gamelogs_0720_id[gamelogs_0720_id$idTeam.x != gamelogs_0720_id$idTeam.y , ]
#names(gamelogs_0720_id)[c(9,48)] <- c("idTeam", "idOpp")

# add in team and opponent stats
gamelogs_0720_id <- left_join( gamelogs_0720 , gamelogs_0720[ , c(6,7,8,9,10,11,12,16,17,18,23:29,31:47)], by = c("idGame" = "idGame"), suffix = c(".own", ".opp"))
gamelogs_0720_id <- gamelogs_0720_id[gamelogs_0720_id$idTeam.own != gamelogs_0720_id$idTeam.opp , ]
names(gamelogs_0720_id)[c(9,50)] <- c("idTeam", "idOpp")

gamelogs_0720_id <- left_join( gamelogs_0720_id , teamIDs[, c("Team.ID","nbaStatR")] , by = c("idTeam"="nbaStatR") )
gamelogs_0720_id <- left_join( gamelogs_0720_id , teamIDs[, c("Team.ID","nbaStatR")] , by = c("idOpp"="nbaStatR") )
names(gamelogs_0720_id)[c(length(names(gamelogs_0720_id))-1,length(names(gamelogs_0720_id)))] <- c("teamID", "oppID")


# create common game ID
gamelogs_0720_id$seasonID <- as.numeric(gsub("-", "", gamelogs_0720$slugSeason))
gamelogs_0720_id$dateID <- format(gamelogs_0720$dateGame, '%m%d')


# add leading 0s
gamelogs_0720_id$dateID <- sapply( gamelogs_0720_id$dateID , function(x) ifelse( nchar(x) > 3 , x , paste0(0,x) ) )
gamelogs_0720_id$teamIDchar <- sapply( gamelogs_0720_id$teamID , function(x) ifelse( nchar(x) == 2 , x , paste0(0,x) ) )
gamelogs_0720_id$oppIDchar <- sapply( gamelogs_0720_id$oppID , function(x) ifelse( nchar(x) == 2 , x , paste0(0,x) ) )


gamelogs_0720_id$newGameID <- paste0( gamelogs_0720_id$seasonID , "_" ,
                                      gamelogs_0720_id$dateID , "_" ,
                                      ifelse( gamelogs_0720_id$locationGame == 'A' , gamelogs_0720_id$teamIDchar , gamelogs_0720_id$oppIDchar ),  "_" ,
                                      ifelse( gamelogs_0720_id$locationGame == 'A' , gamelogs_0720_id$oppIDchar , gamelogs_0720_id$teamIDchar )
)


# export data
write.csv(gamelogs_0720_id, "nba_gamelogs_clean_with_opps_050320.csv", na = "", row.names = F)


# get all star games
# library(dplyr)
# library(nbastatR)
# df_asg <-
#   all_star_games()
# df_asg %>% glimpse()
# df_asg %>% count(namePlayerMVP, sort = T)

######  old  #####
s07 <- nbastatR::seasons_schedule(seasons = 2008, box_score_tables = 'Traditional')


s07 <- nbastatR::seasons_schedule(seasons = 2008, season_types = "Regular Season",
                                  parse_boxscores = F, box_score_tables = c("Traditional"),
                                  nest_data = FALSE, return_message = TRUE)

teams <- nbastatR::nba_teams_seasons()

all_star_games(include_aba = F, return_message = T)
game_logs(seasons = 2019, result_types = c("team", "player"))
bref_teams_stats()
teams <- nba_teams()

xyz <- nbastatR::players_careers("Stephen Curry")

View(xyz[1,]$dataTable[[1]])

abc <-  nbastatR::game_logs(seasons = 2019, result_types = "team")