#### nbastatR ####

devtools::install_github("abresler/nbastatR")
#install.packages("nbastatR" , dependencies = T)
library(nbastatR)

all_star_games(include_aba = F, return_message = T)
game_logs(seasons = 2019, result_types = c("team", "player"))
bref_teams_stats()
teams <- nba_teams()

xyz <- nbastatR::players_careers("Stephen Curry")

View(xyz[1,]$dataTable[[1]])

abc <-  nbastatR::game_logs(seasons = 2019, result_types = "team")


