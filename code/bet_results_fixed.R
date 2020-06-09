## Sample betting results for fixed bet methods ##

library(ggplot2)
library(reshape2)
library(dplyr)

setwd("~/Documents/UCLA MAS/Thesis/repo-local/uclathesis/data/")
options(scipen = 99999)

nba <- read.csv("predictions/best-preds-ytd.csv", stringsAsFactors = F)

### DATA PREP
{
nba$dateGame <- as.Date(nba$dateGame)
nba <- nba %>% arrange(dateGame, idTeam.team1)

#nba <- nba[(nba$teamGameNumNoPost.team1 >= 8 & nba$teamGameNumNoPost.team2 >= 8),]

testNBA <- nba %>% dplyr::select(slugSeason, idGame, dateGame, slugMatchup,
                          idTeam.team1, slugTeam, locationGame.team1, outcomeGame.team1, teamML.team1, teamWinProb.team1, 
                          idOpp, slugOpp, outcomeGame.team2, teamML.team2, teamWinProb.team2,
                          logitPredL, logitPredW)

#testNBA <- testNBA[testNBA$slugSeason == '2019-20', ]

### model prediction to test betting results ###
testNBA$modelPred <- testNBA$logitPredW

# add fake model prediction
set.seed(2020)
testNBA$randPred <- round(runif(nrow(testNBA), 1, 2),0)
# table(testNBA$randPred)
}

##### FIXED BET AMOUNT - MODEL EDGE #####
{
fixedBet <- 10
testNBA$betPayout.team1 <- sapply(testNBA$teamML.team1, function(x) ml_pay(x , fixedBet))
testNBA$betPayout.team2 <- sapply(testNBA$teamML.team2, function(x) ml_pay(x , fixedBet))

# calculate bankroll - model edge
testNBA$betProfit.edge <- NA

testNBA$betProfit.edge <- ifelse( (testNBA$modelPred > testNBA$teamWinProb.team1) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.edge <- ifelse( (testNBA$modelPred > testNBA$teamWinProb.team1) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.edge)
testNBA$betProfit.edge <- ifelse( (testNBA$modelPred < testNBA$teamWinProb.team1) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.edge)
testNBA$betProfit.edge <- ifelse( (testNBA$modelPred < testNBA$teamWinProb.team1) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.edge)

testNBA$bankAmount.edge <- cumsum(testNBA$betProfit.edge)
}

##### FIXED BET AMOUNT - MODEL FAVORITE #####
{
# calculate bankroll - favorites
testNBA$betProfit.fav <- NA

testNBA$betProfit.fav <- ifelse( (testNBA$modelPred > .5) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred > .5) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.fav)
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred < .5) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.fav)
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred < .5) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.fav)

testNBA$bankAmount.fav <- cumsum(testNBA$betProfit.fav)
}

##### FIXED BET AMOUNT - COMBO #####
{
# calculate bankroll - combo
testNBA$betProfit.combo <- NA

testNBA$betProfit.combo <- ifelse( (testNBA$modelPred > .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.combo <- ifelse( (testNBA$modelPred > .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.combo)
testNBA$betProfit.combo <- ifelse( (testNBA$modelPred < .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team2 == 'W',  0, testNBA$betProfit.combo)
testNBA$betProfit.combo <- ifelse( (testNBA$modelPred < .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team2 == 'L', 0, testNBA$betProfit.combo)

testNBA$bankAmount.combo <- cumsum(testNBA$betProfit.combo)
}

##### FIXED BET AMOUNT - ODDS FAV #####
{
# calculate bankroll - odds favorite
testNBA$betProfit.oddsfav <- NA

testNBA$betProfit.oddsfav <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.oddsfav <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.oddsfav)
testNBA$betProfit.oddsfav <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.oddsfav)
testNBA$betProfit.oddsfav <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.oddsfav)

testNBA$bankAmount.oddsfav <- cumsum(testNBA$betProfit.oddsfav)
}

##### FIXED BET AMOUNT - ODDS DOG #####
{
# calculate bankroll - odds favorite
testNBA$betProfit.oddsdog <- NA

testNBA$betProfit.oddsdog <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.oddsdog <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.oddsdog)
testNBA$betProfit.oddsdog <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.oddsdog)
testNBA$betProfit.oddsdog <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.oddsdog)

testNBA$bankAmount.oddsdog <- cumsum(testNBA$betProfit.oddsdog)
}

##### FIXED BET AMOUNT - RANDOM #####
{
# calculate bankroll - odds favorite
testNBA$betProfit.rand <- NA

testNBA$betProfit.rand <- ifelse( (testNBA$randPred == 1) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.rand <- ifelse( (testNBA$randPred == 1) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.rand)
testNBA$betProfit.rand <- ifelse( (testNBA$randPred == 2) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.rand)
testNBA$betProfit.rand <- ifelse( (testNBA$randPred == 2) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.rand)

testNBA$bankAmount.rand <- cumsum(testNBA$betProfit.rand)
}

##### COMBINED GRAPHS #####
{
# add row numbers
testNBA$rownums <- 1:nrow(testNBA)
graphdata <- testNBA %>% dplyr::select(dateGame, bankAmount.edge, bankAmount.fav, bankAmount.combo, bankAmount.oddsfav, bankAmount.oddsdog, bankAmount.rand)
names(graphdata) <- c("dateGame","Model Edge", "Model Favorite","Model Combo", "Odds Favorite", "Odds Underdog", "Random")
#graphdata$dateGame <- as.Date(graphdata$dateGame)

# graph of bankroll over time
d <- melt(graphdata, id.vars="dateGame")
ggplot(d, aes(dateGame,value, col=variable)) + 
  geom_line(size = 1.5, alpha = 0.7) +
  scale_x_date(date_breaks = "months" , date_labels = "%b '%y", limits = c(as.Date('2019-10-22'),as.Date('2020-04-01'))) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5)) +
  xlab("")+
  ylab("Bankroll ($)") +
  labs(title = "Total Bankroll Over Time using Various Betting Strategies", subtitle = "(2019-20 NBA Season)") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  annotate(geom="text", label="Starting Bankroll ($0)", 
           x= as.Date('2020-03-25'), size = 3, y=5, vjust=-1) +
scale_color_manual(values = c("#00CFFF", "#FFD100", "#005587", "#616A6B","#8BB8E8", "#FFA500"))
}

##### SUMMARY STATS #####
{
summ <- testNBA
summ_df <- data.frame("Model" = c("Model Edge", "Model Favorite","Model Combo", "Odds Favorite", "Odds Underdog", "Random"),
                      "Win" = rep(NA,6),
                      "Loss" = rep(NA,6),
                      "No.Bet" = rep(NA,6),
                      "Win Pct" = rep(NA,6),
                      "Final.Bankroll" = rep(NA,6),
                      "ROI" = rep(NA,6))

# edge
summ$bet_type.edge <- ifelse(summ$betProfit.edge > 0 , "win", "loss")
#[summ$betProfit.edge == 0,]$bet_type.edge <- "no bet"
summ_df[1,2] <- sum(summ$bet_type.edge == 'win')
summ_df[1,3] <- sum(summ$bet_type.edge == 'loss')
summ_df[1,4] <- sum(summ$bet_type.edge == 'no bet')
summ_df[1,5] <- round(summ_df[1,2] / (summ_df[1,2]+summ_df[1,3]),3)
summ_df[1,6] <- summ$bankAmount.edge[nrow(summ)]
summ_df[1,7] <- round(summ$bankAmount.edge[nrow(summ)]/(10 * (summ_df[1,2]+summ_df[1,3])),3)

# fav
summ$bet_type.fav <- ifelse(summ$betProfit.fav > 0 , "win", "loss")
#summ[summ$betProfit.fav == 0,]$bet_type.fav <- "no bet"
summ_df[2,2] <- sum(summ$bet_type.fav == 'win')
summ_df[2,3] <- sum(summ$bet_type.fav == 'loss')
summ_df[2,4] <- sum(summ$bet_type.fav == 'no bet')
summ_df[2,5] <- round(summ_df[2,2] / (summ_df[2,2]+summ_df[2,3]),3)
summ_df[2,6] <- summ$bankAmount.fav[nrow(summ)]
summ_df[2,7] <- round(summ$bankAmount.fav[nrow(summ)]/(10 * (summ_df[2,2]+summ_df[2,3])),3)

# combo
summ$bet_type.combo <- ifelse(summ$betProfit.combo > 0 , "win", "loss")
summ[summ$betProfit.combo == 0,]$bet_type.combo <- "no bet"
summ_df[3,2] <- sum(summ$bet_type.combo == 'win')
summ_df[3,3] <- sum(summ$bet_type.combo == 'loss')
summ_df[3,4] <- sum(summ$bet_type.combo == 'no bet')
summ_df[3,5] <- round(summ_df[3,2] / (summ_df[3,2]+summ_df[3,3]),3)
summ_df[3,6] <- summ$bankAmount.combo[nrow(summ)]
summ_df[3,7] <- round(summ$bankAmount.combo[nrow(summ)]/(10 * (summ_df[3,2]+summ_df[3,3])),3)

# odds fav
summ$bet_type.oddsfav <- ifelse(summ$betProfit.oddsfav > 0 , "win", "loss")
summ[summ$betProfit.oddsfav == 0,]$bet_type.oddsfav <- "no bet"
summ_df[4,2] <- sum(summ$bet_type.oddsfav == 'win')
summ_df[4,3] <- sum(summ$bet_type.oddsfav == 'loss')
summ_df[4,4] <- sum(summ$bet_type.oddsfav == 'no bet')
summ_df[4,5] <- round(summ_df[4,2] / (summ_df[4,2]+summ_df[4,3]),3)
summ_df[4,6] <- summ$bankAmount.oddsfav[nrow(summ)]
summ_df[4,7] <- round(summ$bankAmount.oddsfav[nrow(summ)]/(10 * (summ_df[4,2]+summ_df[4,3])),3)


# odds dog
summ$bet_type.oddsdog <- ifelse(summ$betProfit.oddsdog > 0 , "win", "loss")
summ[summ$betProfit.oddsdog == 0,]$bet_type.oddsdog <- "no bet"
summ_df[5,2] <- sum(summ$bet_type.oddsdog == 'win')
summ_df[5,3] <- sum(summ$bet_type.oddsdog == 'loss')
summ_df[5,4] <- sum(summ$bet_type.oddsdog == 'no bet')
summ_df[5,5] <- round(summ_df[5,2] / (summ_df[5,2]+summ_df[5,3]),3)
summ_df[5,6] <- summ$bankAmount.oddsdog[nrow(summ)]
summ_df[5,7] <- round(summ$bankAmount.oddsdog[nrow(summ)]/(10 * (summ_df[5,2]+summ_df[5,3])),3)

# random
summ$bet_type.rand <- ifelse(summ$betProfit.rand > 0 , "win", "loss")
#summ[summ$betProfit.rand == 0,]$bet_type.rand <- "no bet"
summ_df[6,2] <- sum(summ$bet_type.rand == 'win')
summ_df[6,3] <- sum(summ$bet_type.rand == 'loss')
summ_df[6,4] <- sum(summ$bet_type.rand == 'no bet')
summ_df[6,5] <- round(summ_df[6,2] / (summ_df[6,2]+summ_df[6,3]),3)
summ_df[6,6] <- summ$bankAmount.rand[nrow(summ)]
summ_df[6,7] <- round(summ$bankAmount.rand[nrow(summ)]/(10 * (summ_df[6,2]+summ_df[6,3])),3)
}
