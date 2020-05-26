## Sample betting results for fixed bet methods ##

library(ggplot2)
library(reshape2)

setwd("~/Documents/UCLA MAS/Thesis/repo-backup/uclathesis/data/")
options(scipen = 99999)

nba <- read.csv("cleaned-data/new_nba_ytd_matchup.csv", stringsAsFactors = F)
nba2 <- read.csv("nba_adv_complete.csv", stringsAsFactors = F)

nba$dateGame <- as.Date(nba$dateGame)
nba <- nba %>% arrange(dateGame, idTeam.team1)

# nba <- left_join(nba, nba2 %>% select(idGame, idTeam, dateGame, slugMatchup), by = c("idGame" = "idGame", "idTeam.team1" = "idTeam"))
testNBA <- nba %>% select(slugSeason, idGame, dateGame, slugMatchup,
                          idTeam.team1, slugTeam, locationGame.team1, outcomeGame.team1, teamML.team1, teamWinProb.team1, 
                          idOpp, slugOpp, outcomeGame.team2, teamML.team2, teamWinProb.team2)

#testNBA <- nba[!is.na(nba$teamWinProb),]
testNBA <- testNBA[testNBA$slugSeason == '2018-19', ]

### model prediction to test betting results ###
# add fake model prediction
# testNBA$mlOdds <- sapply(testNBA$teamML, function(x) ml_to_prob(x))
set.seed(2020)
testNBA$modelPred <- sapply(testNBA$teamWinProb.team1, function(x) rnorm(1, x, 0.1 ))
testNBA$modelPred <- ifelse(testNBA$modelPred < 0 , 0.001 , testNBA$modelPred)
testNBA$modelPred <- ifelse(testNBA$modelPred > 1 , 0.999 , testNBA$modelPred)


##### FIXED BET AMOUNT - MODEL EDGE #####
fixedBet <- 10
testNBA$betPayout.team1 <- sapply(testNBA$teamML.team1, function(x) ml_pay(x , fixedBet))
testNBA$betPayout.team2 <- sapply(testNBA$teamML.team2, function(x) ml_pay(x , fixedBet))

# calculate bankroll - model edge
testNBA$betProfit <- NA

testNBA$betProfit <- ifelse( (testNBA$modelPred > testNBA$teamWinProb.team1) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit <- ifelse( (testNBA$modelPred > testNBA$teamWinProb.team1) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit)
testNBA$betProfit <- ifelse( (testNBA$modelPred < testNBA$teamWinProb.team1) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit)
testNBA$betProfit <- ifelse( (testNBA$modelPred < testNBA$teamWinProb.team1) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit)

testNBA$bankAmount <- cumsum(testNBA$betProfit)


##### FIXED BET AMOUNT - MODEL FAVORITE #####
# calculate bankroll - favorites
testNBA$betProfit.fav <- NA

testNBA$betProfit.fav <- ifelse( (testNBA$modelPred > .5) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred > .5) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.fav)
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred < .5) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.fav)
testNBA$betProfit.fav <- ifelse( (testNBA$modelPred < .5) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.fav)

testNBA$bankAmount.fav <- cumsum(testNBA$betProfit.fav)


##### FIXED BET AMOUNT - COMBO #####
# calculate bankroll - combo
testNBA$betProfit.comb <- NA

testNBA$betProfit.comb <- ifelse( (testNBA$modelPred > .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.comb <- ifelse( (testNBA$modelPred > .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.comb)
testNBA$betProfit.comb <- ifelse( (testNBA$modelPred < .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team2 == 'W',  0, testNBA$betProfit.comb)
testNBA$betProfit.comb <- ifelse( (testNBA$modelPred < .5 & (testNBA$modelPred > testNBA$teamWinProb.team1)) & testNBA$outcomeGame.team2 == 'L', 0, testNBA$betProfit.comb)

testNBA$bankAmount.comb <- cumsum(testNBA$betProfit.comb)

##### FIXED BET AMOUNT - ODDS FAV #####
# calculate bankroll - odds favorite
testNBA$betProfit.odds <- NA

testNBA$betProfit.odds <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team1 == 'W', testNBA$betPayout.team1, 0 )
testNBA$betProfit.odds <- ifelse( (testNBA$teamWinProb.team1 > .5) & testNBA$outcomeGame.team1 == 'L', -1 * fixedBet, testNBA$betProfit.odds)
testNBA$betProfit.odds <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team2 == 'W',  testNBA$betPayout.team2, testNBA$betProfit.odds)
testNBA$betProfit.odds <- ifelse( (testNBA$teamWinProb.team1 < .5) & testNBA$outcomeGame.team2 == 'L',  -1 * fixedBet, testNBA$betProfit.odds)

testNBA$bankAmount.odds <- cumsum(testNBA$betProfit.odds)


## combined graphs ##
# add row numbers
testNBA$rownums <- 1:nrow(testNBA)
graphdata <- testNBA %>% select(dateGame, bankAmount, bankAmount.fav, bankAmount.comb, bankAmount.odds)
names(graphdata) <- c("dateGame","Model Edge", "Model Favorite","Model Combo", "Odds Favorite")
#graphdata$dateGame <- as.Date(graphdata$dateGame)

# graph of bankroll over time
d <- melt(graphdata, id.vars="dateGame")
ggplot(d, aes(dateGame,value, col=variable)) + 
  geom_line(size = 1.5, alpha = 0.7) +
  scale_x_date(date_breaks = "months" , date_labels = "%b '%y") +
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.position = "top") +
  xlab("")+
  ylab("Bankroll ($)") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  annotate(geom="text", label="Starting Bankroll ($0)", 
           x= as.Date('2019-03-15'), size = 4, y=10, vjust=-1) +
  scale_color_manual(values = c("#8BB8E8","#003B5C", "#FFD100","#808080"))



### Summary Stats ###

# fav
summ <- testNBA
summ$bet_type.fav <- ifelse(summ$betProfit.fav > 0 , "win", "loss")
summ[summ$betProfit.fav == 0,]$bet_type <- "no bet"
table(summ$bet_type.fav)
tail(summ$bankAmount.fav)

# edge
summ$bet_type.edge <- ifelse(summ$betProfit > 0 , "win", "loss")
summ[summ$betProfit == 0,]$bet_type.edge <- "no bet"
table(summ$bet_type.edge)
tail(summ$bankAmount)

# combo
summ$bet_type.comb <- ifelse(summ$betProfit.comb > 0 , "win", "loss")
summ[summ$betProfit.comb == 0,]$bet_type.comb <- "no bet"
table(summ$bet_type.comb)
tail(summ$bankAmount.comb)

# odds fav
summ$bet_type.odds <- ifelse(summ$betProfit.odds > 0 , "win", "loss")
summ[summ$betProfit.odds == 0,]$bet_type.odds <- "no bet"
table(summ$bet_type.odds)
tail(summ$bankAmount.odds)



# # add row numbers
# testNBA$rownums <- 1:nrow(testNBA)
# testNBA$dateGame <- as.Date(testNBA$dateGame)
# 
# # graph of bankroll over time
# ggplot(data = testNBA , aes(x = dateGame , y = bankAmount)) +
#   geom_line( colour = 'darkgreen', size = 1.5, alpha = 0.7) +
#   geom_hline(yintercept=0, linetype="dashed", color = "black") +
#   scale_x_date(date_breaks = "months" , date_labels = "%b '%y") + 
#   theme(axis.text=element_text(size=18), 
#         axis.title=element_text(size=20),
#         legend.text=element_text(size=16),
#         legend.title=element_blank()) +
#   xlab("") +
#   ylab("Bankroll ($)") +
#   geom_hline(yintercept=0, linetype="dashed", color = "black") +
#   annotate(geom="text", label="Starting Bankroll ($0)", 
#            x= as.Date('2019-04-01'), size = 4, y=5, vjust=-1)

