## Sample betting results for kelly criteria ##

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


##### KELLY CRITERIA #####

# exclude games without odds
# testNBA <- fullNBA[!is.na(fullNBA$teamWinProb),]

# calculate kelly criteria
testNBA$kellyCrit.team1 <- apply(testNBA[,c('teamWinProb.team1','modelPred')], 1, function(x) kelly_crit(p = x[2], b = prob_to_odds(x[1])))
testNBA$kellyCrit.team2 <- apply(testNBA[,c('teamWinProb.team2','modelPred')], 1, function(x) kelly_crit(p = (1-x[2]), b = prob_to_odds(x[1])))

#kelly_crit(p = fullNBA$modelPred[1], b = fullNBA$modelPred[1])
#kelly_crit(p = fullNBA$modelPred[2], b = fullNBA$modelPred[2])

testNBA <- as.data.frame(lapply(testNBA, unlist))

# calculate bankroll
myBank_qtr <- 1000
myBank_3rd <- 1000
myBank_half <- 1000
myBank_full <- 1000
testNBA$betAmount_qtr <- NA
testNBA$bankAmount_qtr <- NA
testNBA$betAmount_3rd <- NA
testNBA$bankAmount_3rd <- NA
testNBA$betAmount_half <- NA
testNBA$bankAmount_half <- NA
testNBA$betAmount_full <- NA
testNBA$bankAmount_full <- NA

for (i in 1:nrow(testNBA)){
  if (i == 1) {
    testNBA[i,]$betAmount_qtr <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                        bet_amt( myBank_qtr , testNBA[i,"kellyCrit.team1"])/4, 
                                        bet_amt( myBank_qtr , testNBA[i,"kellyCrit.team2"])/4)
    testNBA[i,]$bankAmount_qtr <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = myBank_qtr , amt = testNBA[i,]$betAmount_qtr , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = myBank_qtr , amt = testNBA[i,]$betAmount_qtr , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))
    
    testNBA[i,]$betAmount_3rd <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( myBank_3rd , testNBA[i,"kellyCrit.team1"])/3, 
                                         bet_amt( myBank_3rd , testNBA[i,"kellyCrit.team2"])/3)
    testNBA[i,]$bankAmount_3rd <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = myBank_3rd , amt = testNBA[i,]$betAmount_3rd , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = myBank_3rd , amt = testNBA[i,]$betAmount_3rd , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))
    
    testNBA[i,]$betAmount_half <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( myBank_half , testNBA[i,"kellyCrit.team1"])/2, 
                                         bet_amt( myBank_half , testNBA[i,"kellyCrit.team2"])/2)
    testNBA[i,]$bankAmount_half <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = myBank_half , amt = testNBA[i,]$betAmount_half , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = myBank_half , amt = testNBA[i,]$betAmount_half , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))
    
    testNBA[i,]$betAmount_full <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( myBank_full , testNBA[i,"kellyCrit.team1"]), 
                                         bet_amt( myBank_full , testNBA[i,"kellyCrit.team2"]))
    testNBA[i,]$bankAmount_full <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                      bankroll( bank = myBank_full , amt = testNBA[i,]$betAmount_full , 
                                             ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                      bankroll( bank = myBank_full , amt = testNBA[i,]$betAmount_full , 
                                                ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))
  }
  else{
    testNBA[i,]$betAmount_qtr <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                        bet_amt( testNBA[i-1,]$bankAmount_qtr , testNBA[i,"kellyCrit.team1"]/4), 
                                        bet_amt( testNBA[i-1,]$bankAmount_qtr , testNBA[i,"kellyCrit.team2"]/4))
    testNBA[i,]$bankAmount_qtr <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bankroll( bank = testNBA[i-1,]$bankAmount_qtr , amt = testNBA[i,]$betAmount_qtr , 
                                                   ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                         bankroll( bank = testNBA[i-1,]$bankAmount_qtr , amt = testNBA[i,]$betAmount_qtr , 
                                                   ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))   
    
    testNBA[i,]$betAmount_3rd <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( testNBA[i-1,]$bankAmount_3rd , testNBA[i,"kellyCrit.team1"]/3), 
                                         bet_amt( testNBA[i-1,]$bankAmount_3rd , testNBA[i,"kellyCrit.team2"]/3))
    testNBA[i,]$bankAmount_3rd <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = testNBA[i-1,]$bankAmount_3rd , amt = testNBA[i,]$betAmount_3rd , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = testNBA[i-1,]$bankAmount_3rd , amt = testNBA[i,]$betAmount_3rd , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))   
    
    testNBA[i,]$betAmount_half <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( testNBA[i-1,]$bankAmount_half , testNBA[i,"kellyCrit.team1"]/2), 
                                         bet_amt( testNBA[i-1,]$bankAmount_half , testNBA[i,"kellyCrit.team2"]/2))
    testNBA[i,]$bankAmount_half <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = testNBA[i-1,]$bankAmount_half , amt = testNBA[i,]$betAmount_half , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = testNBA[i-1,]$bankAmount_half , amt = testNBA[i,]$betAmount_half , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))   
    
    testNBA[i,]$betAmount_full <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                         bet_amt( testNBA[i-1,]$bankAmount_full , testNBA[i,"kellyCrit.team1"]), 
                                         bet_amt( testNBA[i-1,]$bankAmount_full , testNBA[i,"kellyCrit.team2"]))
    testNBA[i,]$bankAmount_full <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bankroll( bank = testNBA[i-1,]$bankAmount_full , amt = testNBA[i,]$betAmount_full , 
                                                    ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                          bankroll( bank = testNBA[i-1,]$bankAmount_full , amt = testNBA[i,]$betAmount_full , 
                                                    ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))    
  }
}

# add row numbers
testNBA$rownums <- 1:nrow(testNBA)
graphdata <- testNBA %>% select(dateGame, bankAmount_qtr, bankAmount_3rd, bankAmount_half, bankAmount_full)
names(graphdata) <- c("dateGame","Qtr Kelly","Third Kelly","Half Kelly", "Full Kelly")

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
  geom_hline(yintercept=1000, linetype="dashed", color = "black") +
  annotate(geom="text", label="Starting Bankroll ($1000)", 
           x= as.Date('2019-03-15'), size = 4, y=1005, vjust=-1) +
  scale_color_manual(values = c("#8BB8E8","#003B5C", "#FFD100","#808080"))


### Summary Stats ###

# edge
summ <- testNBA
summ$bet_type.kc <- ifelse( (summ$modelPred > summ$teamWinProb.team1) & summ$outcomeGame.team1 == 'W', 'win',  0 )
summ$bet_type.kc <- ifelse( (summ$modelPred > summ$teamWinProb.team1) & summ$outcomeGame.team1 == 'L', 'loss', summ$bet_type.kc )
summ$bet_type.kc <- ifelse( (summ$modelPred < summ$teamWinProb.team1) & summ$outcomeGame.team2 == 'W', 'win',  summ$bet_type.kc)
summ$bet_type.kc <- ifelse( (summ$modelPred < summ$teamWinProb.team1) & summ$outcomeGame.team2 == 'L', 'loss', summ$bet_type.kc)
#summ$bet_type.edge <- ifelse(summ$betProfit > 0 , "win", "loss")
#summ[summ$betProfit == 0,]$bet_type.edge <- "no bet"
table(summ$bet_type.kc)
tail(summ$bankAmount_full)


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

