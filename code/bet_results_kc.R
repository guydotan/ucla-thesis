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
  
  #nba <- nba[(nba$teamGameNumNoPost.team1 >= 9 & nba$teamGameNumNoPost.team2 >= 9),]
  
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

##### KELLY CRITERIA #####
{
  # exclude games without odds
  # testNBA <- fullNBA[!is.na(fullNBA$teamWinProb),]
  
  # calculate kelly criteria
  testNBA$kellyCrit.team1 <- apply(testNBA[,c('teamWinProb.team1','modelPred')], 1, function(x) kelly_crit(p = x[2], b = prob_to_odds(x[1])))
  testNBA$kellyCrit.team2 <- apply(testNBA[,c('teamWinProb.team2','modelPred')], 1, function(x) kelly_crit(p = (1-x[2]), b = prob_to_odds(x[1])))
  
  #kelly_crit(p = fullNBA$modelPred[1], b = fullNBA$modelPred[1])
  #kelly_crit(p = fullNBA$modelPred[2], b = fullNBA$modelPred[2])
  
  testNBA <- as.data.frame(lapply(testNBA, unlist))
  
  # calculate bankroll
  myBank_qtr <- 10000
  myBank_5th <- 10000
  myBank_8th <- 10000
  myBank_full <- 10000
  testNBA$betAmount_qtr <- NA
  testNBA$bankAmount_qtr <- NA
  testNBA$betAmount_5th <- NA
  testNBA$bankAmount_5th <- NA
  testNBA$betAmount_8th <- NA
  testNBA$bankAmount_8th <- NA
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
      
      testNBA[i,]$betAmount_5th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bet_amt( myBank_5th , testNBA[i,"kellyCrit.team1"])/5, 
                                          bet_amt( myBank_5th , testNBA[i,"kellyCrit.team2"])/5)
      testNBA[i,]$bankAmount_5th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                           bankroll( bank = myBank_5th , amt = testNBA[i,]$betAmount_5th , 
                                                     ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                           bankroll( bank = myBank_5th , amt = testNBA[i,]$betAmount_5th , 
                                                     ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))
      
      testNBA[i,]$betAmount_8th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bet_amt( myBank_8th , testNBA[i,"kellyCrit.team1"])/8, 
                                          bet_amt( myBank_8th , testNBA[i,"kellyCrit.team2"])/8)
      testNBA[i,]$bankAmount_8th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                           bankroll( bank = myBank_8th , amt = testNBA[i,]$betAmount_8th , 
                                                     ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                           bankroll( bank = myBank_8th , amt = testNBA[i,]$betAmount_8th , 
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
      
      testNBA[i,]$betAmount_5th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bet_amt( testNBA[i-1,]$bankAmount_5th , testNBA[i,"kellyCrit.team1"]/5), 
                                          bet_amt( testNBA[i-1,]$bankAmount_5th , testNBA[i,"kellyCrit.team2"]/5))
      testNBA[i,]$bankAmount_5th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                           bankroll( bank = testNBA[i-1,]$bankAmount_5th , amt = testNBA[i,]$betAmount_5th , 
                                                     ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                           bankroll( bank = testNBA[i-1,]$bankAmount_5th , amt = testNBA[i,]$betAmount_5th , 
                                                     ml = testNBA[i,"teamML.team2"], win = testNBA[i , "outcomeGame.team2"] ))   
      
      testNBA[i,]$betAmount_8th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                          bet_amt( testNBA[i-1,]$bankAmount_8th , testNBA[i,"kellyCrit.team1"]/8), 
                                          bet_amt( testNBA[i-1,]$bankAmount_8th , testNBA[i,"kellyCrit.team2"]/8))
      testNBA[i,]$bankAmount_8th <- ifelse(testNBA[i,"kellyCrit.team1"] > 0, 
                                           bankroll( bank = testNBA[i-1,]$bankAmount_8th , amt = testNBA[i,]$betAmount_8th , 
                                                     ml = testNBA[i,"teamML.team1"], win = testNBA[i , "outcomeGame.team1"] ),
                                           bankroll( bank = testNBA[i-1,]$bankAmount_8th , amt = testNBA[i,]$betAmount_8th , 
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
}

##### COMBINED GRAPHS #####
{
  # add row numbers
  testNBA$rownums <- 1:nrow(testNBA)
  graphdata <- testNBA %>% dplyr::select(dateGame, bankAmount_full, bankAmount_qtr, bankAmount_5th, bankAmount_8th)
  names(graphdata) <- c("dateGame","Full Kelly","Qtr Kelly","5th Kelly", "8th Kelly")
  
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
    labs(title = "Total Bankroll Over Time using Various Kely Criteria", subtitle = "(2019-20 NBA Season)") +
    geom_hline(yintercept=10000, linetype="dashed", color = "black") +
    annotate(geom="text", label="Starting Bankroll ($10,000)", 
             x= as.Date('2020-03-25'), size = 3, y=10005, vjust=-1) +
    scale_color_manual(values = c("#616A6B", "#FFD100", "#005587", "#00CFFF"))#,"#8BB8E8", "#FFA500"))
}

##### SUMMARY STATS #####
{
  summ <- testNBA
  summ_df <- data.frame("Model" = c("Full Kelly","Qtr Kelly","5th Kelly", "8th Kelly"),
                        "Win" = rep(NA,4),
                        "Loss" = rep(NA,4),
                        "No.Bet" = rep(NA,4),
                        "Win Pct" = rep(NA,4),
                        "Final.Bankroll" = rep(NA,4),
                        "ROI" = rep(NA,4))
  summ$bet_type.kc <- ifelse( (summ$modelPred > summ$teamWinProb.team1) & summ$outcomeGame.team1 == 'W', 'win',  0 )
  summ$bet_type.kc <- ifelse( (summ$modelPred > summ$teamWinProb.team1) & summ$outcomeGame.team1 == 'L', 'loss', summ$bet_type.kc )
  summ$bet_type.kc <- ifelse( (summ$modelPred < summ$teamWinProb.team1) & summ$outcomeGame.team2 == 'W', 'win',  summ$bet_type.kc)
  summ$bet_type.kc <- ifelse( (summ$modelPred < summ$teamWinProb.team1) & summ$outcomeGame.team2 == 'L', 'loss', summ$bet_type.kc)
  
  # full
  summ_df[1,2] <- sum(summ$bet_type.kc == 'win')
  summ_df[1,3] <- sum(summ$bet_type.kc == 'loss')
  summ_df[1,4] <- sum(summ$bet_type.kc == 'no bet')
  summ_df[1,5] <- round(summ_df[1,2] / (summ_df[1,2]+summ_df[1,3]),3)
  summ_df[1,6] <- round(summ$bankAmount_full[nrow(summ)],2)
  summ_df[1,7] <- round((summ$bankAmount_full[nrow(summ)] - myBank_full)/(myBank_full),3)
  
  # qtr
  summ_df[2,2] <- sum(summ$bet_type.kc == 'win')
  summ_df[2,3] <- sum(summ$bet_type.kc == 'loss')
  summ_df[2,4] <- sum(summ$bet_type.kc == 'no bet')
  summ_df[2,5] <- round(summ_df[2,2] / (summ_df[2,2]+summ_df[2,3]),3)
  summ_df[2,6] <- round(summ$bankAmount_qtr[nrow(summ)],2)
  summ_df[2,7] <- round((summ$bankAmount_qtr[nrow(summ)] - myBank_qtr)/(myBank_qtr),3)
  
  # 5th
  summ_df[3,2] <- sum(summ$bet_type.kc == 'win')
  summ_df[3,3] <- sum(summ$bet_type.kc == 'loss')
  summ_df[3,4] <- sum(summ$bet_type.kc == 'no bet')
  summ_df[3,5] <- round(summ_df[3,2] / (summ_df[3,2]+summ_df[3,3]),3)
  summ_df[3,6] <- round(summ$bankAmount_5th[nrow(summ)],2)
  summ_df[3,7] <- round((summ$bankAmount_5th[nrow(summ)] - myBank_5th)/(myBank_5th),3)
  
  # 8th
  summ_df[4,2] <- sum(summ$bet_type.kc == 'win')
  summ_df[4,3] <- sum(summ$bet_type.kc == 'loss')
  summ_df[4,4] <- sum(summ$bet_type.kc == 'no bet')
  summ_df[4,5] <- round(summ_df[4,2] / (summ_df[4,2]+summ_df[4,3]),3)
  summ_df[4,6] <- round(summ$bankAmount_8th[nrow(summ)],2)
  summ_df[4,7] <- round((summ$bankAmount_8th[nrow(summ)] - myBank_8th)/(myBank_8th),3)
  
}
