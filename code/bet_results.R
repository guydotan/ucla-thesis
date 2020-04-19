## Sample betting results including kelly criteria ##

library(ggplot2)
library(reshape2)

setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data/")
options(scipen = 99999)

nba <- read.csv("nba.csv", stringsAsFactors = F)
testNBA <- nba[!is.na(nba$teamWinProb),]
testNBA <- nba[nba$seasonID == 201819, ]

### model prediction to test betting results ###
# add fake model prediction
# testNBA$mlOdds <- sapply(testNBA$teamML, function(x) ml_to_prob(x))
#set.seed(1235)
testNBA$modelPred <- sapply(testNBA$teamWinProb, function(x) rnorm(1, x, 0.1 ))
testNBA$modelPred <- ifelse(testNBA$modelPred < 0 , 0.001 , testNBA$modelPred)
testNBA$modelPred <- ifelse(testNBA$modelPred > 1 , 0.999 , testNBA$modelPred)


##### FIXED BET AMOUNT #####
fixedBet <- 10
testNBA$betPayout <- sapply(testNBA$teamML, function(x) ml_pay(x , fixedBet))

# calculate bankroll
testNBA$betProfit <- NA
# for (i in 1:nrow(testNBA)){
#   if ((testNBA[i,]$modelPred > testNBA[i,]$teamWinProb) & testNBA[i,]$isWin == TRUE) {
#     testNBA[i,]$betProfit <- testNBA[i,]$betPayout
#   }
#   else if ((testNBA[i,]$modelPred > testNBA[i,]$teamWinProb) & testNBA[i,]$isWin == FALSE){    testNBA[i,]$betProfit <- -1 * fixedBet
#   }
#   else{
#     testNBA[i,]$betProfit <- 0
#   }
# }

testNBA$betProfit <- ifelse( (testNBA$modelPred > testNBA$teamWinProb) & testNBA$isWin == TRUE, testNBA$betPayout - 0, 0 )
testNBA$betProfit <- ifelse( (testNBA$modelPred > testNBA$teamWinProb) & testNBA$isWin == FALSE, -1 * fixedBet, testNBA$betProfit)

testNBA$bankAmount <- cumsum(testNBA$betProfit)


# add row numbers
testNBA$rownums <- 1:nrow(testNBA)
testNBA$dateGame <- as.Date(testNBA$dateGame)

# graph of bankroll over time
ggplot(data = testNBA , aes(x = dateGame , y = bankAmount)) +
  geom_line( colour = 'darkgreen', size = 1.5, alpha = 0.7) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  scale_x_date(date_breaks = "months" , date_labels = "%b '%y") + 
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.title=element_blank()) +
  xlab("") +
  ylab("Bankroll ($)") +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  annotate(geom="text", label="Starting Bankroll ($0)", 
           x= as.Date('2019-05-25'), size = 4, y=5, vjust=-1)



##### KELLY CRITERIA #####

# exclude games without odds
# testNBA <- fullNBA[!is.na(fullNBA$teamWinProb),]

# add fake model prediction
#testNBA$modelPred <- sapply(testNBA$teamWinProb, function(x) rnorm(1, x, 0.1 ))
#testNBA$modelPred <- ifelse(testNBA$modelPred < 0 , 0.001 , testNBA$modelPred)
#testNBA$modelPred <- ifelse(testNBA$modelPred > 1 , 0.999 , testNBA$modelPred)

# calculate kelly criteria
testNBA$kellyCrit <- apply(testNBA[,c('teamWinProb','modelPred')], 1, function(x) kelly_crit(p = x[2], b = prob_to_odds(x[1])))

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
    testNBA[i,]$betAmount_qtr <- bet_amt( myBank_qtr , testNBA[i,"kellyCrit"] )/4
    testNBA[i,]$bankAmount_qtr <- bankroll( bank = myBank_qtr , amt = bet_amt( myBank_qtr , testNBA[i,"kellyCrit"] )/4 , 
                                        ml = testNBA[i,"teamML"], win = testNBA[i , "isWin"] )
    
    testNBA[i,]$betAmount_3rd <- bet_amt( myBank_3rd , testNBA[i,"kellyCrit"] )/3
    testNBA[i,]$bankAmount_3rd <- bankroll( bank = myBank_3rd , amt = bet_amt( myBank_3rd , testNBA[i,"kellyCrit"] )/3 , 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , "isWin"] )
    
    testNBA[i,]$betAmount_half <- bet_amt( myBank_half , testNBA[i,"kellyCrit"] )/2
    testNBA[i,]$bankAmount_half <- bankroll( bank = myBank_half , amt = bet_amt( myBank_half , testNBA[i,"kellyCrit"] )/2 , 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , "isWin"] )
    
    testNBA[i,]$betAmount_full <- bet_amt( myBank_full , testNBA[i,"kellyCrit"] )
    testNBA[i,]$bankAmount_full <- bankroll( bank = myBank_full , amt = bet_amt( myBank_full , testNBA[i,"kellyCrit"] ) , 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , "isWin"] )
    
  }
  else{
    testNBA[i,]$betAmount_qtr <-  bet_amt( testNBA[i-1,]$bankAmount_qtr , testNBA[i,"kellyCrit"] )/4
    testNBA[i,]$bankAmount_qtr <- bankroll( bank = testNBA[i-1,]$bankAmount_qtr[[1]] , amt = bet_amt( testNBA[i-1,]$bankAmount_qtr , testNBA[i,"kellyCrit"] )/4, 
                                        ml = testNBA[i,"teamML"], win = testNBA[i , ]$isWin )
    
    testNBA[i,]$betAmount_3rd <- bet_amt( testNBA[i-1,]$bankAmount_3rd , testNBA[i,"kellyCrit"] )/3
    testNBA[i,]$bankAmount_3rd <- bankroll( bank = testNBA[i-1,]$bankAmount_3rd[[1]] , amt = bet_amt( testNBA[i-1,]$bankAmount_3rd , testNBA[i,"kellyCrit"] )/3, 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , ]$isWin )
    
    testNBA[i,]$betAmount_half <- bet_amt( testNBA[i-1,]$bankAmount_half , testNBA[i,"kellyCrit"] )/2
    testNBA[i,]$bankAmount_half <- bankroll( bank = testNBA[i-1,]$bankAmount_half[[1]] , amt = bet_amt( testNBA[i-1,]$bankAmount_half , testNBA[i,"kellyCrit"] )/2, 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , ]$isWin )
    
    testNBA[i,]$betAmount_full <- bet_amt( testNBA[i-1,]$bankAmount_full , testNBA[i,"kellyCrit"] )
    testNBA[i,]$bankAmount_full <- bankroll( bank = testNBA[i-1,]$bankAmount_full[[1]] , amt = bet_amt( testNBA[i-1,]$bankAmount_full , testNBA[i,"kellyCrit"] ), 
                                            ml = testNBA[i,"teamML"], win = testNBA[i , ]$isWin )
  }
}

# add row numbers
testNBA$rownums <- 1:nrow(testNBA)
graphdata <- testNBA[,c(5,69,71,73,75)]
names(graphdata) <- c("dateGame","Qtr Kelly","3rd Kelly","Half Kelly", "Full Kelly")

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
           x= as.Date('2019-05-25'), size = 4, y=1025, vjust=-1)


# old graph
# ggplot(data = testNBA , aes(x = rownums , y = bankAmount)) +
#   geom_line( colour = 'darkgreen') +
#   geom_hline(yintercept=0, linetype="dashed", color = "black") +
#   geom_hline(yintercept=1000, linetype="dashed", color = "black") +
#   geom_text(data=data.frame(x=nrow(testNBA)/2,y=myBank), aes(x, y), label= "Starting Bankroll ($1000)", vjust=-1)


### Summary Stats ###

summ <- testNBA
summ$bet_type <- ifelse(summ$betProfit > 0 , "win", "loss")
summ[summ$betProfit == 0,]$bet_type <- "no bet"

table(summ$bet_type)
