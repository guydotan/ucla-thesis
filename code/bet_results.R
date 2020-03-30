## Sample betting results including kelly criteria ##

setwd("~/Documents/UCLA MAS/Thesis/repo/uclathesis/data/")
options(scipen = 99999)

nba <- read.csv("nba.csv", stringsAsFactors = F)
testNBA <- nba[!is.na(nba$teamWinProb),]

### model prediction to test betting results ###
# add fake model prediction
#testNBA$mlOdds <- sapply(testNBA$teamML, function(x) ml_to_prob(x))
testNBA$modelPred <- sapply(testNBA$teamWinProb, function(x) rnorm(1, x, 0.1 ))
testNBA$modelPred <- ifelse(testNBA$modelPred < 0 , 0.001 , testNBA$modelPred)
testNBA$modelPred <- ifelse(testNBA$modelPred > 1 , 0.999 , testNBA$modelPred)


##### FIXED BET AMOUNT #####
fixedBet <- 10
testNBA$betPayout <- sapply(testNBA$teamML, function(x) ml_pay(x , fixedBet))

# calculate bankroll
testNBA$betProfit <- NA
for (i in 1:nrow(testNBA)){
  if ((testNBA[i,]$modelPred > testNBA[i,]$teamWinProb) & testNBA[i,]$isWin == TRUE) {
    testNBA[i,]$betProfit <- testNBA[i,]$betPayout
  }
  else if ((testNBA[i,]$modelPred > testNBA[i,]$teamWinProb) & testNBA[i,]$isWin == FALSE){    testNBA[i,]$betProfit <- -1 * fixedBet
  }
  else{
    testNBA[i,]$betProfit <- 0
  }
}

testNBA$trr <- ifelse( (testNBA$modelPred > testNBA$mlOdds) & testNBA$isWin == TRUE, testNBA$betPayout - 10, 0 )
testNBA$trr <- ifelse( (testNBA$modelPred > testNBA$mlOdds) & testNBA$isWin == FALSE, -1 * fixedBet, testNBA$trr)

#testNBA$trr <- ifelse( testNBA$isWin == TRUE, testNBA$betPayout - 10, -10 )
testNBA$bankAmount <- cumsum(testNBA$trr)


# add row numbers
testNBA$rownums <- 1:nrow(testNBA)

# graph of bankroll over time
ggplot(data = testNBA , aes(x = rownums , y = bankAmount)) +
  geom_line( colour = 'darkgreen') +
  geom_hline(yintercept=0, linetype="dashed", color = "black")


##### KELLY CRITERIA #####

# exclude games without odds
# testNBA <- fullNBA[!is.na(fullNBA$teamWinProb),]

# add fake model prediction
testNBA$modelPred <- sapply(testNBA$teamWinProb, function(x) rnorm(1, x, 0.1 ))
testNBA$modelPred <- ifelse(testNBA$modelPred < 0 , 0.001 , testNBA$modelPred)
testNBA$modelPred <- ifelse(testNBA$modelPred > 1 , 0.999 , testNBA$modelPred)

# calculate kelly criteria
testNBA$kellyCrit <- apply(testNBA[,c('teamWinProb','modelPred')], 1, function(x) kelly_crit(p = x[2], b = prob_to_odds(x[1])))

#kelly_crit(p = fullNBA$modelPred[1], b = fullNBA$modelPred[1])
#kelly_crit(p = fullNBA$modelPred[2], b = fullNBA$modelPred[2])

testNBA <- as.data.frame(lapply(testNBA, unlist))

# calculate bankroll
myBank <- 1000
testNBA$betAmount <- NA
testNBA$bankAmount <- NA
for (i in 1:nrow(testNBA)){
  if (i == 1) {
    testNBA[i,]$betAmount <- bet_amt( myBank , testNBA[i,"kellyCrit"] )/4
    testNBA[i,]$bankAmount <- bankroll( bank = myBank , amt = bet_amt( myBank , testNBA[i,"kellyCrit"] )/4 , 
                                        ml = testNBA[i,"teamML"], win = testNBA[i , "isWin"] )
  }
  else{
    testNBA[i,]$betAmount <- bet_amt( testNBA[i-1,]$bankAmount , testNBA[i,"kellyCrit"] )/4
    testNBA[i,]$bankAmount <- bankroll( bank = testNBA[i-1,]$bankAmount[[1]] , amt = bet_amt( testNBA[i-1,]$bankAmount , testNBA[i,"kellyCrit"] )/4, 
                                        ml = testNBA[i,"teamML"], win = testNBA[i , ]$isWin )
  }
}

# add row numbers
testNBA$rownums <- 1:nrow(testNBA)

# graph of bankroll over time
ggplot(data = testNBA , aes(x = rownums , y = bankAmount)) +
  geom_line( colour = 'darkgreen') +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_hline(yintercept=1000, linetype="dashed", color = "black") +
  geom_text(data=data.frame(x=nrow(testNBA)/2,y=myBank), aes(x, y), label= "Starting Bankroll ($1000)", vjust=-1)