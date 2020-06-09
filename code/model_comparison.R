###### Model Comparison 

# set working directory and load packages
setwd("~/Documents/UCLA MAS/Thesis/repo-local/uclathesis/")
library("dplyr")
library("caret")
library("MASS")
library("reshape2")

# read in data sets
nba_xgm <- read.csv("data/cleaned-data/new_nba_xgm_matchup.csv", stringsAsFactors = F, na.strings = "NA")
nba_ytd <- read.csv("data/cleaned-data/new_nba_ytd_matchup.csv", stringsAsFactors = F, na.strings = "NA")

#### DATA PREP - YTD ####
# transform asb variable into binary
nba_ytd$beforeASB.team1 <- ifelse(nba_ytd$asb_prepost.team1 == "pre", T, F)
nba_ytd$beforeASB.team2 <- ifelse(nba_ytd$asb_prepost.team2 == "pre", T, F)
nba_ytd <- nba_ytd %>% dplyr::select(-asb_prepost.team1, -asb_prepost.team2)

# make home/away into a factor
nba_ytd <- nba_ytd %>% 
  mutate(locationGame.team1 = factor(locationGame.team1, levels = c("A", "H")))
nba_ytd <- nba_ytd %>% 
  mutate(locationGame.team2 = factor(locationGame.team2, levels = c("A", "H")))

# make win/loss into a factor
nba_ytd <- nba_ytd %>% 
  mutate(outcomeGame.team1 = factor(outcomeGame.team1, levels = c("W", "L")))

# save validation set
orig1920_ytd <- nba_ytd[nba_ytd$slugSeason == '2019-20',]

# remove away outcome variable
nba_ytd <- nba_ytd %>% dplyr::select(-outcomeGame.team2)

# remove missing data
nba_ytd_cc <- nba_ytd[complete.cases(nba_ytd$ptsTeam.own.agg.per100.team1 & nba_ytd$ptsTeam.own.agg.per100.team2),]
sum(is.na(nba_ytd_cc$ptsTeam.opp.agg.per100.team1))

# remove unneeded variables
nba_ytd_cc_mod <- nba_ytd_cc %>% dplyr::select(-newGameID, -idTeam.team1, -idOpp, -slugOpp, -dateGame,
                                               -locationGame.team2, -idTeam.team2, -beforeASB.team2,
                                               -isWin.agg.team1, -isLoss.agg.team1, -isWin.agg.team2, -isLoss.agg.team2,
                                               -teamGameNumNoPost.team1, -teamGameNumNoPost.team2,
                                               -numberGameTeamSeason.own.team1, -numberGameTeamSeason.opp.team1, -numberGameTeamSeason.own.team2, -numberGameTeamSeason.opp.team2,
                                               -countDaysRestTeam.own.team1, -countDaysRestTeam.opp.team1, -countDaysRestTeam.own.team2, -countDaysRestTeam.opp.team2,
                                               -countDaysNextGameTeam.own.team1, -countDaysNextGameTeam.opp.team1, -countDaysNextGameTeam.own.team2, -countDaysNextGameTeam.opp.team2,
                                               -possessions.own.agg.per100.team1, -possessions.opp.agg.per100.team1, -possessions.own.agg.per100.team2, -possessions.opp.agg.per100.team2,
                                               -teamML.team1, -oppML.team1, -teamWinProb.team1, -oppWinProb.team1,
                                               -teamML.team2, -oppML.team2, -teamWinProb.team1, -oppWinProb.team2,
                                               -teamWinProb.team2, -oppWinProb.team2)

# remove 2019-20 season for validation set
ytd1920_full <- nba_ytd_cc_mod[nba_ytd_cc_mod$slugSeason == '2019-20',]

# define model to build with
ytd_full <- nba_ytd_cc_mod[nba_ytd_cc_mod$slugSeason != '2019-20',]


#### DATA PREP - XGM #####
# transform asb variable into binary
nba_xgm$beforeASB.team1 <- ifelse(nba_xgm$asb_prepost.team1 == "pre", T, F)
nba_xgm$beforeASB.team2 <- ifelse(nba_xgm$asb_prepost.team2 == "pre", T, F)
nba_xgm <- nba_xgm %>% dplyr::select(-asb_prepost.team1, -asb_prepost.team2)

# make home/away into a factor
nba_xgm <- nba_xgm %>% 
  mutate(locationGame.team1 = factor(locationGame.team1, levels = c("A", "H")))
nba_xgm <- nba_xgm %>% 
  mutate(locationGame.team2 = factor(locationGame.team2, levels = c("A", "H")))

# make win/loss into a factor
nba_xgm <- nba_xgm %>% 
  mutate(outcomeGame.team1 = factor(outcomeGame.team1, levels = c("W", "L")))

# save validation set
orig1920_xgm <- nba_xgm[nba_xgm$slugSeason == '2019-20',]

# remove away outcome variable
nba_xgm <- nba_xgm %>% dplyr::select(-outcomeGame.team2)

# remove missing data
nba_xgm_cc <- nba_xgm[complete.cases(nba_xgm$ptsTeam.own.agg.per100.team1 & nba_xgm$ptsTeam.own.agg.per100.team2),]
sum(is.na(nba_xgm_cc$ptsTeam.opp.agg.per100.team1))

# remove unneeded variables
nba_xgm_cc_mod <- nba_xgm_cc %>% dplyr::select(-newGameID, -idTeam.team1, -idOpp, -slugOpp, -dateGame,
                                               -locationGame.team2, -idTeam.team2, -beforeASB.team2,
                                               -isWin.agg.team1, -isLoss.agg.team1, -isWin.agg.team2, -isLoss.agg.team2,
                                               -teamGameNumNoPost.team1, -teamGameNumNoPost.team2,
                                               -numberGameTeamSeason.own.team1, -numberGameTeamSeason.opp.team1, -numberGameTeamSeason.own.team2, -numberGameTeamSeason.opp.team2,
                                               -countDaysRestTeam.own.team1, -countDaysRestTeam.opp.team1, -countDaysRestTeam.own.team2, -countDaysRestTeam.opp.team2,
                                               -countDaysNextGameTeam.own.team1, -countDaysNextGameTeam.opp.team1, -countDaysNextGameTeam.own.team2, -countDaysNextGameTeam.opp.team2,
                                               -possessions.own.agg.per100.team1, -possessions.opp.agg.per100.team1, -possessions.own.agg.per100.team2, -possessions.opp.agg.per100.team2,
                                               -teamML.team1, -oppML.team1, -teamWinProb.team1, -oppWinProb.team1,
                                               -teamML.team2, -oppML.team2, -teamWinProb.team1, -oppWinProb.team2,
                                               -teamWinProb.team2, -oppWinProb.team2)

# remove 2019-20 season for validation set
xgm1920 <- nba_xgm_cc_mod[nba_xgm_cc_mod$slugSeason == '2019-20',]

# define model to build with
xgm_full <- nba_xgm_cc_mod[nba_xgm_cc_mod$slugSeason != '2019-20',]



#### MODEL PREP - YTD ####
set.seed(2020)
# remove id variables
ytd <- ytd_full %>% dplyr::select(-slugSeason, -idGame, -slugTeam, -slugMatchup)

# converting every categorical variable to numerical using dummy variables
dmy_ytd <- dummyVars("outcomeGame.team1 ~ .", data = ytd, fullRank = T)
trans_ytd <- data.frame(predict(dmy_ytd, newdata = ytd))

# append the y variable
trans_ytd$outcomeGame.team1 <- ytd$outcomeGame.team1

# spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(trans_ytd$outcomeGame.team1, p=0.75, list=FALSE)
ytdTrain <- trans_ytd[ index,]
ytdTest <- trans_ytd[-index,]


#### MODEL PREP - XGM ####
set.seed(2020)
# remove id variables
xgm <- xgm_full %>% dplyr::select(-slugSeason, -idGame, -slugTeam, -slugMatchup)

# converting every categorical variable to numerical using dummy variables
dmy_xgm <- dummyVars("outcomeGame.team1 ~ .", data = xgm, fullRank = T)
trans_xgm <- data.frame(predict(dmy_xgm, newdata = xgm))

# append the y variable
trans_xgm$outcomeGame.team1 <- xgm$outcomeGame.team1

# spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(trans_xgm$outcomeGame.team1, p=0.75, list=FALSE)
xgmTrain <- trans_xgm[ index,]
xgmTest <- trans_xgm[-index,]


#### SET HYPERPARAMETERS ####
set.seed(2020)
# cross validation
ctrl <- trainControl(
  method = 'repeatedcv',          # k-fold cross validation
  number = 10,                    # number of folds
  savePredictions = 'final',      # saves predictions for optimal tuning parameter
  classProbs = T,                 # should class probabilities be returned
)


#### VARIABLE SELECTION ####
outcomeName <- "outcomeGame.team1"
rfe.vars <- c(
  "locationGame.team1.H",
  "net_rtg.team2",
  "net_rtg.team1",
  "winpct.team1",
  "winpct.team2",
  "ptsTeam.own.agg.per100.team1",
  "ptsTeam.own.agg.per100.team2",
  "stlTeam.own.agg.per100.team1",
  "blkTeam.own.agg.per100.team2",
  "fgpct.own.agg.team2",
  "fgpct.own.agg.team1",
  "fg2pct.own.agg.team2",
  "fg2pct.own.agg.team1",
  "fg2pct.opp.agg.team2",
  "fgmTeam.own.agg.per100.team1",
  "astTeam.opp.agg.per100.team2",
  "fgmTeam.own.agg.per100.team2",
  "fgpct.opp.agg.team1",
  "fg2pct.opp.agg.team1",
  "fgpct.opp.agg.team2"
)

step.vars <- c(
  "locationGame.team1.H", "isB2B.own.team1TRUE", "isB2BFirst.own.team1TRUE", "isB2BSecond.opp.team1TRUE", "fgmTeam.own.agg.per100.team1",
  "fgaTeam.own.agg.per100.team1", "fg3mTeam.own.agg.per100.team1", "fg3aTeam.own.agg.per100.team1", "ftmTeam.own.agg.per100.team1", "drebTeam.own.agg.per100.team1",
  "astTeam.own.agg.per100.team1", "stlTeam.own.agg.per100.team1", "blkTeam.own.agg.per100.team1", "tovTeam.own.agg.per100.team1", "fg3aTeam.opp.agg.per100.team1",
  "drebTeam.opp.agg.per100.team1", "astTeam.opp.agg.per100.team1", "stlTeam.opp.agg.per100.team1", "blkTeam.opp.agg.per100.team1", "tovTeam.opp.agg.per100.team1"
)

top50.vars <- c(
  "locationGame.team1.H",
  "net_rtg.team2",
  "net_rtg.team1",
  "winpct.team1",
  "winpct.team2",
  "ptsTeam.own.agg.per100.team1",
  "ptsTeam.own.agg.per100.team2",
  "ptsTeam.opp.agg.per100.team1",
  "ptsTeam.opp.agg.per100.team2",
  "fgpct.own.agg.team2",
  "fgpct.own.agg.team1",
  "fg2pct.own.agg.team2",
  "fg2pct.own.agg.team1",
  "fg2pct.opp.agg.team2",
  "fgmTeam.own.agg.per100.team1",
  "astTeam.opp.agg.per100.team2",
  "fgmTeam.own.agg.per100.team2",
  "fgpct.opp.agg.team1",
  "fg2pct.opp.agg.team1",
  "fgpct.opp.agg.team2",
  "astTeam.opp.agg.per100.team1",
  "fgmTeam.opp.agg.per100.team2",
  "astTeam.own.agg.per100.team2",
  "astTeam.own.agg.per100.team1",
  "drebTeam.opp.agg.per100.team1",
  "fgmTeam.opp.agg.per100.team1",
  "drebTeam.own.agg.per100.team2",
  "trebTeam.opp.agg.per100.team2",
  "trebTeam.opp.agg.per100.team1",
  "fg3mTeam.own.agg.per100.team1",
  "fg3mTeam.own.agg.per100.team2",
  "drebTeam.own.agg.per100.team1",
  "fg3pct.own.agg.team1",
  "fg3pct.own.agg.team2",
  "tovTeam.own.agg.per100.team2",
  "drebTeam.opp.agg.per100.team2",
  "ftaTeam.opp.agg.per100.team2",
  "fg3aTeam.own.agg.per100.team1",
  "ftmTeam.opp.agg.per100.team2",
  "fg2aTeam.own.agg.per100.team2",
  "fg3mTeam.opp.agg.per100.team2",
  "pfTeam.opp.agg.per100.team2",
  "fg3aTeam.opp.agg.per100.team2",
  "ftmTeam.own.agg.per100.team2",
  "trebTeam.own.agg.per100.team1",
  "fg2mTeam.opp.agg.per100.team1",
  "blkTeam.opp.agg.per100.team2",
  "fg2aTeam.own.agg.per100.team1",
  "fg3aTeam.own.agg.per100.team2",
  "ftaTeam.own.agg.per100.team2"
)


#### MODEL BUILDING - YTD ####
# logistic - rfe
mod_log_rfe_ytd <- train(ytdTrain[,rfe.vars],
                         ytdTrain[,outcomeName],
                         trControl = ctrl,
                         method='glm',
                         family=binomial())

# logistic - step
mod_log_step_ytd <- train(ytdTrain[,step.vars],
                          ytdTrain[,outcomeName],
                          trControl = ctrl,
                          method='glm',
                          family=binomial())

# random forest
system.time({
mod_rf_ytd <- train(ytdTrain[,top50.vars],
                          ytdTrain[,outcomeName],
                          trControl = ctrl,
                          method='rf')

})
# top 50
#     user   system  elapsed 
# 1393.562   20.673 1442.968 

# all vars
#     user   system  elapsed 
# 3137.039   41.365 3248.301

# xgboost
system.time({
  mod_xgb_ytd50 =  train(ytdTrain %>% dplyr::select(top50.vars),
                         ytdTrain[,outcomeName],
                         trControl = ctrl,
                         method='xgbTree')
})
# top50 vars
#     user   system  elapsed 
# 1604.607    8.893 1621.218 

# rfe vars
#    user  system elapsed 
# 742.623   6.591 757.280



#### MODEL BUILDING - XGM ####
# logistic - rfe
mod_log_rfe_xgm <- train(xgmTrain[,rfe.vars],
                       xgmTrain[,outcomeName],
                       trControl = ctrl,
                       method='glm',
                       family=binomial())

# logistic - step
mod_log_step_xgm <- train(xgmTrain[,step.vars],
                          xgmTrain[,outcomeName],
                          trControl = ctrl,
                          method='glm',
                          family=binomial())

# random forest
system.time({
mod_rf_xgm <- train(xgmTrain[,top50.vars],
                          xgmTrain[,outcomeName],
                          trControl = ctrl,
                          method='rf')
})
# top50 
# user   system  elapsed 
# 1420.071   20.670 1454.628 

#     user   system  elapsed 
# 3194.547   36.910 3303.974

# xgboost
system.time({
  mod_xgb_xgm =  train(xgmTrain %>% dplyr::select(top50.vars),
                         xgmTrain[,outcomeName],
                         trControl = ctrl,
                         method='xgbTree')
})
# top 50


# all vars
#    user  system elapsed 
# 735.098   9.738 756.824 


#### FEATURE IMPORTANCE ####
# variable importance
varImp_log.rfe.ytd <- varImp(object = mod_log_rfe_ytd)
varImp_log.step.ytd <- varImp(object = mod_log_step_ytd)
varImp_log.rfe.xgm <- varImp(object = mod_log_rfe_xgm)
varImp_log.step.xgm <- varImp(object = mod_log_step_xgm)
varImp_rf.ytd <- varImp(object = mod_rf_ytd)
varImp_rf.xgm <- varImp(object = mod_rf_xgm)
varImp_xgb.ytd <- varImp(object = mod_xgb_ytd)
varImp_xgb.xgm <- varImp(object = mod_xgb_xgm)  

plot(varImp_log.rfe.ytd, main="Logistic Regression - Variable Importance (via RFE) - YTD", top = 15)
plot(varImp_log.step.ytd, main="Logistic Regression - Variable Importance (via Step) - YTD", top = 15)
plot(varImp_log.rfe.xgm, main="Logistic Regression - Variable Importance (via RFE) - 8gm", top = 15)
plot(varImp_log.step.xgm, main="Logistic Regression - Variable Importance (via Step) - 8gm", top = 15)
plot(varImp_rf.ytd, main="Random Forest - Variable Importance - YTD", top = 15)
plot(varImp_rf.xgm, main="Random Forest - Variable Importance - 8GM", top = 15)
plot(varImp_xgb.ytd, main="XGBoost - Variable Importance - YTD", top = 15)
plot(varImp_xgb.xgm, main="XGBoost - Variable Importance - 8GM", top = 15)


### MODEL COMPARISON ####
### Compare all model results
results <- resamples(list("Log.Step.ytd" = mod_log_step_ytd, 
                          "Log.RFE.ytd" = mod_log_rfe_ytd,
                          "Log.Step.xgm" = mod_log_step_xgm, 
                          "Log.RFE.xgm" = mod_log_rfe_xgm,
                          "Rf.ytd" = mod_rf_ytd,
                          "Rf.xgm" = mod_rf_xgm,
                          "Xgb.ytd" = mod_xgb_ytd,
                          "Xgb.xgm" = mod_xgb_xgm
                         ))

# summarize the distributions
summary(results)

# boxplots of results
bwplot(results , 
       par.strip.text = list(cex=2),
       scales=list(title = list(cex=2), x=list(cex=2), y=list(cex=2)))


#### CHECKING TEST SET ####
# predictions
pred.log_rfe_ytd <- predict(object = mod_log_rfe_ytd, ytdTest, type="raw")
pred.log_step_ytd <- predict(object = mod_log_step_ytd, ytdTest, type="raw")
pred.log_rfe_xgm <- predict(object = mod_log_rfe_xgm, xgmTest, type="raw")
pred.log_step_xgm <- predict(object = mod_log_step_xgm, xgmTest, type="raw")
pred.rf_ytd <- predict(object = mod_rf_ytd, ytdTest, type="raw")
pred.rf_xgm <- predict(object = mod_rf_xgm, xgmTest, type="raw")
pred.xgb_ytd <- predict(object = mod_xgb_ytd, ytdTest[,top50.vars], type="raw")
pred.xgb_xgm <- predict(object = mod_xgb_xgm, xgmTest[,top50.vars], type="raw")

# confusion matrix
c_mtx.lry <- confusionMatrix(pred.log_rfe_ytd, ytdTest[,outcomeName])
c_mtx.lsy <- confusionMatrix(pred.log_step_ytd, ytdTest[,outcomeName])
c_mtx.lrx <- confusionMatrix(pred.log_rfe_xgm, xgmTest[,outcomeName])
c_mtx.lsx <- confusionMatrix(pred.log_step_xgm, xgmTest[,outcomeName])
c_mtx.rfy <- confusionMatrix(pred.rf_ytd, ytdTest[,outcomeName])
c_mtx.rfx <- confusionMatrix(pred.rf_xgm, xgmTest[,outcomeName])
c_mtx.xgby <- confusionMatrix(pred.xgb_ytd, ytdTest[,outcomeName])
c_mtx.xgbx <- confusionMatrix(pred.xgb_xgm, xgmTest[,outcomeName])

results_df <- data.frame(
  "ModelType" = c("Logistic - RFE",  "Logistic - Step",
                  "Logistic - RFE",  "Logistic - Step",
                  "Random Forest", "Random Forest",
                  "XGBoost", "XGBoost"),
  "AggType" = c("YTD", "YTD",
                "8GM", "8GM",
                "YTD", "8GM",
                "YTD", "8GM"),
  "Accuracy" = c(c_mtx.lry$overall[1],  c_mtx.lsy$overall[1],
                 c_mtx.lrx$overall[1],  c_mtx.lsx$overall[1],
                 c_mtx.rfy$overall[1],  c_mtx.rfx$overall[1],
                 c_mtx.xgby$overall[1], c_mtx.xgbx$overall[1]),
  "Accuracy95CI" = c(paste0("[",round(c_mtx.lry$overall[3],4),",",round(c_mtx.lry$overall[4],4),"]"),
                        paste0("[",round(c_mtx.lsy$overall[3],4),",",round(c_mtx.lsy$overall[4],4),"]"),
                        paste0("[",round(c_mtx.lrx$overall[3],4),",",round(c_mtx.lrx$overall[4],4),"]"),
                        paste0("[",round(c_mtx.lsx$overall[3],4),",",round(c_mtx.lsx$overall[4],4),"]"),
                        paste0("[",round(c_mtx.rfy$overall[3],4), ",",round(c_mtx.rfy$overall[4],4),"]"),
                        paste0("[",round(c_mtx.rfx$overall[3],4),",",round(c_mtx.rfx$overall[4],4),"]"),
                        paste0("[",round(c_mtx.xgby$overall[3],4),",",round(c_mtx.xgby$overall[4],4),"]"),
                        paste0("[",round(c_mtx.xgbx$overall[3],4),",",round(c_mtx.xgbx$overall[4],4),"]")),
  "Kappa" = c(c_mtx.lry$overall[2], c_mtx.lsy$overall[2],
              c_mtx.lrx$overall[2], c_mtx.lsx$overall[2],
              c_mtx.rfy$overall[2], c_mtx.rfx$overall[2],
              c_mtx.xgby$overall[2], c_mtx.xgbx$overall[2]
              ),
  "Precision" = c(round(c_mtx.lry$byClass[5],3), round(c_mtx.lsy$byClass[5],4),
           round(c_mtx.lrx$byClass[5],3), round(c_mtx.lsx$byClass[5],4),
           round(c_mtx.rfy$byClass[5],3), round(c_mtx.rfx$byClass[5],4),
           round(c_mtx.xgby$byClass[5],3), round(c_mtx.xgbx$byClass[5],4)
  ),
  "Recall" = c(round(c_mtx.lry$byClass[6],3), round(c_mtx.lsy$byClass[6],4),
           round(c_mtx.lrx$byClass[6],3), round(c_mtx.lsx$byClass[6],4),
           round(c_mtx.rfy$byClass[6],3), round(c_mtx.rfx$byClass[6],4),
           round(c_mtx.xgby$byClass[6],3), round(c_mtx.xgbx$byClass[6],4)
  ),
  "F1" = c(round(c_mtx.lry$byClass[7],3), round(c_mtx.lsy$byClass[7],4),
           round(c_mtx.lrx$byClass[7],3), round(c_mtx.lsx$byClass[7],4),
           round(c_mtx.rfy$byClass[7],3), round(c_mtx.rfx$byClass[7],4),
           round(c_mtx.xgby$byClass[7],3), round(c_mtx.xgbx$byClass[7],4)
  )
)
ordered_results <- results_df[order(results_df$ModelType, results_df$AggType),]
ordered_results$Accuracy <- round(ordered_results$Accuracy,3)
ordered_results$Kappa <- round(ordered_results$Kappa,3)
row.names(ordered_results) <- NULL
ordered_results 


#### TEST COMPARISON PLOT ####
# melt the data frame for plotting
data.melt <- melt(results_df, id.vars=c('ModelType','AggType'))

# plot bar plot
ggplot(data.melt, aes(x = ModelType, y= value, fill = AggType)) +   
  geom_bar(position = "dodge", alpha = 0.6, stat="identity") +
  facet_grid(.~variable) + 
  scale_fill_manual(name = "Aggregation Method:",
                    values=c("#2774AE","#FFD100"),
                    labels = c("8-gm Span", "Year-to-Date")) +
  ggtitle("Accuracy and Kappa Results for the Testing Set") + 
  ylim(c(0,.8))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_blank(),
        strip.text.x = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.position = 'bottom')


#### VALIDATION SET ####
# choose best model
mod_best <- mod_log_rfe_ytd

# remove id variables
ytd1920 <- ytd1920_full %>% dplyr::select(-slugSeason, -idGame, -slugTeam, -slugMatchup)

# converting every categorical variable to numerical using dummy variables
dmy_val_ytd <- dummyVars("outcomeGame.team1 ~ .", data = ytd1920, fullRank = T)
trans_val_ytd <- data.frame(predict(dmy_val_ytd, newdata = ytd1920))

# append the y variable
trans_val_ytd$outcomeGame.team1 <- ytd1920$outcomeGame.team1

# predictions
pred_prob_1920 <- predict(object = mod_best, trans_val_ytd, type="prob")
pred_raw_1920 <- predict(object = mod_best, trans_val_ytd, type="raw")

# confusion Matrix
confusionMatrix(pred_raw_1920, ytd1920[,"outcomeGame.team1"])

# save results
gamePreds_1920 <- cbind(pred_prob_1920, ytd1920_full$idGame)
names(gamePreds_1920) <- c("logitPredW", "logitPredL", "idGame")
preds1920_best <- left_join(orig1920_ytd, gamePreds_1920, "idGame")

#write.csv(preds1920_best , "best-preds-ytd.csv", row.names = F, na = "")








