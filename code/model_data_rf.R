## model 1 - random forest ##
# source - tidymodels

setwd("~/Documents/UCLA MAS/Thesis/repo-backup/uclathesis/")
library("dplyr")
#library("tidymodels")
#library("parsnip")
#library("caret")

nba_5gm <- read.csv("data/cleaned-data/new_nba_xgm_matchup.csv", stringsAsFactors = F, na.strings = "NA")
nba_ytd <- read.csv("data/cleaned-data/new_nba_ytd_matchup.csv", stringsAsFactors = F, na.strings = "NA")

# transform asb variable into binary
#nba_5gm$before_asb <- ifelse(nba_5gm$asb_prepost == "pre", T, F)
nba_ytd$beforeASB.team1 <- ifelse(nba_ytd$asb_prepost.team1 == "pre", T, F)
nba_ytd$beforeASB.team2 <- ifelse(nba_ytd$asb_prepost.team2 == "pre", T, F)
nba_ytd <- nba_ytd %>% select(-asb_prepost.team1, -asb_prepost.team2)

# make team1/away into a factor
nba_ytd <- nba_ytd %>% 
            mutate(locationGame.team1 = factor(locationGame.team1, levels = c("A", "H")))
nba_ytd <- nba_ytd %>% 
            mutate(locationGame.team2 = factor(locationGame.team2, levels = c("A", "H")))

# make win/loss into a factor
nba_ytd <- nba_ytd %>% 
  mutate(outcomeGame.team1 = factor(outcomeGame.team1, levels = c("W", "L")))

# remove away outcome variable
nba_ytd <- nba_ytd %>% select(-outcomeGame.team2)

# test model on single season smaller data set
nba19_ytd <- nba_ytd[nba_ytd$slugSeason == '2018-19',]
apply(nba19_ytd, 2, function(x) any(is.na(x)))

# remove unneeded variables
nba19_ytd_mod <- nba19_ytd %>% select(-slugSeason, -newGameID, -idTeam.team1, -idOpp, -slugOpp, 
                                      -teamML.team1, -oppML.team1, -teamWinProb.team1,
                                      -oppWinProb.team1, idTeam.team2, -teamML.team2, -oppML.team2, 
                                      -teamWinProb.team2, -oppWinProb.team2)

nba_mod <- nba19_ytd_mod

## CARET MODEL
# https://www.machinelearningplus.com/machine-learning/caret-package/
# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

# load caret package
library(caret)

# structure of df
str(nba_mod)

# create training/test datasets
set.seed(2020)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(nba_mod$outcomeGame.team1, p=0.75, list=FALSE)

# Step 2: Create the training  dataset
trainData <- nba_mod[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- nba_mod[-trainRowNumbers,]

# Store X and Y for later use.
x = trainData %>% select(-outcomeGame.team1)
y = trainData$outcomeGame.team1

trainData <- trainData %>% select (-idGame, -slugTeam, -idTeam.team2)

#library(skimr)
#skimmed <- skim_to_wide(trainData)
#View(skimmed[,c(1:10)])

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(outcomeGame.team1 ~ ., data=trainData)

# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)

# # Convert to dataframe
trainData <- data.frame(trainData_mat)

# # See the structure of the new dataset
str(trainData)

# append the y variable
trainData$outcomeGame.team1 <- y

## TRANSFORMATION??

## VARIABLE IMPORTANCE

# feature box plots
featurePlot(x = trainData[, 1:15], 
            y = trainData$outcomeGame.team1, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# feature densities
featurePlot(x = trainData[, 1:15], 
            y = trainData$outcomeGame.team1, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# recursive feature elimination (rfe)
#library(e1071)
set.seed(2020)
options(warn=-1)

subsets <- c(1:10, 20, 50, 100)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[, 1:ncol(trainData)-1], y=trainData$outcomeGame.team1,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold, repeated 5 times) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy    Kappa AccuracySD KappaSD Selected
# 1   0.5279 0.002484    0.04095 0.07942         
# 2   0.5729 0.096843    0.04438 0.09231         
# 3   0.5841 0.112954    0.04242 0.09011         
# 4   0.5909 0.129675    0.04727 0.09931         
# 5   0.5967 0.137976    0.04826 0.10283         
# 6   0.6138 0.172728    0.05125 0.11042         
# 7   0.6149 0.174317    0.04432 0.09751         
# 8   0.6213 0.185759    0.04635 0.09737         
# 9   0.6249 0.195827    0.04421 0.09510         
# 10   0.6329 0.210185    0.04074 0.08763         
# 20   0.6451 0.235636    0.04170 0.08959         
# 50   0.6436 0.230969    0.03980 0.08416         
# 100   0.6401 0.220364    0.03742 0.08353         
# 144   0.6470 0.232101    0.04264 0.09730        *
#   
#   The top 5 variables (out of 144):
#   winpct.home, net_rtg.home, ptsTeam.own.agg.per100.home, drebTeam.own.agg.per100.away, astTeam.opp.agg.per100.home


## TRAIN & TUNE MODEL

# MARS model
# Set the seed for reproducibility
set.seed(2020)

# Train the model using randomForest and predict on the training data itself.
model_mars = train(outcomeGame.team1 ~ ., data=trainData, method='earth')
fitted <- predict(model_mars)

# plot accuracy
plot(model_mars, main="Model Accuracies with MARS")

# plot var importance
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")



## PREPARE TEST DATASET

# Step 1: Impute missing values 
# testData2 <- predict(preProcess_missingdata_model, testData)  

# Step 2: Create one-hot encodings (dummy variables)
testData2 <- predict(dummies_model, testData)

# Step 3: Transform the features to range between 0 and 1
# testData3 <- predict(preProcess_range_model, testData2)

# View
head(testData2[, 1:10])

# predict on testData
predicted <- predict(model_mars, testData2)
head(predicted)


# confusion matrix
confusionMatrix(reference = testData$outcomeGame.team1, data = predicted, mode='everything', positive='W')

# set up train control
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 

# hyper parameter tuning with tuneLength
# Step 1: Tune hyper parameters by setting tuneLength
set.seed(2020)
model_mars2 = train(outcomeGame.team1 ~ ., data=trainData, method='earth', tuneLength = 5, metric='ROC', trControl = fitControl)
model_mars2

# Step 2: Predict on testData and Compute the confusion matrix
predicted2 <- predict(model_mars2, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = predicted2, mode='everything', positive='W')

# hyper parameter tuning with tuneGrid
# Step 1: Define the tuneGrid
marsGrid <-  expand.grid(nprune = c(2, 4, 6, 8, 10), 
                         degree = c(1, 2, 3))

# Step 2: Tune hyper parameters by setting tuneGrid
set.seed(2020)
model_mars3 = train(outcomeGame.team1 ~ ., data=trainData, method='earth', metric='ROC', tuneGrid = marsGrid, trControl = fitControl)
model_mars3

# Step 3: Predict on testData and Compute the confusion matrix
predicted3 <- predict(model_mars3, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = predicted3, mode='everything', positive='W')


### DIFFERENT MODELS

# Adaboost
set.seed(2020)
model_adaboost = train(outcomeGame.team1 ~ ., data=trainData, method='adaboost', tuneLength=2, trControl = fitControl, metric="Accuracy")
model_adaboost
# plot var importance
varimp_adaboost <- varImp(model_adaboost)
plot(varimp_adaboost, top = 50, main="Variable Importance with Adaboost")
# confmatx
pred_ada <- predict(model_adaboost, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = pred_ada, mode='everything', positive='W')


# Random Forest
set.seed(2020)
model_rf = train(outcomeGame.team1 ~ ., data=trainData, metric = "Accuracy", method='rf', tuneLength=5, trControl = fitControl)
model_rf
# plot accuracy
plot(model_rf, metric="Sens", main="Model Accuracies with Random Forest")
# plot var importance
varimp_rf <- varImp(model_rf)
plot(varimp_rf, top = 50, main="Variable Importance with Random Forest")
# confmatx
pred_rf <- predict(model_rf, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = pred_rf, mode='everything', positive='W')


# xgBoost DART
set.seed(2020)
model_xgb = train(outcomeGame.team1 ~ ., data=trainData, method='xgbDART', tuneLength=5, trControl = fitControl, verbose=F)
model_xgb
# plot var importance
varimp_xgb <- varImp(model_xgb)
plot(varimp_xgb, top = 50, main="Variable Importance with xgBoost")
# confmatx
pred_xgb <- predict(model_xgb, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = pred_xgb, mode='everything', positive='W')


# SVM
set.seed(2020)
model_svm = train(outcomeGame.team1 ~ ., data=trainData, method='svmRadial', tuneLength=15, trControl = fitControl)
model_svm
# plot var importance
varimp_svm <- varImp(model_svm)
plot(varimp_svm, top = 50, main="Variable Importance with SVM")
# confmatx
pred_svm <- predict(model_svm, testData2)
confusionMatrix(reference = testData$outcomeGame.team1, data = pred_svm, mode='everything', positive='W')



# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf))#, XGBDART=model_xgb, MARS=model_mars3))#, SVM=model_svmRadial))

# Summary of the models performances
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)






####### TIDY MODELS
#[,-c(1,2,4,6,10,17,18,19,20)]

# train test split
set.seed(2020)
nba_split <- initial_split( nba19_ytd_mod %>% select(-idGame, -slugTeam) , strata = outcomeGame.team1)

# 75/25
nba_train <- training(nba_split)
nba_test  <- testing(nba_split)
nrow(nba_train)
nrow(nba_train)/nrow(nba19_ytd_mod)

# training set proportions by class
nba_train %>% 
  count(outcomeGame.team1) %>% 
  mutate(prop = n/sum(n))

# test set proportions by class
nba_test %>% 
  count(outcomeGame.team1) %>% 
  mutate(prop = n/sum(n))


# prep model
rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(2020)
rf_fit <- 
  rf_mod %>% 
  fit(outcomeGame.team1 ~ ., data = nba_train %>% select(-idGame, -slugTeam))
rf_fit

# estimating performance
rf_training_pred <- 
  predict(rf_fit, nba_train) %>% 
  bind_cols(predict(rf_fit, nba_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(nba_train %>% 
              select(outcomeGame.team1))

# test results
rf_training_pred %>%                # training set predictions
  roc_auc(truth = outcomeGame.team1, .pred_W)

# accuracy
rf_training_pred %>%                # training set predictions
  accuracy(truth = outcomeGame.team1, .pred_class)


rf_testing_pred <- 
  predict(rf_fit, nba_test) %>% 
  bind_cols(predict(rf_fit, nba_test, type = "prob")) %>% 
  bind_cols(nba_test %>% select(outcomeGame.team1))


rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = outcomeGame.team1, .pred_W)

rf_testing_pred %>%                   # test set predictions
  accuracy(truth = outcomeGame.team1, .pred_class)



## using resampling
set.seed(2020)
folds <- vfold_cv(nba_train, v = 10)
folds

rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(outcomeGame.team1 ~ . )

set.seed(2020)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

rf_fit_rs

collect_metrics(rf_fit_rs)



rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = outcomeGame.team1, .pred_W)

rf_testing_pred %>%                   # test set predictions
  accuracy(truth = outcomeGame.team1, .pred_class)



##    Tune model parameters
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tune_spec


tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)


set.seed(2020)
nba_folds <- vfold_cv(nba_train)


set.seed(2020)

tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(outcomeGame.team1 ~ . )

tree_res <- 
  tree_wf %>% 
  tune_grid(
   resamples = nba_folds,
   grid = tree_grid
  )
tree_res

# tree results
tree_res %>% collect_metrics()

# plot results
tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# see best candidates
tree_res %>%
  show_best("roc_auc")
best_tree <- tree_res %>%
  select_best("roc_auc")
best_tree

# finalize model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_wf

# fit to the data
final_tree <- 
  final_wf %>%
  fit(data = nba_train) 

final_tree

# variable importance plot
library(vip)
library(rpart.plot)

final_tree %>% 
  pull_workflow_fit() %>% 
  vip()


# last fit
final_fit <- 
  final_wf %>%
  last_fit(nba_split) 

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  roc_curve(outcomeGame.team1, .pred_W) %>% 
  autoplot()



test_results <- 
  predict(final_tree, nba_test) %>% 
  bind_cols(predict(final_tree, nba_test, type = "prob")) %>% 
  bind_cols(nba_test)# %>% select(outcomeGame.team1))


# confusion matrix
results <-
  tibble(
    actual = nba_test$outcomeGame.team1,
    predicted = predict(final_tree, nba_test)
  )

xx <-final_fit %>%
  collect_predictions()  %>% 
  conf_mat(truth = outcomeGame.team1, estimate = .pred_class)

final_fit %>%
  collect_predictions()  %>%
  ggplot() +
  geom_density(aes(x = .pred_W, fill = outcomeGame.team1), 
               alpha = 0.5)



conf_mat(results, truth = actual, estimate = predicted)[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(Prediction, Truth, alpha = n)) + 
  geom_tile(fill = custom_palette[4], show.legend = FALSE) +
  geom_text(aes(label = n), color = "white", alpha = 1, size = 8) +
  custom_theme +
  labs(
    title = "Confusion matrix"
  )
