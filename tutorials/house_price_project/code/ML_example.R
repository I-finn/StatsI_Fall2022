#################################################
# Machine learning model with gradient boosting #
#################################################

### Using the code below, we are able to achieve an RMSE on
### the training set of $109,481 (this may differ on your
### machine). In the test set, the RMSE is $186,503. This
### suggests the model is possibly overfitted, and we may
### need to further optimise certain parameters.

# load packages
library(xgboost)
library(vtreat)
library(tidyverse)

## load data
train <- readRDS("data/train.rds")
test <- readRDS("data/test.rds")

## prepare data
# we perform identical transformation on the training and test set
train$ZipCode_c <- as.character(train$ZipCode) # Each zipcode to be added as a separate category
train$SqFtLot_l <- log(train$SqFtLot) # Log transformation of all area variables
train$SqFtTotLiving_l <- log(train$SqFtTotLiving)
train$SqFtFinBasement_l <- log(train$SqFtFinBasement)
test$ZipCode_c <- as.character(test$ZipCode)
test$SqFtLot_l <- log(test$SqFtLot)
test$SqFtTotLiving_l <- log(test$SqFtTotLiving)
test$SqFtFinBasement_l <- log(test$SqFtFinBasement)
# create a character vector of predictor variables
predictors <- c("SqFtTotLiving_l", 
                "SqFtLot_l",
                "SqFtFinBasement_l",
                "NbrLivingUnits",
                "NewConstruction",
                "Bathrooms", 
                "Bedrooms", 
                "BldgGrade", 
                "PropertyType",
                "ZipCode_c",
                "TrafficNoise",
                "YrBuilt",
                "YrRenovated")
# xgboost requires that our predictors are added as a matrix.
# These functions from the vtest package transform non-numeric
# variables to dummy variables.
treatplan <- designTreatmentsZ(train, predictors)
scoreFrame <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    select(varName, origName, code)

newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)

train.treat <- prepare(treatplan, train, varRestriction = newvars)
test.treat <- prepare(treatplan, test, varRestriction = newvars)

# The xgb.Dmatrix transforms our predictors to a sparse matrix, and
# identifies our outcome variable using the label = argument.
# The outcome variable is also log transformed
dtrain <- xgb.DMatrix(data = as.matrix(train.treat[,newvars]), label = log(train$AdjSalePrice))
dtest <- xgb.DMatrix(data = as.matrix(test.treat[,newvars]), label = log(test$AdjSalePrice))

## Run model  
# First run cross validation to optimise model parameters
cv <- xgb.cv(data = dtrain,
             nrounds = 100,
             nfold = 5, # Number of k folds for cross validation
             objective = "reg:squarederror",
             eta = 0.3, # The learning function for avoiding overfitting
             max_depth = 6, # Maximum branch depth for gradiant boosting
             early_stopping_rounds = 10,
             verbose = 0)

# The best_iteration item in the cv object provides the optimal nrounds
mod_xgb <- xgboost(data = dtrain,
                   nrounds = 100,
                   objective = "reg:squarederror",
                   eta = 0.3,
                   depth = 6,
                   verbose = 0)

## Evaluate model
# predict on training set
train$predict <- predict(mod_xgb, dtrain)

# calculate RMSE
train %>%
  mutate(residuals = AdjSalePrice - exp(predict)) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

# predict on test set
test$predict <- predict(mod_xgb, dtest)

# calculate RMSE
test %>%
  mutate(residuals = AdjSalePrice - exp(predict)) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

# plot training set
ggplot(train, aes(x = exp(predict), y = AdjSalePrice)) + 
  geom_point() + 
  geom_abline()

# plot test set
ggplot(test, aes(x = exp(predict), y = AdjSalePrice)) + 
  geom_point() + 
  geom_abline()
