################################
# Final model script: Team ucd #
################################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
library(tidyverse)

# Load training data
train <- readRDS("data/train.rds")

# Data transformation

zip_group_pr <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice), 
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

train <- train %>%
  left_join(select(zip_group_pr, ZipCode, ZipGroup), by = "ZipCode")

# Model
mod <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup) + SqFtTotLiving:ZipGroup, 
          data = train,
          na.action = na.omit)

summary(mod)

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
zip_group_pr <- test %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice), 
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

test <- test %>%
  left_join(select(zip_group_pr, ZipCode, ZipGroup), by = "ZipCode")
  
# Run model on test data
test$prediction <- predict(mod, newdata = test)

# Calculate RMSE
test$residuals <- test$AdjSalePrice - test$prediction
(rmse <- sqrt(mean(test$residuals^2)))

# Calculate R^2
mean_y <- mean(test$AdjSalePrice)
tss <- sum((test$AdjSalePrice - mean_y)^2)
rss <- sum((test$AdjSalePrice - test$prediction)^2)
(r_sq <- 1 - (rss/tss))
