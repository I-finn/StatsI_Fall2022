##############################
# Final model script: Team A #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
library(broom)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(dplyr)

# Load training data
train <- readRDS("data/train.rds")

# Data transformation
zip_group <- train %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(SalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))
train <- train %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")
  
  
  # Add here any code necessary to transform variables

# Model Ariana
mod <- lm(AdjSalePrice ~ SqFtTotLiving + YrBuilt, 
          data = train,
          na.action = na.omit)

summary(mod)

# Model Marcus
mod1 <- lm(AdjSalePrice ~ SqFtFinBasement,
  data = train,
  na.action = na.omit)

summary(mod1)

ggplot(train, aes(SqFtFinBasement, AdjSalePrice))+
  geom_point()+
  geom_smooth(method = "lm")

# Outlier Identification
par(mfrow = c(2,2))
plot(mod1)

# Model Natasha 
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = train)

stargazer(mod4, type = "latex")

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
zip_group <- test %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(SalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))
test <- test %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")
  
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
