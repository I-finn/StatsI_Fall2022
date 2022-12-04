##############################
# Final model script: Team C #
##############################

### Note: fill in the code below until the line.
### Make sure your model works as expected by running summary().

# Load any packages here
library(tidyverse)

# Load training data
dat <- readRDS("data/train.rds")

# Initial Model and its residuals.
lm.1 <- lm(dat$AdjSalePrice ~ dat$BldgGrade
           + dat$SqFtTotLiving)
lm.1.res <- resid(lm.1)


# Append residuals to dataset
c(dat, lm.1.res)

# Code for transforming Zip Code
zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(resids = median(lm.1.res),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

# 2nd dataset with Zip Group categorical
dat2 <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

# Final Model with  square foot to living and bldgGrade squared, 
# and zip group.

mod <- lm(AdjSalePrice ~  I(SqFtTotLiving^2) + SqFtTotLiving 
          +  I(BldgGrade^2) + BldgGrade + ZipGroup,
          data = dat2, na.action = na.omit)

summary(mod)

########## do not fill in below the line ###########

# Load test data
test <- readRDS("data/test.rds")

# Transform test data
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
