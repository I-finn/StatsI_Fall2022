####################################
# Zipcode: clustering on residuals #
####################################

# load packages
library(tidyverse)
library(broom)
library(stargazer)

# load data
dat <- readRDS("data/train.rds")

# run a model
mod1 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade, data = dat)

# we can access the residuals of the model using the resid() function
resid(mod1)

# our initial code for clustering on AdjSalePrice
zip_group_pr <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice), # we need to change this
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group_pr, ZipCode, ZipGroup), by = "ZipCode")

# bind residuals to data
dat <- cbind(dat, residuals = resid(mod1))

# code for clustering on residuals
zip_group_res <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(residuals), # we need to change this
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup_r = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(zip_group_res, ZipCode, ZipGroup_r), by = "ZipCode")

# compare the two approaches...
# as a constant
mod2 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)
mod3 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup_r, data = dat)
stargazer(mod1, mod2, mod3, type = "text")

## note: when adding zipcode as a constant, we need to be careful interpreting
## our constant, as there is no house in our data without zero zipcode effect.
## (The same would go for building grade).

# as categories
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup), data = dat)
mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + as.factor(ZipGroup_r), data = dat)
stargazer(mod1, mod4, mod5, type = "text")

## note: when modelled as a category, the constant now includes the effect of 
## the reference category.

# Which zipcodes are in each category?
# dplyr approach: group_by two variables
dat %>%
  group_by(ZipGroup, ZipCode) %>%
  summarise(n = n())

# make a list using a for loop :(
ZipCode <- list(NULL)
for (i in seq_along(unique(dat$ZipGroup))) {
  ZipCode[[i]] <- unique(dat$ZipCode[dat$ZipGroup == i])
}
