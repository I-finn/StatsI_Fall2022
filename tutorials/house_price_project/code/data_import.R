###########################################
# DATA IMPORT SCRIPT: HOUSE PRICE PROJECT #
###########################################

library(tidyverse) # packages

# read in data
dat <- read.csv("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv",
                sep = "\t")

# transform date column
dat <- dat %>%
  mutate(Ddate = as.Date(dat$DocumentDate))

# add an identifier index
dat <- dat %>%
  mutate(ID = row_number()) %>%
  select(ID, everything()) # reorder to place ID column first

# perform train/test split (one approach)
set.seed(1999) # set the random seed to make results reproducible
index <- runif(nrow(dat)) # create a random vector (uniform distribution) with values between 0 and 1
train <- subset(dat, index < 0.9) # use random vector to subset 90% of observations
test <- subset(dat, index >=0.9) # use random vector to subset 10% of observations
nrow(test) + nrow(train) == nrow(dat) # test our two new subsets add up to the original dataset

# save to file
saveRDS(dat, "data/house_data.rds") # complete data
saveRDS(train, "data/train.rds") # training data
saveRDS(test, "data/test.rds") # hold out data
