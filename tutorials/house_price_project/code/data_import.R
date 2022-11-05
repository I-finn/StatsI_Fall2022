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

# save to file
saveRDS(dat, "data/house_data.rds")

# load in data
dat <- readRDS("data/house_data.rds")
