################################
# Movies data wrangling script #
################################

# Packages
library(tidyverse)
library(lubridate)

# import movies.csv
dat <- read.csv("movies.csv") # Base R function (utils package)

# Convert factors
char_vecs <- sapply(dat, is.character) # Make an index object for subsetting character vectors
unique <- Map(length, lapply(dat[,char_vecs], unique)) # Count the unique values in each character vector
factors <- names(dat[,char_vecs][,unique <= 11 & unique > 2]) # Subset character vectors with more than 2 and fewer than 12 unique values
dat[,factors] <- lapply(dat[,factors], as.factor) # Coerce these to factor

# Relevel factors with implicit order
dat$mpaa_rating <- factor(dat$mpaa_rating, 
                          levels = c("G", "PG", "PG-13", # new level order
                                     "R", "NC-17", "Unrated"))
dat$critics_rating <- factor(dat$critics_rating, 
                             levels = c("Rotten", "Fresh", # new level order
                                        "Certified Fresh"))

# Convert logicals
logical <- names(dat[,char_vecs][,unique == 2]) # Make an index object for logicals
logical <- logical[-1] # Drop one column (audience_rating) which is not logical

dat[,logical] <- ifelse(dat[,logical] == "no", FALSE, TRUE) # Convert "yes"/"no" to TRUE/FALSE
dat[,logical] <- lapply(dat[,logical], as.logical) # Coerce to logical

# Convert audience_rating to factor
dat$audience_rating <- factor(dat$audience_rating, levels = c("Spilled", "Upright"))

# Convert date columns to single column
dat$thtr_rel <- make_date(dat$thtr_rel_year,
                          dat$thtr_rel_month,
                          dat$thtr_rel_day)
dat$dvd_rel <- make_date(dat$dvd_rel_year,
                         dat$dvd_rel_month,
                         dat$dvd_rel_day)

dat <- dat[, !names(dat)  %in% c("thtr_rel_year",
                                 "thtr_rel_month",
                                 "thtr_rel_day",
                                 "dvd_rel_year",
                                 "dvd_rel_month",
                                 "dvd_rel_day")]

saveRDS(dat, "movies.rds")