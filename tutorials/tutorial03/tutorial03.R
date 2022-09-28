########################################################
# Tutorial 3: Contingency Tables and Chi-squared Tests #
########################################################

## Packages
library(tidyverse) # load our packages here

## Import data
dat <- read.csv("movies.csv")

## Explore data
str(dat)
head(dat)
summary(dat)

#######################
# Wrangling the dataset
#######################

# Explore the `genre` column. How many unique values does it 
# have? Try using the as.factor() function to transform it to
# a factor.

# Explore the `top200_box` column. Try using the as.logical() 
# function to transform it. What goes wrong?

# The ifelse() function can be useful for transforming values,
# which then allows us to transform the class of the vector.
# Read the help file on ifelse() and try to use it on 
#`top200_box` to transform "No" to FALSE and "Yes" to TRUE.

######################
# Exploratory Analysis
######################

# The file below is a pre-wrangled version of `movies.csv`. You
# can inspect the script used to make it, `data_wrangling.R`, in
# your own time.
dat <- readRDS("movies.RDS")

## Making contingency tables
# To create a contingency table, we use the `table()` function.
# We can also wrap the call in the `with()` function to avoid
# having to call the $ operator.
with(dat, table(genre, critics_rating))

# If we want to add margins to the table, we can use the 
# addmargins() function as a wrapper to table().
with(dat, addmargins(table(genre, critics_rating)))

# The prop.table() function is a wrapper of table() that converts 
# raw counts to proportions. It has an argument, `margin =`, 
# to toggle between row props and column props. If left blank, 
# it returns the overall prop (i.e proportion of total sum).

# Proportion along the rows:
with(dat, prop.table(table(genre, critics_rating), margin = 1))

# Try to find the proportion along the columns:


# Total proportion:
with(dat, prop.table(table(genre, critics_rating)))

# Note: the round() function is often useful here:
with(dat, round(prop.table(table(genre, critics_rating), 
                           margin = 1), 
                digits = 2))

## Subsetting our data
# There are quite a lot of different movie genres, and many 
# are relatively sparse categories. Let's focus on just five 
# categories: "Action & Adventure", "Comedy", "Documentary", 
# "Drama", and "Mystery & Suspense". We can use the subset() 
# function to subset, or filter, the dataset appropriately.

dat_mini <- subset(dat, dat$genre %in% c("Action & Adventure", 
                                         "Comedy", 
                                         "Documentary",
                                         "Drama", 
                                         "Mystery & Suspense"))

# Run the code below. What is wrong with the output?
with(dat_mini, table(genre, critics_rating))

# Casting or coercing data from one class to another can have
# unintended consequences. 
with(dat_mini, levels(genre))

# Even though we filtered our data to exclude certain 
# observations, the underlying levels still exist. To get rid of
# these, we need to use the droplevels() function.
dat_mini$genre <- droplevels(dat_mini$genre)

with(dat_mini, table(genre, critics_rating))

###############
# Visualisation
###############

# A good way of visualising the frequency of categorical 
# variables is to use a barplot. Base R contains the 
# `barplot()` function, which we will use today. 

# The barplot function requires we input our values in the
# form of a matrix.
mat <- as.matrix(with(dat_mini, table(genre, critics_rating)),
                 nrows = 5)

# Look at the output of this code. Is there a better way of
# visualising our data?
barplot(height = mat, 
        beside = TRUE, 
        legend.text = TRUE,
        args.legend = list(x = "topleft", 
                           cex = 0.4, 
                           box.col = "white"))

# Let's try using prop.table to get a proportional picture
mat_p <- as.matrix(prop.table(table(dat_mini$genre, 
                                    dat_mini$critics_rating),
                              margin = 1),
                   nrows = 5)

barplot(height = mat_p, 
        beside = TRUE, 
        legend.text = TRUE,
        args.legend = list(x = "topleft", 
                           cex = 0.4, 
                           box.col = "white"))

# Is this a better visualisation? Does anything about the data
# strike you?

# Let's save that last plot for use in our Latex file.
png(filename = "barplot.png",
    width = 600,
    height = 350)
barplot(height = mat_p, 
        beside = TRUE, 
        main = "Critics Rating by Genre",
        legend.text = TRUE,
        args.legend = list(x = "topright", 
                           cex = 1, 
                           box.col = "white"))
dev.off()

##########################
# Testing for Significance
##########################

# Look at the help file for the chisq.test() function. How does 
# it work? Let's call it on the contingency table we used for 
# the bar plot above.

chi <- 

# Remember, when we assign the result of a test to an object,
# we can then access all the information which belongs to that
# object.
chi
ls(chi)
chi$residuals

# How do you interpret these results? 