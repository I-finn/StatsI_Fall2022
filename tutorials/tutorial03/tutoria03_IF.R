# tutorial 3 
# 28/9/22


# barplotting  
# generating png files
# auto chi-sq test


# git pull origin main - from cmd line
# ... push ... -> to upload to repository

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############################################################

movies <- read.csv("movies.csv", header=TRUE)
str(movies)
head(movies)

summary(movies)

#######################
# Wrangling the dataset
#######################

# Explore the `genre` column. How many unique values does it 
# have? Try using the as.factor() function to transform it to
# a factor.

genres <- unique(movies$genre)
length(genres)
movies$genres_f <- as.factor(movies$genre)

# Explore the `top200_box` column. Try using the as.logical() 
# function to transform it. What goes wrong?
as.logical(movies$top200_box)       # NAs

# The ifelse() function can be useful for transforming values,
# which then allows us to transform the class of the vector.
# Read the help file on ifelse() and try to use it on 
#`top200_box` to transform "No" to FALSE and "Yes" to TRUE.

unique(movies$top200_box)
movies$top200box_l <-ifelse(movies$top200_box=="no", FALSE, TRUE)


######################################################
######################
# Exploratory Analysis
######################

rm(list = c("genres", "genres_f", "t2box"))

# The file below is a pre-wrangled version of `movies.csv`. You
# can inspect the script used to make it, `data_wrangling.R`, in
# your own time.
movies<- readRDS("movies.RDS")


## Making contingency tables
# To create a contingency table, we use the `table()` function.
# We can also wrap the call in the `with()` function to avoid
# having to call the $ operator.
# table rows - genre, columns - rating (factor), data = cross counts
with(movies, table(genre, critics_rating)) 

# If we want to add margins to the table, we can use the 
# addmargins() function as a wrapper to table().
with(movies, addmargins(table(genre, critics_rating)))

# The prop.table() function is a wrapper of table() that converts 
# raw counts to proportions. It has an argument, `margin =`, 
# to toggle between row props and column props. If left blank, 
# it returns the overall prop (i.e proportion of total sum).

# Proportion along the rows:  # each row sums to 1
with(movies, prop.table(table(genre, critics_rating), margin = 1))
rtab<-with(movies, prop.table(table(genre, critics_rating), margin = 1))
sum(rtab[1,])

# Try to find the proportion along the columns: # each col sums to 1
with(movies, prop.table(table(genre, critics_rating), margin = 2))
ctab<-with(movies, prop.table(table(genre, critics_rating), margin = 2))
sum(ctab[,1])

# Total proportion:   # total table sums to 1
with(movies, prop.table(table(genre, critics_rating)))
ttab<-with(movies, prop.table(table(genre, critics_rating)))

sum(ttab)

# Note: the round() function is often useful here:
with(movies, round(prop.table(table(genre, critics_rating), 
                         margin = 1), 
                          digits = 2))


##################################################################
## Subsetting our data
# There are quite a lot of different movie genres, and many 
# are relatively sparse categories. Let's focus on just five 
# categories: "Action & Adventure", "Comedy", "Documentary", 
# "Drama", and "Mystery & Suspense". We can use the subset() 
# function to subset, or filter, the dataset appropriately.

movies_mini <- subset(movies, movies$genre %in% c("Action & Adventure", 
                                         "Comedy", 
                                         "Documentary",
                                         "Drama", 
                                         "Mystery & Suspense"))

# Run the code below. What is wrong with the output?
with(movies_mini, table(genre, critics_rating))
# includes empty levels

# Casting or coercing data from one class to another can have
# unintended consequences. 
with(movies_mini, levels(genre))

# Even though we filtered our data to exclude certain 
# observations, the underlying levels still exist. To get rid of
# these, we need to use the droplevels() function.
movies_mini$genre <- droplevels(movies_mini$genre)

with(movies_mini, table(genre, critics_rating))
with(movies_mini, round(prop.table(table(genre, critics_rating), margin = 1), 3))

chi_sq_tbl <-with(movies_mini, table(genre, critics_rating))
###############
# Visualisation
###############

# A good way of visualising the frequency of categorical 
# variables is to use a barplot. Base R contains the 
# `barplot()` function, which we will use today. 

# The barplot function requires we input our values in the
# form of a matrix.
mat <- as.matrix(with(movies_mini, table(genre, critics_rating)),
                 nrows = 5)

# Look at the output of this code. Is there a better way of
# visualising our data?
barplot(height = mat, 
        beside = TRUE, 
        main = "raw counts", 
        legend.text = TRUE,
        args.legend = list(x = "topleft", 
                           cex = 0.4, 
                           box.col = "white"))

# Let's try using prop.table to get a proportional picture
mat_p <- as.matrix(prop.table(table(movies_mini$genre, 
                                    movies_mini$critics_rating),
                              margin = 1),
                   nrows = 5)

barplot(height = mat_p, 
        beside = TRUE, 
        legend.text = TRUE,
        main = "proportions, genre sums to 1", 
        args.legend = list(x = "topright", 
                           cex = 0.4,   # text size
                           box.col = "white"))

# Is this a better visualisation? Does anything about the data
# strike you?

# action and comedy disproportionate in rotten
# documentaries disproportionately under in rotten

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


##################################################################
##########################
# Testing for Significance
##########################

# Look at the help file for the chisq.test() function. How does 
# it work? Let's call it on the contingency table we used for 
# the bar plot above.

chi <- chisq.test( chi_sq_tbl)
  
  # Remember, when we assign the result of a test to an object,
  # we can then access all the information which belongs to that
  # object.
  chi

# df = (5-1) * (3-1) = 8
  
ls(chi)
chi$residuals   # pearson residuals
# biggest single contribution = documentary in rotten
# then doc in fresh, comedy in rotten, action in rotten

sort(abs(chi$residuals))   # pearson residuals

# How do you interpret these results? 
# p-value tiny -> reject null
