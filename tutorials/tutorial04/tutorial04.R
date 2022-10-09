##########################################
# Tutorial 4: Correlation and Regression #
##########################################

rm(list=ls())
## Packages
library(ggplot2, psych, tidyverse) # Load our packages here

# bivariate variables & relationships
#In R we will:
  
#  Introduce the advanced plotting capabilities of the ggplot2 package.
#Show how to produce many plots at once using faceting.
#Learn how to use the cor() function to find the correlation of two variables.
#Learn how to use the lm() function to run a linear regression.
#We will also revise the data science skills we have been using in recent weeks, including:
  
#  Working with an R script to perform analysis which is reproducible.
#Saving graphical output to file.
#Using Latex to produce a pdf of our analysis.
#Finally, we will be introducing the statistical concepts of correlation and 
#regression for measuring and quantifying the relationship between two continuous variables.

# tut guide
with (iris, plot(Sepal.Length, Sepal.Width, main = "Plot of sepal length and width for 3 species of iris"))
      
with(iris, plot(Sepal.Length, Sepal.Width,
                col = Species,                     # colour
                pch = c(as.numeric(Species)+5),      # shapes, max = 17?
                main = "Plot of sepal length and width for 3 species of iris"))
head(iris)

# Pearson correlation coefficient
#   bounded : range -1, 1  (positive = increasing together)
with(iris, cor(Sepal.Width, Sepal.Length))  

# (covariance not bounded)


by(iris[,c("Sepal.Length", "Sepal.Width")], 
   iris$Species, 
   function(x) {cor(x$Sepal.Length, x$Sepal.Width)})

# The code above groups our two continuous variables by Species (it uses base
# R’s by() function, together with a lambda or anonymous function; this is much
# more easily done using the dplyr package, which we will introduce next week). 
# As we can see from the output, by controlling for species, the true relationship 
# between sepal length and width is revealed, and goes in the opposite direction 
# to our previous result.

#This finding should encourage us to be careful when exploring bivariate relationships 
# - has our analysis accounted for all important intervening variables? Failing 
# to do so can produce meaningless, or even misleading results.


##### Regression
# This is especially so when we move from correlation to regression. Whereas the 
# coefficient of correlation tells us the strength of the relationship between two 
# variables (i.e. how much of the variation in one variable can be associated with
# variation in another, and whether that relationship is positive or negative), 
# regression provides us with an estimate of the coefficients that best describe 
# that relationship: the intercept and the slope.

# These are typically given in the form of an equation, Y = a + bX, where 
#   Y is the dependent variable, 
#   X the independent variable, 
#   a the intercept, and 
#   b the slope. 
# R’s lm() function performs the necessary calculations to obtain these values, 
# and many others.

lm(Sepal.Width ~ Sepal.Length, # formula is of type Y ~ X! ie Y = f(X)
   data = iris)
lm_iris <- lm(Sepal.Width ~ Sepal.Length, # formula is of type Y ~ X! ie Y = f(X)
   data = iris)


# abline - best fit line for Y~X
with(iris, plot(x=Sepal.Length, y=Sepal.Width,
                main = "Sepal length and width for 3 species of iris"))
abline(lm_iris,
       col = "blue")

summary(lm_iris)

#As we can see, the line intercepts the y axis at 3.42 (or would do, if the 
# x axis began at 0), and slopes gently downwards (-0.06).

#The basic output (or “call”) from the lm() function is sparse: it provides only
# the intercept and the slope coefficients. For this reason, you should always
# assign the output of the lm() function to an object, and then inspect it. 


#The summary() function provides us with much more detailed output: as well as 
# the coefficients, we also have information about the distribution of the residuals 
# (the “bits left over”, or difference between the predicted and actual values), 
# as well as statistics relating to the statistical significance of the model. 
# Here, for instance, we see that the estimate for the intercept, 3.42, is 
# highly statistically significant (***), whereas the estimate for the slope is not. 
# What does this mean?

#Finally, at the bottom we have measures for R-squared and the F-statistic. 
# R-squared, the coefficient of determination, is related to Pearson’s coefficient 
# of correlation, but it should be interpreted a little differently. Simply put, 
# it is the percentage of variation in our dependent variable (sepal width) 
# “explained” by our independent variable(s). Here, this is only 1.4 percent!



### get models for irises by species
by(iris[,c("Sepal.Length", "Sepal.Width")], 
   iris$Species, 
   function(x) {summary(lm(x$Sepal.Width ~ x$Sepal.Length))})

#Again, when we control for species, we get quite different results from our 
# regression models. Can you interpret these results? In particular, how does 
# the significance of the intercept change with respect to the slope? And how 
# about R-squared?

#We should always be cautious when running models! A further example of this are
# the plots at the top of the page, which are taken from the anscombe dataset, a
# teaching dataset in which all four x-y subsets have the same statistical properties
# of mean, variance, correlation and regression line. The blue line in each plot 
# is the regression line: as you can see, despite the very different distributions 
# of the data, they all have the same line!

#-------------------------------------------------------------------------------
## Assign data
dat <- midwest # A built-in dataset of ggplot - so tibble

install.packages("tidyverse")
library(tidyverse)


## Explore data
?midwest                   
# use your own code here to explore
head(midwest)
glimpse(midwest)
class(midwest)

unique(dat$state)
summary(midwest)
str(midwest)


### ggplot

# The ggplot package is probably now the most popular way of plotting in R. 
# "gg" stands for "grammar of graphics", and the package uses a specific 
# syntax for plotting based upon the work of Leland Wilkinson. A good 
# introduction to ggplot is provided in the Wickham/Grolemund "R for Data 
# Science" book, which is available online: https://r4ds.had.co.nz/data-visualisation.html

# A simple ggplot:
ggplot(aes(x = x1, y = y1), # We use the aes() function to supply an x and y argument
       data = anscombe) + # We use the `+` operator to add an additional geom
  geom_point() +
  geom_abline()

# To use ggplot, rather than the plot() function, we call the ggplot() 
# function. We supply to it our x and y arguments *inside* the aes() 
# function, and our data using the argument `data =`. We then *add* the 
# type of plot we want as a *geom* using the + operator. Here, we want a 
# scatter plot, which is geom_point().

# Exercise: using the midwest dataset, create a scatterplot in which the 
# percent college educated is used to predict the percentage of people
# below the poverty line.

ggplot(aes(x = percollege , # add the independent variable
           y = percbelowpoverty), # add the dependent variable
       data = midwest ) + # add the data source
  geom_point() + 
  labs(title = "relationship between college education and poverty", 
       subtitle = "percentages", 
       x = "percentage college educated", 
       y = "percentage below poverty line"
  )


## Correlations
# We have a lot of variables in our midwest dataset, so the pairs() 
# function may not be the best for simultaneously visualising them.
# We might use the `psych` package, and the cor.plot() function, to 
# visualise them instead.

#install.packages("psych")
library(psych)
png("midwest_cor_plot.png")
cor.plot(Filter(is.numeric, dat))             # google how to change x label
dev.off()
# Alternatively, we might take a subset of columns and use pairs() to
# visualise the correlations.

pairs(dat[c("poptotal", "percwhite", "percblack", "perchsd", 
            "percollege", "percprof", "percbelowpoverty")],
      upper.panel = NULL) # What does this argument change?
          # changes from mirrored rectangle, to triangle

# can add correl coefficients - google

# Exercise: from your visual exploration of the scatter plots of 
# correlation, pick two variables to run the cor() function on. 
# Visualise these separately using ggplot, and save the output to file.

with(dat , cor(percbelowpoverty, percollege))             # google how to change x label

ggplot(aes(percbelowpoverty, y=percollege), data = dat) +
  geom_point()


# Code to save your plot (saves last plot)
ggsave("corr_plot.png",
       device = "png",
       dpi = 300)

## Advanced plotting skills
# When we have a lot of observations, it can sometimes be difficult to 
# visualise the distribution using a scatter plot. Equally, when data fall
# along discrete intervals, points get plotted on top of each other and it
# can be hard to gauge density. Finally, it is sometimes the case (as we 
# saw with the iris dataset) that continuous variables are better divided
# up according to categories. Each of these issues can be addressed using
# advanced plotting skills in ggplot.

# 1. Alpha
# We can use the alpha argument to change the shading of our points 
# according to their density.

ggplot(aes(percbelowpoverty, percollege), data = dat) +
  geom_point(alpha = 0.2) # set alpha manually to between 0 and 1

# 2. Jitter
# We can use the position = "jitter" argument to add random noise to the 
# plotting of points, which prevents them from being placed directly on 
# top of each other.

ggplot(aes(percbelowpoverty, state), data = dat) +
  geom_point(
    position = "jitter" # un/comment this line and run again
    )

# 3. Faceting
# We can use the facet_wrap() geom to create separate plots according
# to categorical variables.

ggplot(aes(percbelowpoverty, percollege, 
           colour = state), # Here, we group by colour on the same plot
       data = dat) +
  geom_point(alpha = 0.3)

ggplot(aes(percbelowpoverty, percollege), 
       data = dat) +
  geom_point(alpha = 0.2) +
  facet_wrap(~state) # Here, we facet

ggsave("facet_wrap.png",
       device = "png",
       dpi = 300)

dev.off() # To prevent plotting errors below

## Regression
# Exercise: we were introduced to the lm() function in the tutorial guide.
# Use it below to run a model in which the percent college educated is 
# the independent variable, and percent below poverty is the outcome, or
# dependent variable.

coll_pov <- lm(percbelowpoverty ~ percollege, # add the correct code here 
               data = dat)
summary(coll_pov)

# In base R we can plot this model by adding an abline() call to a 
# scatter plot.
plot(percbelowpoverty ~ percollege, # An alternative way to call the axes
     data = dat,
     main = "College Education and Poverty")
abline(coll_pov, # our regression model
       col = "blue")

# How would we plot this using ggplot?
ggplot(aes(x = percollege , 
           y = percbelowpoverty), 
       data = dat) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

## Regression models
# Remember, when you assign the output of lm() to an object, you get an 
# object of class...
class(coll_pov)

# This contains a lot of useful data
str(coll_pov)

# We can plot our residuals
plot(dat$percollege, 
     resid(coll_pov)) # a convenience function for extracting residuals
abline(h = 0, col = "red")

# With ggplot:
ggplot(aes(midwest$percollege, resid(coll_pov)), 
       data = NULL) +
  geom_point(alpha = 0.4) +
  geom_smooth()

# not a great fit - linear model prob not ideal 

