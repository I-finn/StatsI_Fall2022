##########################################
# Tutorial 4: Correlation and Regression #
##########################################

## Packages
library(ggplot2) # Load our packages here

## Assign data
dat <- midwest # A built-in dataset

## Explore data
?midwest
# use your own code here to explore


### ggplot

# The ggplot package is probably now the most popular way of plotting in R. 
# "gg" stands for "grammar of graphics", and the package uses a specific 
# syntax for plotting based upon the work of Leland Wilkinson. A good 
# introduction to ggplot is provided in the Wickham/Grolemund "R for Data 
# Science" book, which is available online: https://r4ds.had.co.nz/data-visualisation.html

# A simple ggplot:
ggplot(aes(x = x1, y = y1), # We use the aes() function to supply an x and y argument
       data = anscombe) + # We use the `+` operator to add an additional geom
  geom_point()

# To use ggplot, rather than the plot() function, we call the ggplot() 
# function. We supply to it our x and y arguments *inside* the aes() 
# function, and our data using the argument `data =`. We then *add* the 
# type of plot we want as a *geom* using the + operator. Here, we want a 
# scatter plot, which is geom_point().

# Exercise: using the midwest dataset, create a scatterplot in which the 
# percent college educated is used to predict the percentage of people
# below the poverty line.

ggplot(aes(x = , # add the independent variable
           y = ), # add the dependent variable
       data = ) + # add the data source
  geom_point()

# How would we add a title to this plot? Try looking in the help file and
# using google to find out.

## Correlations
# We have a lot of variables in our midwest dataset, so the pairs() 
# function may not be the best for simultaneously visualising them.
# We might use the `psych` package, and the cor.plot() function, to 
# visualise them instead.

install.packages("psych")
library(psych)

cor.plot(Filter(is.numeric, dat))

# Alternatively, we might take a subset of columns and use pairs() to
# visualise the correlations.

pairs(dat[c("poptotal", "percwhite", "percblack", "perchsd", 
            "percollege", "percprof", "percbelowpoverty")],
      upper.panel = NULL) # What does this argument change?

# Exercise: from your visual exploration of the scatter plots of 
# correlation, pick two variables to run the cor() function on. 
# Visualise these separately using ggplot, and save the output to file.

# Code to save your plot (saves last plot)
ggsave("corr_plot",
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
    #position = "jitter" # uncomment this line and run again
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

dev.off() # To prevent plotting errors below

## Regression
# Exercise: we were introduced to the lm() function in the tutorial guide.
# Use it below to run a model in which the percent college educated is 
# the independent variable, and percent below poverty is the outcome, or
# dependent variable.

coll_pov <- lm(, # add the correct code here 
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
ggplot(aes(x = , 
           y = ), 
       data = ) +
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
