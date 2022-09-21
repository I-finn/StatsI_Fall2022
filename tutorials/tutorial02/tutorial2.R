#####################
# Imelda 
# Tutorial 2 - statsI

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

# read in relevant packages
# Packages
library(tidyverse) # load our packages here

# read in height data
hData<-read.csv("Data/height.csv")

# inspect data
summary(hData)
str(hData)
head(hData)

# Visualise
# plot histogram
hist(hData$height,
     #breaks = 12,
     main = "Histogram of Height",
     xlab = "Height (cm)"
)

# plot density function
plot(density(hData$height),
     main = "Pdf of Height",
     xlab = "Height (cm)"
)

?hist
?density


# Use a QQ plot to determine if our height variable is
# normally distributed
# check if actual distribution data points is consistent with fitted line
qqnorm(hData$height)
qqline(hData$height,
       distribution = qnorm)


# simplify data references
heightData <- hData$height
meanHeight <- mean(heightData)
n <- length(heightData)
sexData <- hData$sex

# calc conf intervals
## Confidence Intervals
# Calculate 90 percent confidence intervals using normal distribution
CI_lower <- qnorm(0.05, 
                  mean = meanHeight, 
                  sd = (sd(heightData)/sqrt(n)) # the equation for the standard error of the mean
)

CI_upper <- qnorm(0.95,
                  mean = meanHeight,
                  sd = (sd(heightData)/sqrt(n))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))


# Calculate 90 percent confidence intervals using a t distribution
se <- sd(heightData)/sqrt(n) # Create an object with our standard error
t_score <- qt(.05, df = n-1, lower.tail = FALSE)
CI_lower_t <- mean(heightData) - (se * t_score)
CI_upper_t <- mean(heightData) + (se * t_score)

# Check our working
t.test(heightData, conf.level = 0.9, alternative = "two.sided")

## Hypothesis Testing
# Wrangling our data
class(sexData) # What class of vector is our sex variable?

as.logical(sexData) # We can use functions to change the class (here w/o assigning)
as.factor(sexData)

?factor # Factors have *levels*: if we want to change sex to a factor, we need to also change the levels
hData$sex_f <- as.factor(hData$sex) # Convert to factor
levels(hData$sex_f) <- c("M", "F") # Change the levels
head(hData) # Check our result

aggregate(data$height, by = list(data$sex_f), FUN = mean) # Base R grouping
aggregate(height ~ sex_f, data = data, FUN = mean) # Does the same using a formula

data %>% # Tidyverse method for grouping
  group_by(sex_f) %>%
  summarise(mean = mean(height))

# Visualise
boxplot(data$height ~ data$sex_f, # here we use formula notation to group
        main = "Boxplot of Height by Sex",
        ylab = "cm",
        xlab = "")
# By looking at the boxplot, what might we conclude?

# Formulate our null hypothesis:
# What is our null hypothesis? 
# Where do we set alpha? (i.e. rejecting null hypothesis when we shouldn't; 
# note that we don't get to decide beta - i.e. the probability of incorrectly 
# accepting the null hypothesis)
# Is a one-tail or two-tail test more appropriate?

# Test our hypothesis
# First - are our variances equal? (Don't worry too much about this now...)
var.test(height ~ sex_f,
         data = data,
         ratio = 1,
         alternative = "two.sided",
         conf.level = 0.95)

# Now we run our t test
t.test(height ~ sex_f, # Use formula to group
       data = data,
       mu = 0, # The default (null) is zero
       var.equal = FALSE, # The default is FALSE
       alternative = "greater", # Try changing this to "two.sided"
       conf.level = .95) # Try changing this critical value
# How do we interpret the output? (Hint: looking back at the boxplot can help)


mean(hData$height)
