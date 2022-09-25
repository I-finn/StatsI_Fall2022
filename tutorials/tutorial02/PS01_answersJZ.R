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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2", "stargazer"),  pkgTest)
library(tidyverse) # load our packages here

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data as vector
iqData <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# capture the number of observations
n <- length(iqData)

## Save our data to a .csv file in the data directory
write.csv(iqData, 
          file = "Data/iq.csv",
          row.names = FALSE)

# Explore data
summary(iqData)
str(iqData)
head(iqData)

# Visualise
hist(iqData,
     #breaks = 12,
     main = "Histogram of IQ",
     xlab = "IQ"
)

plot(density(iqData),
     main = "Pdf of IQ",
     xlab = "IQ"
)


# Use a QQ plot to determine if our height variable is
# normally distributed
qqnorm(iqData)
qqline(iqData,
       distribution = qnorm)

?qqnorm

## Confidence Intervals
# Calculate 90 percent confidence intervals using normal distribution
CI_lower <- qnorm(0.05, 
                  mean = mean(iqData), 
                  sd = (sd(iqData)/sqrt(length(iqData))) # the equation for the standard error of the mean
)

CI_upper <- qnorm(0.95,
                  mean = mean(iqData),
                  sd = (sd(iqData)/sqrt(length(iqData)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))


iqSum <- sum(iqData)


# Calculate 90 percent confidence intervals using a t distribution
se <- sd(iqData)/sqrt(length(iqData)) # Create an object with our standard error
t_score <- qt(.05, df = length(iqData)-1, lower.tail = FALSE)
CI_lower_t <- mean(iqData) - (se * t_score)
CI_upper_t <- mean(iqData) + (se * t_score)

# Check our working
t.test(iqData, conf.level = 0.9, alternative = "two.sided")


#####################
# Problem 2
#####################

# read in expenditure data
#expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
expenditure <- read.table("../../datasets/expenditure.txt", header=T)

# create scatterplot of Y and X1 
pdf("plot_example.pdf")
plot(expenditure$X1, expenditure$Y)
dev.off() # close plot...pdf, so can view it
head(expenditure)

# run an example regression, to show how to save table
regression1 <- lm(Y~X1, data=expenditure)
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("regression_output1.tex", regression1)   # file fragment 
ls()
