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
     breaks = 10,
     main = "Histogram of IQ",
     xlab = "IQ"
)

plot(density(iqData),
     main = "Pdf of IQ",
     xlab = "IQ"
)

# calculate mean
iqSum <- sum(iqData)           # sum of IQ scores
iqMean <-iqSum / n             # mean IQ score for sample
iqsd <- sd(iqData)             # standard deviation sample IQ scores
iqse <- iqsd / sqrt(n)         # standard error of sample

# Use a QQ plot to determine if our height variable is
# normally distributed
qqnorm(iqData)
qqline(iqData,
       distribution = qnorm)

#?qqnorm
# Sample values fall away from normal line at upper end

## Confidence Intervals
# Calculate 90 percent confidence intervals using normal distribution
alphaVal = 0.1 
CI_lower <- qnorm(alphaVal/2, 
                  mean = iqMean, 
                  sd =  iqse # the equation for the standard error of the mean
)

CI_upper <- qnorm(1-alphaVal/2,
                  mean = iqMean,
                  sd = iqse
)

cat(str_glue("{(1-alphaVal)*100}% Confidence Intervals, two-sided z-test"))
matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))


# Calculate 90 percent confidence intervals using a t distribution
#se <- sd(iqData)/sqrt(n) # Create an object with our standard error
t_score <- qt(alphaVal/2, df = n-1, lower.tail = FALSE)
CI_lower_t <- iqMean - (iqse * t_score)
CI_upper_t <- iqMean + (iqse * t_score)


cat(str_glue("{(1-alphaVal)*100}% Confidence Intervals - for two-sided t-test"))

matrix(c(CI_lower_t, CI_upper_t), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))
# t-test results in (slightly) wider confidence interval

# Check our working

t.test(iqData, conf.level = 1-alphaVal, alternative = "two.sided")

cat("Our Confidence interval for the IQ of the students in the sample is: ")
cat(str_glue("  {CI_lower_t} < mean IQ < {CI_upper_t} "))
cat(str_glue("with a confidence level of {(1-alphaVal)*100}%"))

    
## Hypothesis Testing
# Wrangling our data
class(iqData) # What class of vector is our IQ variable? - numeric

# compare plot of normal distribution, with population mean and sample sd
x.range <-seq(60, 140, by=1)
mainString <- str_glue("The Standard Normal Distribution (mean=100, sd= {iqsd})")
plot(x.range, dnorm(x=x.range, mean=100, sd=iqsd), 
     type="l",                                        
     main=mainString,
     ylab="density",
     xlab = "",
     lwd=2,
     xaxt="n")


# Hypothesis test:
# H0 : average IQ of students in school is less than national average 
# Ha : average IQ of students in school is greater than national average

# alpha = 0.05, 1-tail test, single population

# don't have variance of population, only have mean to compare against

# Test our hypothesis

#?t.test
# Now we run our t test
t.test( iqData  ,
       mu = 100, # population mean
       var.equal = TRUE, # The default is FALSE - don't have var for popn
       alternative = "greater", # H0: sample mean > population mean
       conf.level = .95) # 


# How do we interpret the output? 
# for confidence level of 95%, we cannot reject the hypothesis (p-value > alpha)

t.test( iqData  , # sample data
        mu = 100, # population mean IQ
        var.equal = TRUE, # The default is FALSE - don't have var for popn
        # irrelevant for single population test?
        alternative = "less", # Try changing this to "two.sided"
        conf.level = .95) # Try changing this critical value
# How do we interpret the output? 

t.test( iqData  , # sample data
        mu = 100, # population mean IQ
        var.equal = TRUE, # The default is FALSE - don't have var for popn
        alternative = "two.sided", # looking for likelihood of sample being representative
        conf.level = .90) # 

# cannot support null hyp that distributions are same
# mean and var (of samples) both differ; male mean > female (p-value 2.046e-06)

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
