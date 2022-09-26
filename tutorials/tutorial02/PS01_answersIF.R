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
meanIQ <- c()
for (i in 1:1000) {
  meanIQ[i] <- mean(sample(iqData, 20, replace=TRUE))
}
mean(meanIQ)
summary(meanIQ)

plot(meanIQ) +
  plot(iqData, col = "red")

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
# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# read in expenditure data
#expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
expenditure <- read.table("../../datasets/expenditure.txt", header=T)

# State 50 states in US
#Y per capita expenditure on shelters/housing assistance in state
#X1 per capita personal income in state
#X2 Number of residents per 100,000 that are "financially insecure" in state
#X3 Number of people per thousand residing in urban areas in state
#Region 1=Northeast, 2= North Central, 3= South, 4=West
data_headers <- c("State", "$ExpenditurePC", "$IncomePC", "FInsecureResidents", 
             "UrbanResidents", "Region")
regions <- c("Northeast","North Central", "South", "West")
names(expenditure)

# Inspect the data
head(expenditure)
str(expenditure)
summary(expenditure)

#investigate spending on Housing assistance

# Visualise
hist(expenditure$Y,
     #breaks = 12,
     main = "Histogram of spending on HA ",
     xlab = "$, per capita"
)

plot(density(expenditure$Y),
     main = "PDF of spending on HA ",
     xlab = "$, per capita"
)

qqnorm(expenditure$Y)
qqline(expenditure$Y,
       distribution = qnorm)


# create plots of Y and Xn 
onefile <- TRUE
#pdf( file = if(onefile) "expenditure_plots.pdf" else "expenditure_plots%03d.pdf")
#pdf("plot_example.pdf" )

ggplot(expenditure) +
  geom_point(aes( Y, X1), colour = "blue") +
  geom_smooth(aes( Y, X1))

ggplot(expenditure) +
  geom_point(aes( Y, X2), colour = "blue") +
  geom_smooth(aes( Y, X2))

ggplot(expenditure) +
  geom_point(aes( Y, X3), colour = "blue", ) +
  geom_smooth(aes( Y, X3), colour = "red")


ggplot(expenditure) +
  geom_point(aes( STATE, Y), colour = "green") +
  geom_point(aes( STATE, X1), colour = "blue") 

ggplot(expenditure) +
  geom_point(aes( STATE, X1/Y), colour = "green")


ggplot(expenditure) +
  geom_point(aes( Y, X1), colour = "blue") 

ggplot(expenditure) +
  geom_point(aes( Y, X2), colour = "green") 

ggplot(expenditure) +
  geom_point(aes( Y, X3)) +
  geom_smooth(aes( Y, X3))

#main = "Income per capita vs spending on HA "
ggplot(expenditure) +
  geom_point(aes( Y, X1), colour = "blue") +
  geom_smooth(aes( Y, X1))

#dev.off() # close pdf file


# regional expenditure on housing assistance

ggplot(expenditure) +
  geom_point(aes( Y, X2, colour = factor(Region))) +
  geom_smooth(aes( Y, X2))
#logarithmic scale

#factor(expenditure$Region) <- regions
ggplot(expenditure) +
  geom_point(aes(  Region, Y,colours = factor(Region))) 
# more spread in r4, least in r2

# can see eg that no crossover in interquartile ranges
boxplot(expenditure$Y ~ expenditure$Region, # here we use formula notation to group
        main = "Boxplot of per capita spending on HA by Region",
        names=regions,
        ylab = "$",
        xlab = "")


regional_mean_table <-expenditure %>% # Tidyverse method for grouping
  group_by(Region) %>%
  summarise(mean = mean(Y))

regional_mean_table <- cbind(regional_mean_table, regions)

output_stargazer("regional_means.tex", regional_mean_table[, -1], appendVal = FALSE)   # file fragment 

ggplot(re_means) + 
  geom_point(aes(regions, mean, colour = regions), size=3)

# West region has highest per capita mean expenditure on housing assistance

#---------------------------------------------------------------------------
# look at income vs expenditure on HA, by region
#factor(expenditure$Region) <- regions
ggplot(expenditure) +
  geom_point(aes( Y, X1)) +
  geom_smooth(aes(Y, X1))

ggplot(expenditure) +
  geom_point(aes( Y, X1, colour= regions[Region], shape= regions[Region])) 

ggplot(expenditure) +
  geom_point(aes( Y, X1, colour= regions[Region], shape= regions[Region])) +
  geom_smooth(aes(Y, X1))

ggplot(expenditure) +
  geom_point(aes( Y, X1, colour= factor(Region), shape= factor(Region))) +
  geom_smooth(aes( Y, X1, colour = factor(Region)))

# run an example regression, to show how to save table


lm(Y~X1, data=expenditure)
lm(Y~X2, data=expenditure)
lm(Y~X3, data=expenditure)

# execute function and check ls() to make sure it worked
ls()
