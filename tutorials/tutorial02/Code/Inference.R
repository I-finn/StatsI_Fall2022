#######################
# Inference: A Reminder
#######################

## import our data
data <- read.csv("Data/height.csv")

## REMEMBER: our data are drawn from a sample!
(x_bar <- mean(data$height)) # not == mu

## REMEMBER: the standard deviation of a sample is different 
## to the standard deviation of a population: the denominator 
## is degrees of freedom (n -1), not n.
(sd_pop <- sqrt(
  sum(
    (data$height - mean(data$height))^2)
  /length(data$height))) # formula for the standard deviation

(sd_samp <- sd(data$height)) # the sd() function for a sample (n-1)

sd_pop == sd_samp # these two are not the same

## The distribution of our sample
hist(data$height)

## The *sampling distribution of the sample means* 
## (using bootstrapping)

# A bootstrap sample is made by randomly sampling from our
# existing sample WITH replacement.
(a_bootstrap_sample <- sample(data$height, 
                              size = 200, 
                              replace = TRUE))

mean(a_bootstrap_sample) == x_bar # note that the means are different!

# We can create a sampling distribution of the sampling means
# using repeated bootstrapping. In the first example, we use
# a parameter of n = 200
samp_dist_n200 <- vector(mode = "double", length = 1000)
for (i in 1:1000){
  samp_dist_n200[i] <- mean(sample(data$height, size = 200, replace = TRUE))
}
  
hist(samp_dist_n200)
mean(samp_dist_n200)

# Next we use a parameter of n = 5
samp_dist_n5 <- vector(mode = "double", length = 1000)
for (i in 1:1000){
  samp_dist_n5[i] <- mean(sample(data$height, size = 5, replace = TRUE))
}

hist(samp_dist_n5)
mean(samp_dist_n5)

# Notice that the mean for each is similar, but the distribution
# looks different.
plot(density(samp_dist_n5),
     xlim = c(min(samp_dist_n5), max(samp_dist_n5)),
     ylim = c(0, max(density(samp_dist_n200)$y)),
     col = "blue",
     main = "Pdf of two sampling distributions \n of the sample means for height",
     xlab = bquote(bar(x))
)
lines(density(samp_dist_n200), col = "red")
legend("topleft", 
       legend = c("n = 5", "n = 200"), 
       col = c("blue", "red"),
       lty = 1,
       cex = 0.8)

# Compare with the pdf of the raw sample values
plot(density(data$height),
     main = "Pdf of height",
     xlab = "")

## Inferring from our data
# When calculating *confidence intervals* from our sample, we're in effect
# taking the probability density function of the sampling distribution of 
# the sample means for our sample size, n, and working out between what 
# values of x bar 90 percent (or 95 percent, etc.) of our data lie. 

# In practice, we use the central limit theorem to help us calculate this. 
# This states that the mean of the sampling distribution of the mean is the
# same as the population mean, and that the standard error (the standard
# deviation of the sampling distribution of the mean) is equal to the 
# population standard deviation divided by the square root of the sample
# size.

# Because we can use statistics from our sample to *infer* parameters from
# our population, we don't need to bootstrap: we just need to use the 
# mathematical formula which underlies the distribution which we're modelling
# (in this case the normal distribution) to find the area under the curve.

# Fortunately, R has a set of functions which calculate for us the area 
# under the curve.

CI_lower <- qnorm(0.05, 
                  mean = mean(data$height), 
                  sd = (sd(data$height)/sqrt(length(data$height))) # the equation for the standard error of the mean
                  )

CI_upper <- qnorm(0.95,
                  mean = mean(data$height),
                  sd = (sd(data$height)/sqrt(length(data$height)))
                  )

# In the code above, we use the standard deviation of our sample as our 
# best estimate of the standard deviation of the population.
matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))
