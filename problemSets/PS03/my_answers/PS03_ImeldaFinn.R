###############################################################
# Imelda Finn, 22334657
# POP77003 - Stats I
# clear global .envir, load libraries, set wd
###############################################################

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
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "car", "broom"),  pkgTest)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###############################################################

#Problem Set 3
#Applied Stats/Quant Methods 1
#Due: November 20, 2021
#Instructions
# Please show your work! You may lose points by simply writing in the answer. If the
#problem requires you to execute commands in R, please include the code you used to
#get your answers. Please also include the .R file that contains your code. If you are
#not sure if work needs to be shown for a particular problem, please ask.
# Your homework should be submitted electronically on GitHub.
# This problem set is due before 23:59 on Sunday November 20, 2022. No late assign-
#  ments will be accepted.
# Total available points for this homework is 80.
#In this problem set, you will run several regressions and create an add variable plot (see the
#lecture slides) in R using the incumbents subset.csv dataset. Include all of your code.

#####################
# get data
#####################
# line in tex = 61
dat = read_csv("../../../datasets/incumbents_subset.csv")
saveRDS(dat, "Data/incumbents_subset.csv")
dat <- readRDS("Data/incumbents_subset.csv")

# - explore ------------------------------------------
# difflog is the difference between the incumbents' spending
# and the challengers' spending
dl = sum(dat$difflog - (dat$incspend - dat$chalspend))

png("Graphics/incumbent_subset.png" )
pairs(~voteshare + presvote + difflog, dat)
dev.off() # close output

mean(dat$difflog)
fivenum(dat$difflog)
#-3.0600882  0.6733625  1.6105532  2.9856564  5.8558103'
fivenum(dat$presvote)
#0.1931263 0.4695266 0.5478388 0.6239612 0.9606069
fivenum(dat$voteshare)
#0.3475569 0.5845994 0.6568562 0.7232970 0.9930361

#####################
#Question 1
#####################
#We are interested in knowing how the difference in campaign spending between incumbent
#and challenger affects the incumbent's vote share.
#1. Run a regression where the outcome variable is voteshare and the explanatory variable
#is difflog.
#2. Make a scatterplot of the two variables and add the regression line.
#3. Save the residuals of the model in a separate object.
#4. Write the prediction equation.

#1.  # line in tex 93
mod_vote_spend <- lm(voteshare ~ difflog, data = dat)

#2.
dat %>% ggplot(aes( x= difflog, y = voteshare)) +
  geom_point(size=0.5) + 
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')

ggsave("Graphics/vote_spend.png")

#3.  line in tex 104
resid_vote_spend <- mod_vote_spend$residuals

#4.
summary(mod_vote_spend)

output_stargazer("Tables/mod_vote_spend.tex", appendVal = FALSE, mod_vote_spend,
                 title="Vote share as a function of Differental Spending",
                 label="tab:vote_spend",  digits = 6 #, summary=FALSE
)


#Prediction Equation 
# voteshare = 0.579031 + (0.041666) * difflog
# ie the voteshare is 0.579031 when difflog is 0
#    it increases by 0.041666 for each unit increase in difflog

#Call:
#  lm(formula = voteshare ~ difflog, data = dat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.26832 -0.05345 -0.00377  0.04780  0.32749 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.579031   0.002251  257.19   <2e-16 ***
#  difflog     0.041666   0.000968   43.04   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.07867 on 3191 degrees of freedom
#Multiple R-squared:  0.3673,	Adjusted R-squared:  0.3671 
#F-statistic:  1853 on 1 and 3191 DF,  p-value: < 2.2e-16


#Question 2
#We are interested in knowing how the difference between incumbent and challenger's 
# spending and the vote share of the presidential candidate of 
# the incumbent's party are related.
#1. Run a regression where the outcome variable is presvote and the explanatory variable
#is difflog.
#2. Make a scatterplot of the two variables and add the regression line.
#3. Save the residuals of the model in a separate object.
#4. Write the prediction equation.

#1.   # line in tex 149
mod_pres_spend <- lm(presvote ~ difflog, data = dat)

#2.
dat %>% ggplot(aes( x= difflog, y = presvote)) +
  geom_point(size=0.5) + 
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')
ggsave("Graphics/pres_spend.png")

#3.  line in tex 159
resid_pres_spend <- mod_pres_spend$residuals

#4.
summary(mod_pres_spend)

output_stargazer("Tables/mod_pres_spend.tex", appendVal = FALSE, 
                 mod_pres_spend,
                 title="Presidential vote share as a function of Differental Spending",
                 label="tab:pres_spend",  digits = 6 
)

#Prediction Equation 
# presvote = 0.507583 + (0.023837) * difflog
# ie the presvote is 0.507583 when difflog is 0
#    it increases by 0.023837 for each unit increase in difflog

#Call:
#lm(formula = presvote ~ difflog, data = dat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.32196 -0.07407 -0.00102  0.07151  0.42743 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.507583   0.003161  160.60   <2e-16 ***
#  difflog     0.023837   0.001359   17.54   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.1104 on 3191 degrees of freedom
#Multiple R-squared:  0.08795,	Adjusted R-squared:  0.08767 
#F-statistic: 307.7 on 1 and 3191 DF,  p-value: < 2.2e-16


#Question 3
#We are interested in knowing how the vote share of the presidential
# candidate of the incumbent's party is associated with the 
#incumbent's electoral success.
#1. Run a regression where the outcome variable is voteshare and the explanatory variable
#is presvote.
#2. Make a scatterplot of the two variables and add the regression line.
#3. Write the prediction equation.

#1.    # line in tex 204
mod_vote_pres <- lm(voteshare ~ presvote, data = dat)

#2)
dat %>% ggplot(aes( x= presvote, y = voteshare)) +
  geom_point(size=0.5) + 
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')
ggsave("Graphics/vote_pres.png")

#3)
summary(mod_vote_pres)

output_stargazer("Tables/mod_vote_pres.tex", appendVal = FALSE, 
                 mod_vote_pres,
                 title="Vote share as a function of Presidential vote share",
                 label="tab:vote_pres",  digits = 6 
)

#Prediction Equation 
# voteshare = 0.441330 + (0.388018) * presvote
# ie the voteshare is 0.441330 when presvote is 0
#    it increases by 0.388018 for each unit increase in presvote


#Call:
#  lm(formula = voteshare ~ presvote, data = dat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.27330 -0.05888  0.00394  0.06148  0.41365 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.441330   0.007599   58.08   <2e-16 ***
#  presvote    0.388018   0.013493   28.76   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.08815 on 3191 degrees of freedom
#Multiple R-squared:  0.2058,	Adjusted R-squared:  0.2056 
#F-statistic:   827 on 1 and 3191 DF,  p-value: < 2.2e-16


#Question 4
#The residuals from part (a) tell us how much of the variation in voteshare is not explained
#by the difference in spending between incumbent and challenger. The residuals in part (b)
#tell us how much of the variation in presvote is not explained by the difference in spending
#between incumbent and challenger in the district.
#1. Run a regression where the outcome variable is the residuals from Question 1 and the
#explanatory variable is the residuals from Question 2.
#2. Make a scatterplot of the two residuals and add the regression line.
#3. Write the prediction equation.

#1.    # line in tex 258-259
resid_dat = tibble(resid_pres_spend, resid_vote_spend)
mod_resid_vote_pres <- lm(resid_vote_spend ~ resid_pres_spend, data = resid_dat)

#2.

dat_add <- augment(mod_resid_vote_pres)
head(resid_dat)

resid_dat %>% ggplot(aes(x = resid_pres_spend, y = resid_vote_spend)) +
  geom_point(size=0.5) + 
#  geom_line(data = dat_add, aes(y = .fitted), colour = "blue") + # we change our data to the fitted values of the additive model
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, 
              se = FALSE, linetype = 'dashed')

ggsave("Graphics/residuals.png")



#3.
summary(mod_resid_vote_pres)
output_stargazer("Tables/mod_resid_vote_pres.tex", appendVal = FALSE, 
                 mod_resid_vote_pres,
                 title="Incumbent\'s vote share residuals as a function of Presidential vote share residuals",
                 label="tab:residuals",  digits = 6 #, summary=FALSE
)

# residuals for both are clustered around 0 
#Prediction Equation 
# voteshare residuals  = -5.207e-18 + (0.2569) * presvote residuals
# ie the voteshare residual value is -5.207e-18 when presvote residual value is 0
#    it increases by 0.2569 for each unit increase in presvote residuals

fivenum(resid_pres_spend)
fivenum(resid_vote_spend)
mean(resid_pres_spend)     # ie 0
mean(resid_vote_spend)     # ie 0, as expected


#Call:
#  lm(formula = resid_vote_spend ~ resid_pres_spend)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -5.207e-18  1.299e-03    0.00        1    
#resid_pres_spend  2.569e-01  1.176e-02   21.84   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.07338 on 3191 degrees of freedom
#Multiple R-squared:   0.13,	Adjusted R-squared:  0.1298 
#F-statistic:   477 on 1 and 3191 DF,  p-value: < 2.2e-16

#Question 5
#What if the incumbent's vote share is affected by both the president's popularity and the
#difference in spending between incumbent and challenger?
#1. Run a regression where the outcome variable is the incumbent's voteshare and the
#explanatory variables are difflog and presvote.
#2. Write the prediction equation.
#3. What is it in this output that is identical to the output in Question 4? Why do you
#think this is the case?

#1.    # line in tex 326
mod_vote_spend_pres <- lm(voteshare ~ presvote + difflog, data = dat)

#2)
#install.packages("car")
#library(car)
png("Graphics/add_variable.png" )
avPlots(mod_vote_spend_pres, col = carPalette()[1], pch = 1, cex=0.5, id=FALSE)
dev.off() # close output

#3)
summary(mod_vote_spend_pres)

#output_stargazer("Tables/mod_vote_spend_pres.tex", appendVal = FALSE, 
#                 mod_vote_spend_pres,
#                 title="Vote share as a function of Presidential vote share and differential spending",
#                 label="tab:vote_spend_pres",  digits = 6 #, summary=FALSE
#)

# output for q5 and q4 combined
output_stargazer("Tables/mod_vote_spend_pres.tex", appendVal = FALSE, 
                 mod_vote_spend_pres, mod_resid_vote_pres, 
                 title="Vote share as a function of Presidential vote share and differential spending",
                 label="tab:vote_spend_pres",  digits = 6 
)


#stargazer::stargazer(mod_vote_spend_pres, mod_resid_vote_pres, type = "latex",  )
#Prediction Equation 
# voteshare = 0.4486442 + (0.0355431) * difflog + (0.2568770) * presvote
# ie the voteshare is 0.4486442 when difflog and presvote are 0
#    it increases by 0.0355431 for each unit increase in difflog
#           (holding presvote constant)
#    it increases by 0.2568770 for each unit increase in presvote
#           (holding difflog constant)

#Call:
#  lm(formula = voteshare ~ difflog + presvote, data = dat)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.25928 -0.04737 -0.00121  0.04618  0.33126 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.4486442  0.0063297   70.88   <2e-16 ***
#  difflog     0.0355431  0.0009455   37.59   <2e-16 ***
#  presvote    0.2568770  0.0117637   21.84   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.07339 on 3190 degrees of freedom
#Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4493 
#F-statistic:  1303 on 2 and 3190 DF,  p-value: < 2.2e-16


#-----------------------------------------------------------------------
#ref:https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r
#https://rdrr.io/github/smbjohnson/UtilityFunctions/src/R/ggAVplots.R

avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile()
  png(filename = ff)
  OUT <- car::avPlots(MODEL, ...)
  dev.off()
  unlink(ff)
  OUT }
#Produce matrix of added variable plots
library(gridExtra)

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
#  AVPLOTS <- avPlots.invis(MODEL)
  AVPLOTS <- avPlots(MODEL)
  K       <- length(AVPLOTS)
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K)
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]])
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black', size = 0.5) + 
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')) }
  
  #Return output object
  GGPLOTS }

require(gridExtra)

gg_avs <- ggAVPLOTS(mod_vote_spend_pres)

grid.arrange(gg_avs[[1]], gg_avs[[2]], nrow = 2, ncol=1)
ggsave("./Graphics/av_ggplot.png")# only saves 2nd plot
# export manually to ./Graphics/av_gg_manual.png

#--------------------------------------------
dat_add <- augment(mod_vote_spend_pres)
summary(dat_add)

dat %>% ggplot( aes(x = difflog + presvote , y = voteshare)) +
  geom_point(alpha = 0.5)+
  geom_line(data = dat_add, aes(y = .fitted)) + # we change our data to the fitted values of the additive model
  geom_smooth(method = "lm", colour = "red", formula = y ~ x, linetype = 'dashed') 

