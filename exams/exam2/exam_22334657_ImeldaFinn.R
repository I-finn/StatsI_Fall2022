###############################################################
# Imelda Finn, 22334657
# POP77003 - Stats I
# Exam 2
# clear global .envir, load libraries, set wd
###############################################################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if(length(package.list)>0)  for(package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if(length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "car",
         "broom", "gridExtra"),  
       pkgTest)
#require(gridExtra)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
###############################################################

#Exam 2
#Applied Stats/Quant Methods 1
#Due: December 9, 2022
#Instructions
#Please read carefully: You have from 09:00 Wednesday December 7 until 08:59
#Friday December 9 to complete the exam. Please export your answers as a single
#PDF file and include all code you produce in a supporting R file, which you will
#upload to Blackboard. The exam is open book; you can consult any materials you
#like. You must not collborate with or seek help from other students. In case
#of questions or technical difficulties, you can contact Professor Ziegler via email. You
#should write-up your answers in R and LaTeX as you would for a problem set. Please
#make sure to concisely number your answers so that they can be matched with the
#corresponding questions.
#-----------------------------------------------------------------

#q1
#q1b
-18.1368 + 10* 2.2980 + (-4.0716)
[1] 0.7716

#q2

#q3

#q4

#crossover point for well.depth:dist100 for well.depth = 1 -> 0
8.95/4.50   #[1] 1.988889

beta0 <- -5.73
beta_well <- 8.95
beta_dist <- -0.06
beta_mult <- -4.5
se_mult <- 2.66

tstat_arsenic <- beta_mult /se_mult
pval_arsenic <- 2*pt(abs(tstat_arsenic), df=1000-4 , lower.tail = FALSE)
tstat_arsenic #[1] -1.691729
pval_arsenic  #[1] 0.0910104

#prefer model 1
arsenic_a <- -0.03 + 1 * 3.33 + 0.4 * (-4.62)
arsenic_b <- -0.03 + 1 * 3.33 + 2.08 * (-4.62)
arsenic_a - arsenic_b  # 7.7616

#household a: well_depth = 1, dist100 = 0.4

arsenic_a <- -0.03 + 1 * 3.33 + 0.4 * (-4.62)
#1.452
#arsenic_a <- -5.73 + 1 * 8.95 + 0.4 * (-0.06) + 1 * 0.04 * (-4.50) 
#3.016

#household b: well_depth = 1, dist100 = 2.08

arsenic_b <- -0.03 + 1 * 3.33 + 2.08 * (-4.62)
#-6.3096
arsenic_b <- -5.73 + 1 * 8.95 + 2.08 * (-0.06) + 1 * 2.08 * (-4.50) 
#-6.2648

diff_ab <- arsenic_a - arsenic_b

diff_ab
#[1] 7.7616
#[1] 9.2808


Wells <- Wells[1:1000,]

plot_coefs1 <- c(-0.03, 3.33, -4.62)
plot_coefs2 <- c(-5.73, 8.95, -0.06, -4.50)

fivenum(Wells$arsenic)
fivenum(Wells$distance)
png("Graphics/arsenic.png")
plot(Wells$distance , Wells$arsenic, type= "n" ,
     xlim=c(-10 , 10) , ylim=c(-10 , 10) , ylab = "arsenic" , xlab = "distance" )
#points(Prestige[which(Prestige$professional ==1) , "income"], 
#       Prestige[which(Prestige$professional ==1) , "prestige" ],
#       col =2 , pch =18)
#points(Prestige[which(Prestige$professional ==0) , "income"],
#       Prestige[which(Prestige$professional ==0) , "prestige"],
#       col =3 , pch =18)

# no interaction
abline( plot_coefs1[1] , plot_coefs1[2] , col =2 , lwd =2)

# no interaction
abline( plot_coefs1[1] + plot_coefs1[3], plot_coefs1[2], col =2 , lwd =1)

#when prof = 1
abline( plot_coefs2[1] + plot_coefs2[2], plot_coefs2[3] + 
          plot_coefs2[4], col =3 , lwd =2)
#when prof = 0
abline( plot_coefs2[1] , plot_coefs2[3] , col =4 , lwd =2)

legend("bottomright", legend=c("no interaction" , "deep well", "shallow well"),
       col=c(2, 3, 4), lty=1, cex=0.8)

dev.off()

#q5
#q5b
# into LaTeX
beta_int<- -9.747 
se_int <-  28.86
beta_ideology <-  -3.614
se_ideology <- 1.381 
beta_age <- -10.75 
se_age <- 4.874
beta_education <- 4.419
se_education <- 2.373

N <- 1166
est_coeffs <- 4
#degrees of freedom = 1166-4 = 1162
tscore <- qt(0.975, 1166-4)
#1.962008

CI_L <- beta_ideology - tscore*se_ideology
CI_U <- beta_ideology + tscore*se_ideology

#At the 95% confidence level, the increase in support for climate action 
# for each unit increase in ideology (with constant age and education) is:
paste0("CI: ",round(CI_L,3), " to ",round(CI_U,3),"." )
#[1] "CI: -6.324 to -0.904."

#q5c
age <- 0
education <- 11.99
low_ideology <- 2.29
high_ideology <- 5.71

low_climate_support <- beta_int + low_ideology * beta_ideology + 
  beta_age * age + beta_education * education
high_climate_support <- beta_int + high_ideology * beta_ideology + 
  beta_age * age + beta_education * education

diff_climate_support <- low_climate_support - high_climate_support
#12.35988
paste0("Climate Change support (low, high ideology): ",round(low_climate_support,3),
       ", ",round(high_climate_support,3),"." )
#[1] "Climate Change support (low, high ideology): 34.961, 22.601."

#-


sd_ideology <-(high_ideology - low_ideology)/2
mean_ideology <- low_ideology + sd_ideology

#


#q6
agreement <- 1
agreement2 <- agreement^2
relief_model <- lm(relief~ agreement + agreement2, data = relief_data)

