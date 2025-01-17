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
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr"),  pkgTest)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}


# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

#Question 1 (40 points): Political Science

#The following table was created using the data from a study run in a major
# Latin American city.
# As part of the experimental treatment in the study, one employee of the research
# team was chosen to make illegal left turns across traffic to draw the attention 
# of the police officers on shift. Two employee drivers were upper class, two were 
# lower class drivers, and the identity of the driver was randomly assigned per 
# encounter. The researchers were interested in whether officers were more or less 
# likely to solicit a bribe from drivers depending on their class (officers use 
# phrases like, ``We can solve this the easy way'' to draw a bribe). 
# The table below shows the resulting data.

	
#& Not Stopped & Bribe requested & Stopped/given warning \\
#Upper class & 14 & 6 & 7 \\
#Lower class & 7 & 7 & 1 \\
results <- matrix( c (14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE) 
observed <- results            
# create data structure with named dimensions
cols <- c("NotStopped", "BribeRequested",  "StoppedGivenWarning")
rows <- c("UpperClass", "LowerClass")
#results <- matrix( c (14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE, 
#                   dimnames = list(rows, cols))
                   
ncols <- length(observed[1,])
nrows <-length(observed[,1])


#rm(results_tab)

print(observed)

#\item [(a)]
#Calculate the $\chi^2$ test statistic by hand/manually (even better if you can 
# do "by hand" in \texttt{R}).\\

#results_df <- data.frame(results)
#results_df


# get totals
row_tots <- vector("double", nrows)
col_tots <- vector("double", ncols)

totals <- sum(observed)

for (i in 1:nrows) {
  row_tots[i] <- sum(observed[i,])
}
for (i in 1:ncols) {
  col_tots[i] <- sum(observed[, i])
}

#get expected
expected <- observed
for (i in 1:nrows) {
  for (j in 1:ncols) {
    expected[i,j] <- row_tots[i] * col_tots[j] / totals
  }
}

expected

o_e <- observed 
for (i in 1:nrows) {
  for (j in 1:ncols) {
    o_e[i,j] <- (observed[i,j] - expected[i,j])^2  /expected[i,j]
  }
}

chi_sq_val <- sum(o_e)
cat(str_glue("The chi_squared statistic is {round(chi_sq_val,3)}"))

df = (nrows-1) * (ncols-1)

cat(str_glue("The chi_squared degrees of freedom is {df}"))


#\item [(b)]
#Now calculate the p-value from the test statistic you just created R
# .\footnote{Remember frequency should be $>$ 5 for all cells, but let's calculate 
# the p-value here anyway.}  What do you conclude if $\alpha = 0.1$?\\

pchisq(chi_sq_val, df=df, lower.tail=FALSE)

# p > alpha, can't reject null

cat(str_glue("The chi_squared degrees of freedom is {df}"))

cells_ok <- length(observed[observed<5])


#	\item [(c)] Calculate the standardized residuals for each cell and put them in the table below.

z <- observed 
for (i in 1:nrows) {
  row_prop<- (1-row_tots [i] / totals)
  
  for (j in 1:ncols) {
    col_prop<- (1-col_tots [j] / totals)
    z[i,j] <- (observed[i,j] - expected[i,j])  /sqrt (expected[i,j]* row_prop * col_prop)
  }
}

out_z<- z

dimnames(out_z) <- list(rows, cols)


#\begin{table}[h]
#		\centering
#		\begin{tabular}{l | c c c }
#			& Not Stopped & Bribe requested & Stopped/given warning \\
#			\\[-1.8ex] 
#			\hline \\[-1.8ex]
#			Upper class  &  &  &  \\
#			\\
#			Lower class &  &   &   \\
			
#		\end{tabular}
#	\end{table}
	

#	\item [(d)] How might the standardized residuals help you interpret the results?  




######################################################################################
# Problem 2
#####################

#Question 2 (40 points): Economics
#Chattopadhyay and Duflo were interested in whether women promote different policies 
# than men. 
# Answering this question with observational data is pretty difficult due to potential 
# confounding problems (e.g. the districts that choose female politicians are 
# likely to systematically differ in other aspects too). Hence, they exploit a 
# randomized policy experiment in India, where since the mid-1990s, 1/3 of 
# village council heads have been randomly reserved for women. A subset of the data 
# from West Bengal can be found at the following link: 
#    \url{https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv}

# Each observation in the data set represents a village and there are two villages 
# associated with one GP (i.e. a level of government is called "GP"). 
# Figure~\ref{fig:women_desc} below shows the names and descriptions of the variables in the dataset. The authors hypothesize that female politicians are more likely to support policies female voters want. Researchers found that more women complain about the quality of drinking water than men. You need to estimate the effect of the reservation policy on the number of new or repaired drinking water facilities in the villages.
# Names and description of variables from Chattopadhyay and Duflo (2004)



#\item [(a)] State a null and alternative (two-tailed) hypothesis. 


#\item [(b)] Run a bivariate regression to test this hypothesis in \texttt{R} (include your code!).


#\item [(c)] Interpret the coefficient estimate for reservation policy. 



#policy <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
write.csv(policy,"Data/policy.csv")
policy<-read.csv("Data/policy.csv")

pairs(policy)
