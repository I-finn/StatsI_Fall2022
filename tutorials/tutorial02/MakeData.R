####################
# Data import script
####################

# Packages
#library() # any packages we need will go here

# We should add a short note here to explain what we're doing in 
# this script

## Create a vector for height
height <- c(182, 171, 183, 193, 175, 173, 173, 169, 184, 179, 160,
            168, 182, 182, 188, 172, 165, 165, 171, 173, 180, 174,
            154, 167, 180, 173, 182, 161, 165, 167, 157, 157, 185)#rnorm(27, mean = 160, sd = 15)

## Create a vector for sex
sex <- c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 
         0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0)#sample(c(0,1), replace=TRUE, size=27)

## Create a data.frame object from these vectors using cbind()
data <- data.frame(cbind(height, sex))

## Save our data to a .csv file in the data directory
write.csv(data, 
          file = "Data/height.csv",
          row.names = FALSE)
