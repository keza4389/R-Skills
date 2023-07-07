#############################################################
# This is where we will load and clean up data. #############
#############################################################
#--------------------------------------------
library(tidyverse)
library(data.table)
#--------------------------------------------


# Create a path for our data set
path <- "data/bank.csv"
# Read csv file and assign a variable
dt <- read.csv(path,sep = ";")
# Clean each variable
dt <- drop_na(dt)
# Make sure it is a data table type
dt <- as.data.table(dt)

# Change all of the character class to factor
dt[] <- lapply(dt, function(x) {
  if (is.character(x)) {
    factor(x)
  } else {
    x
  }
})

