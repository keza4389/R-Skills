# Clear environment from the workspace
rm(list = ls())  

# Load data
source("00_load_data.R")
# Load functions
source("01_functions.R")

# Generate report
rmarkdown::render("02_report.Rmd",
                  output_file = "report.html")
