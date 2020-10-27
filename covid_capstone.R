## Importing packages

library(tidyverse) # metapackage with lots of helpful functions
library(lubridate)
library(splines)

## Reading in file

dir <- "../data"
#files <- list.files(dir)
#files
#file_name <- paste(dir, "covid_19_clean_complete.csv", sep = "/")
#all_data <- read_csv(file_name)

source("params.r")
source("capstone_utils.R")