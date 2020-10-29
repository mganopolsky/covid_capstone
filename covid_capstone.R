## Importing packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(archivist)) install.packages("archivist", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(splines)) install.packages("splines", repos = "http://cran.us.r-project.org")
if(!require(yaml)) install.packages("yaml", repos = "http://cran.us.r-project.org")
if(!require(argparser)) install.packages("argparser", repos = "http://cran.us.r-project.org")


library(splines)
library(yaml)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
library(formatR)
library(archivist)
library(devtools)
library(futile.logger)
library(sets)
library(argparser)



## Reading in file

#dir <- "../data"
#files <- list.files(dir)
#files
#file_name <- paste(dir, "covid_19_clean_complete.csv", sep = "/")
#all_data <- read_csv(file_name)

source("params.r")
source("capstone_utils.R")
source("covid_classes.R")

cfg <- read_yaml("config.yml")
CONFIG_FILEPATHS = unlist(cfg$filepaths)

time_beginning <- Sys.time()
yesterday <- format(Sys.Date() - 1, "%Y%m%d")

yesterday_logs_filename <- paste(
  yesterday , "_", format(Sys.time(), "%H%M")
)

parser <- arg_parser("Delphi")

parser <- add_argument(parser, "--optimizer", type="character", help= paste(
  "Which optimizer among 'tnc', 'trust-constr' or 'annealing' would you like to use ? " ,
    "Note that 'tnc' and 'trust-constr' lead to local optima, while 'annealing' is a " ,
    "method for global optimization: ")
                    , default="tnc", short='-o')

parser <- add_argument(parser, "--confidence_intervals", type="numeric", 
                       help= "Generate Confidence Intervals? Reply 0 or 1 for False or True.", 
                       default=1, short='-ci')

parser <- add_argument(parser, "--since100case", type="numeric", 
                       help= "Save all history (since 100 cases)? Reply 0 or 1 for False or True.", 
                       default=1, short='-s100')

arguments <- parse_args(parser )


OPTIMZER <- arguments$optimizer
GET_CONFIDENCE_INTERVALS <- as.logical (arguments$confidence_intervals)
SAVE_SINCE100_CASES <- as.logical(arguments$since100case)
PATH_TO_FOLDER_DANGER_MAP = CONFIG_FILEPATHS["danger_map"]

past_prediction_date <- format(Sys.Date() - 14, "%Y%m%d")




