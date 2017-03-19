# header
## title:

## clean workspace
cat("\014") # clear console; 
rm(list = ls()) # remove variables
gc() # collect garbage

## set global variables and load libraries
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
data_dir <- ""
github_url <- "https://raw.githubusercontent.com/jeffzs/common_scripts/master/"
source(paste0(github_url,"r_common.R"))

# data wrangling
## load data


## set inclusion criteria

## decode variables


# data analysis
## descriptive analysis
  
## adjusted analysis
