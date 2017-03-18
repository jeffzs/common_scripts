# COPYRIGHT: 2014-present
# AUTHOR: Zhifei Sun, zhifei.sun@duke.edu
# PURPOSE: Repository for custom functions used in every project

# FUNCTIONS ----------------------------------------------------------
select_hcup_sid_core_cases <- function(core.file, core.variables=c(),
                           aha.file, revisit.file) {
  # returns a data.table class object
  

  library(data.table)
  library(bit64)
  # initialize
  df <- data.table()
  df.core <-data.table()
  df.aha <- data.table()
  df.revisit <- data.table()
  
  # print status message
  print(paste("Reading from", core.file))
  
  # checks to prevent errors
  core.variables <= toupper(core.variables)
  if (! ("KEY" %in% core.variables)) core.variables <- c("KEY", core.variables)  
  if (! ("DSHOSPID" %in% core.variables)) core.variables <- c("DSHOSPID", core.variables)  
  if (! ("YEAR" %in% core.variables)) core.variables <- c("YEAR", core.variables)  
  if (grepl("2009", core.file)) core.variables <- c("VISITLINK","DAYSTOEVENT", core.variables)
  if (grepl("2010", core.file)) core.variables <- c("VISITLINK","DAYSTOEVENT", core.variables)
  if (grepl("2011", core.file)) break("check customscript function here to add in revisit variables")
  
  # read Core file
  if (is.null(core.variables))  {
    df.core  <- data.table(fread(core.file, na.strings = c("","NA","."), verbose = F, showProgress = FALSE))
  } else {
    df.core  <- data.table(fread(core.file, na.strings = c("","NA","."), verbose = F, showProgress = FALSE,  select = core.variables))
  }
  setnames(df.core,toupper(names(df.core))) # standardize column names
  
  # read and link AHA linkage file
  df.aha <- fread(aha.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
  setnames(df.aha,toupper(names(df.aha)))
  
  setkey(df.core,DSHOSPID)
  setkey(df.aha,DSHOSPID)
  df <- df.aha[df.core]  # merge

  # read and link Revisit file
  if(df.core$YEAR[1] < 2009) {           # after 2009, the revisit variable is embeded
    df.revisit <- fread(revisit.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
    setnames(df.revisit,toupper(names(df.revisit)))
    setkey(df,KEY)
    setkey(df.revisit,KEY)
    df <- df.revisit[df]
  }
  
  setDT(df)
  return(df)
}


link_hcup_sid_core_with_hospital <- function(core.data, aha.file){
  if (!c("data.table","bit64") %in% installed.packages()) {
    library(data.table)
    library(bit64)
  }
  
  df.aha <- fread(aha.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
  setnames(df.aha,toupper(names(df.aha)))
  
  setkey(core.data,DSHOSPID)
  setkey(df.aha,DSHOSPID)
  df <- df.aha[core.data]  # merge
  
  setDT(df)
  return(df)
}

link_hcup_sid_core_with_cost <- function(core.data, cost.file) {
  
  if (!c("data.table","bit64") %in% installed.packages()) {
    library(data.table)
    library(bit64)
  }
  
  df.cost <- fread(cost.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
  setnames(df.cost,toupper(names(df.cost)))
  
  df.cost <- as.data.frame(sapply(df.cost, function(x) gsub("\'", "", x)))  # remove all the god damn single quotes
  names(df.cost) <- sapply(names(df.cost), function(x) gsub("\'", "", x))  # remove all the god damn single quotes from column names
  df.cost$HOSPID <-  sub("^[0]+", "", df.cost$HOSPID)  
  
  df <- merge(core.data, df.cost, by = c("HOSPID","YEAR"), all.x = T)
  return(df)
}

link_hcup_sid_core_with_severity <- function() {
  
}

link_hcup_sid_core_with_dxprgrp <- function() {
  
}