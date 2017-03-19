# COPYRIGHT: 2014-present
# AUTHOR: Zhifei Sun, zhifei.sun@duke.edu
# PURPOSE: Repository for custom functions used in every project

# FUNCTIONS ----------------------------------------------------------
select_hcup_sid_core_cases <- function(core.file, core.variables=c(), revisit.file) {
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
  
  # # read and link AHA linkage file
  # df.aha <- fread(aha.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
  # setnames(df.aha,toupper(names(df.aha)))
  # 
  # setkey(df.core,DSHOSPID)
  # setkey(df.aha,DSHOSPID)
  # df <- df.aha[df.core]  # merge

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

calculate_readmission_column <- function(index_df, readmission_df) {
  a$READMIT_NUM <- 0            # initialize number of times readmitted
  a$TOTCHG_SUM <- a$TOTCHG      # initialize total charge to index admission charge
  a$PREV_ADMIT <- 0             # initialize any previous admissions before index readmission
  f <- data.frame()             # initialize data table for readmissions
  counter <- 0
  c$INIT_PROC <- NA
  
  # for loop runs through entire untouched/broader data frame to look for readmissions (df = c)
  for(i in 1:dim(c)[1]) {
    x <- c[i, VISITLINK]
    y <- c[i,DAYSTOEVENT]
    if(x %in% a.visitlink.unique) {
      z <- a$DAYSTOEVENT[a$VISITLINK == x]
      if((y-z<365) & (y-z>0)) {          # only can readmit within 365 days
        a$READMIT_NUM[a$VISITLINK == x]  <- a$READMIT_NUM[a$VISITLINK == x] + 1             # if readmission found, increment by 1 
        a$TOTCHG_SUM[a$VISITLINK == x]   <- a$TOTCHG_SUM[a$VISITLINK == x]  + c[i, TOTCHG]  # also add charges together
        c$INIT_PROC[i] <- a$PROC_TOTAL[a$VISITLINK == x]                                    # set initial proc for the readmission data frame
        f <- rbind(f, c[i])                                                                 # append 'f' with encounter of the readmissions
        print(i) # display status
        
      }
      if(y < z) {
        a$PREV_ADMIT[a$VISITLINK == x] <- 1      # if previous admission found, then flag it        
      }
    }
  }
}

decode_hcup_sid <- function(a) {
  
  
  a[,PROC_TOTAL:="3@Antibiotics"]
  a$PROC_TOTAL[a$PROC_A == "APPY"] <- "1@Appy"
  a$PROC_TOTAL[a$PROC_B == "DRAIN"] <- "2@Drain"
  a$PROC_TOTAL[a$PROC_A == "APPY" & a$PROC_B == "DRAIN"] <- "1@Appy"
  a$PROC_TOTAL <- factor(a$PROC_TOTAL)
  
  a[RACE %in% c(1), RACE_L:="1@White"]
  a[RACE %in% c(2), RACE_L:="2@Black"]
  a[RACE %in% c(3:6), RACE_L:="3@Other"]
  a$RACE_L <- factor(a$RACE_L)
  
  a[PAY1 %in% c(3), PAY_L:="1@Private"]
  a[PAY1 %in% c(1,2), PAY_L:="2@Government"]
  a[PAY1 %in% c(4,5,6), PAY_L:="3@None"]
  a$PAY_L <- factor(a$PAY_L)
  
  
  g$HOSPITAL_VOLUME <- NA
  g$HOSPITAL_VOLUME [ g$HTYPE %in% c(1,3,5)] <- "1@Less Than 100 Beds"
  g$HOSPITAL_VOLUME [ g$HTYPE %in% c(2,4,6,7)] <- "2@More Than 100 Beds"
  g$HOSPITAL_VOLUME <- factor(g$HOSPITAL_VOLUME)
  label(g$HOSPITAL_VOLUME) <- "Hospital Volume"
  
  g$HOSPITAL_URBAN <- NA
  g$HOSPITAL_URBAN [ g$HTYPE %in% c(5,6,7)] <- "1@Urban"
  g$HOSPITAL_URBAN [ g$HTYPE %in% c(3,4)] <- "2@Rural"
  g$HOSPITAL_URBAN <- factor(g$HOSPITAL_URBAN)
  label(g$HOSPITAL_URBAN) <- "Hospital Urbanity"
  
  g$READMIT_NUM <- factor(g$READMIT_NUM)
  label(g$READMIT_NUM) <- "Readmission Number"
  
  g$HOSPST <- factor(g$HOSPST)
  label(g$HOSPST) <- "State"
  
  g$SEX_L <- NA
  g$SEX_L[g$FEMALE == 0] <-"1@Male"
  g$SEX_L[g$FEMALE == 1] <-"2@Female"
  label(g$SEX_L) <- "Sex"
  
  label(g$AGE) <- "Age At Diagnosis"
  g$YEAR <- factor(g$YEAR)
  label(g$YEAR) <- "Year of Index Admission"
  label(g$PAY_L) <- "Insurance Status"
  label(g$LOS) <- "Hospital Length of Stay (days)"
  label(g$DIED) <- "Death During Admission"
  label(g$READMIT) <- "Readmission Status"
  label(g$TOTCHG_2016) <- "Index Admission Charges (2016 USD)"
  label(g$TOTCHG_SUM_2016) <- "Cumulative Admission Charges (2016 USD)"
  label(g$RACE_L) <- "Race"
  
  
  
}