# COPYRIGHT: 2014-present
# AUTHOR: Zhifei Sun, zhifei.sun@duke.edu
# PURPOSE: Repository for custom functions used in every project

# test

# GLOBAL CONSTANTS ---------------------------------------------------
list.of.packages <- c("devtools","MatchIt", "rms" ,"Hmisc","ggplot2","gridExtra","lubridate","dplyr","tidyr",
                      "stringr","gmodels","data.table","extrafont", "maps","bit64")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# devtools::install_github('rstudio/DT')
# devtools::install_github("csgillespie/addinmanager")
# devtools::install_github("rstudio/addinexamples", type = "source")
# LIBRARIES
library(MatchIt)                    # propensity matching
library(survival)                   # survival analysis
library(rms)                        # restricted cubic spline
library(Hmisc)                      # summary tables
library(ggplot2)                    # plotting
library(plyr)                       # useful other functions
library(dplyr)
library(tidyr)
library(gridExtra)
library(reshape2)
library(lubridate)
library(stringr)
library(gmodels)                    # for CrossTable()
library(data.table)                 # big data analysis
library(extrafont)                  # for new york time fonts in plots
# font functions used everytime a new font is added to the system
 #font_import()
 #loadfonts()
#library(devtools)
library(maps)                       # maps
library(bit64)
library(grid)


# LOAD HISTOLOGY CODES
ICDO3_ADENOCARCINOMA <-c(8140,8141,8143,8144,8145,8147,8150,8210,8211,8260,8261,8262,8263,8310,8320,8323,8380,8401,8410,8440,8460,8470,8490,8500,8503,8510)
ICDO3_ADENOCARCINOMA_NEW <-c(8000,8010,8140:8145,8147,8150,8210,8211,8255,8260,8261,8262,8263,8310,8320,8323,8380,8401,8410,8440,8460,8470,8480,8481,8490,8500,8503,8510,8560)
ICDO3_ADENOCARCINOMA_OLD <-c(8140, 8141, 8143, 8144, 8145, 8147, 8150, 8210, 8211 ,8260, 8261, 8262, 8263, 8310, 8320, 8323, 8380, 8401, 8410, 8440, 8460, 8470, 8490, 8500, 8503, 8510)
ICD03_CARCINOID <- c(8240)
ICD03_MELANOMA <- c(8720:8746)
ICDO3_SQUAMOUS <- c(8050,8051,8052,8053,8060,8070,8071,8072,8073,8074,8075,8076,8077,8078,8083,8084)

# To have randomized events produce same value
set.seed(100) 

# DIRECTORIES
DIR.DB  <- "/users/zhifeisun/dropbox/Jeff/Work - Active/[Analysis]/databases/"

# FUNCTIONS ----------------------------------------------------------
HCUPSelectCase <- function(core.file, core.variables=c(),
                           aha.file, revisit.file) {
  library(data.table)
  library(bit64)
  df <- data.table()  # initialize df
  df.core <-data.table()
  df.aha <- data.table()
  df.revisit <- data.table()
  
  print(paste("Reading from", core.file))
  
  # checks to prevent errors
  core.variables <= toupper(core.variables)
  if (! ("KEY" %in% core.variables)) core.variables <- c("KEY", core.variables)  
  if (! ("DSHOSPID" %in% core.variables)) core.variables <- c("DSHOSPID", core.variables)  
  if (! ("YEAR" %in% core.variables)) core.variables <- c("YEAR", core.variables)  
  if (grepl("2009", core.file)) core.variables <- c("VISITLINK","DAYSTOEVENT", core.variables)
  if (grepl("2010", core.file)) core.variables <- c("VISITLINK","DAYSTOEVENT", core.variables)
  if (grepl("2011", core.file)) break("check customscript function here to add in revisit variables")
  
  # Core
  if (is.null(core.variables))  {
    df.core  <- data.table(fread(core.file, na.strings = c("","NA","."), verbose = F, showProgress = FALSE))
  } else {
    df.core  <- data.table(fread(core.file, na.strings = c("","NA","."), verbose = F, showProgress = FALSE,  select = core.variables))
  }
  #print(class(df.core))
  setnames(df.core,toupper(names(df.core)))
  
  #print(class(df.core))
  
  # AHA linkage file
  df.aha <- fread(aha.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
  setnames(df.aha,toupper(names(df.aha)))
  
  #df <- merge(df.core, df.aha, by=c("DSHOSPID","YEAR"), all.x =T, all.y=F)
  setkey(df.core,DSHOSPID)
  setkey(df.aha,DSHOSPID)
  df <- df.aha[df.core]
  
  #print(class(df))
  # Revisit file
  if(df.core$YEAR[1] < 2009) {           # after 2009, the revisit variable is embeded
    df.revisit <- fread(revisit.file, na.strings =  c("","NA","."),verbose = F, showProgress = FALSE)
    setnames(df.revisit,toupper(names(df.revisit)))
    #df <- merge(df, df.revisit, by="KEY", all.x =T, all.y=F)
    setkey(df,KEY)
    setkey(df.revisit,KEY)
    df <- df.revisit[df]
    #print(class(df))
  }
  setDT(df)
  return(df)
}


DecodeNCDBDataset <- function (NCDB_raw) {
  # Decodes raw NCDB csv file.
  # 
  # Args:
  #   NCDB_raw: raw NCDB data file in csv format
  #
  # Returns:
  #   Data frame with additional decoded variables 
  NCDB_decoded <- NCDB_raw 
  
  label(NCDB_decoded$AGE) <- "Age (years)"
  NCDB_decoded$AGE[NCDB_decoded$AGE==999] <- NA
  
  NCDB_decoded$AGE_CAT <- NA
  NCDB_decoded$AGE_CAT[NCDB_decoded$AGE<50] <- '1@<50'
  NCDB_decoded$AGE_CAT[NCDB_decoded$AGE>=50 & NCDB_decoded$AGE<60] <- '2@50-59'
  NCDB_decoded$AGE_CAT[NCDB_decoded$AGE>=60 & NCDB_decoded$AGE<70] <- '3@60-69'
  NCDB_decoded$AGE_CAT[NCDB_decoded$AGE>=70 & NCDB_decoded$AGE<80] <- '4@70-79'
  NCDB_decoded$AGE_CAT[NCDB_decoded$AGE>=80] <- '5@>79'
  NCDB_decoded$AGE_CAT <- as.factor(NCDB_decoded$AGE_CAT)
  
  NCDB_decoded$AGE_BY_DECADE <- floor(0.5 + NCDB_decoded$AGE / 10)
  
  NCDB_decoded$SEX_L <- NA
  NCDB_decoded$SEX_L[NCDB_decoded$SEX==1] <- "1@Male"
  NCDB_decoded$SEX_L[NCDB_decoded$SEX==2] <- "2@Female"
  NCDB_decoded$SEX_L <- factor(NCDB_decoded$SEX_L)
  label(NCDB_decoded$SEX_L) <- "Gender"
  
  NCDB_decoded$RACE_L <- NA
  NCDB_decoded$RACE_L[NCDB_decoded$RACE==1] <- "1@White"
  NCDB_decoded$RACE_L[NCDB_decoded$RACE==2] <- "2@Black"
  NCDB_decoded$RACE_L[NCDB_decoded$RACE==99] <- NA
  NCDB_decoded$RACE_L[!(NCDB_decoded$RACE %in% c(1,2,99))] <- "3@Other"
  NCDB_decoded$RACE_L <- factor(NCDB_decoded$RACE_L)
  label(NCDB_decoded$RACE_L) <- "Race"
  
  
  NCDB_decoded$INSURANCE_STATUS_L <- NA

  NCDB_decoded$INSURANCE_STATUS_L[NCDB_decoded$INSURANCE_STATUS==0] <- "1@None"
  NCDB_decoded$INSURANCE_STATUS_L[NCDB_decoded$INSURANCE_STATUS %in% c(1)] <- "2@Private"
  NCDB_decoded$INSURANCE_STATUS_L[NCDB_decoded$INSURANCE_STATUS %in% c(2,3,4)] <- "3@Government"
  NCDB_decoded$INSURANCE_STATUS_L[NCDB_decoded$INSURANCE_STATUS==9] <- NA
  NCDB_decoded$INSURANCE_STATUS_L <- factor(NCDB_decoded$INSURANCE_STATUS_L)
  label(NCDB_decoded$INSURANCE_STATUS_L) <- "Insurance Status"
  
  NCDB_decoded$INCOME_QUARTILE <- NA
  NCDB_decoded$INCOME_QUARTILE[NCDB_decoded$MED_INC_QUAR_00==1] <- "1@Bottom"
  NCDB_decoded$INCOME_QUARTILE[NCDB_decoded$MED_INC_QUAR_00==2] <- "2@Second"
  NCDB_decoded$INCOME_QUARTILE[NCDB_decoded$MED_INC_QUAR_00==3] <- "3@Third"
  NCDB_decoded$INCOME_QUARTILE[NCDB_decoded$MED_INC_QUAR_00==4] <- "4@Top"
  NCDB_decoded$INCOME_QUARTILE[NCDB_decoded$MED_INC_QUAR_00==9] <- NA
  NCDB_decoded$INCOME_QUARTILE <- factor(NCDB_decoded$INCOME_QUARTILE)
  label(NCDB_decoded$INCOME_QUARTILE) <- "Income Level"
  
  NCDB_decoded$INCOME_MEDIAN <- NA
  NCDB_decoded$INCOME_MEDIAN[NCDB_decoded$MED_INC_QUAR_00 %in% c(1,2)] <- "1@Below Median"
  NCDB_decoded$INCOME_MEDIAN[NCDB_decoded$MED_INC_QUAR_00 %in% c(3,4)] <- "2@Above Median"
  NCDB_decoded$INCOME_MEDIAN[NCDB_decoded$MED_INC_QUAR_00==9] <- NA
  NCDB_decoded$INCOME_MEDIAN <- factor(NCDB_decoded$INCOME_MEDIAN)
  label(NCDB_decoded$INCOME_MEDIAN) <- "Income Level"
  
  NCDB_decoded$EDUCATION_QUARTILE[NCDB_decoded$NO_HSD_QUAR_00==1] <- "1@Bottom"
  NCDB_decoded$EDUCATION_QUARTILE[NCDB_decoded$NO_HSD_QUAR_00==2] <- "2@Second"
  NCDB_decoded$EDUCATION_QUARTILE[NCDB_decoded$NO_HSD_QUAR_00==3] <- "3@Third"
  NCDB_decoded$EDUCATION_QUARTILE[NCDB_decoded$NO_HSD_QUAR_00==4] <- "4@Top"
  NCDB_decoded$EDUCATION_QUARTILE[NCDB_decoded$NO_HSD_QUAR_00==9] <- NA
  NCDB_decoded$EDUCATION_QUARTILE <- as.factor(NCDB_decoded$EDUCATION_QUARTILE)
  label(NCDB_decoded$EDUCATION_QUARTILE) <- "Education Level"
  
  NCDB_decoded$EDUCATION_MEDIAN[NCDB_decoded$NO_HSD_QUAR_00 %in% c(1,2)] <- "1@Below Median"
  NCDB_decoded$EDUCATION_MEDIAN[NCDB_decoded$NO_HSD_QUAR_00 %in% c(3,4)] <- "2@Above Median"
  NCDB_decoded$EDUCATION_MEDIAN[NCDB_decoded$NO_HSD_QUAR_00==9] <- NA
  NCDB_decoded$EDUCATION_MEDIAN <- as.factor(NCDB_decoded$EDUCATION_MEDIAN)
  label(NCDB_decoded$EDUCATION_MEDIAN) <- "Education Level"
  
  NCDB_decoded$FACILITY_TYPE_CD_L <-NA
  NCDB_decoded$FACILITY_TYPE_CD_L[NCDB_decoded$FACILITY_TYPE_CD==1] <- "1@Community"
  NCDB_decoded$FACILITY_TYPE_CD_L[NCDB_decoded$FACILITY_TYPE_CD==2] <- "2@Comprehensive"
  NCDB_decoded$FACILITY_TYPE_CD_L[NCDB_decoded$FACILITY_TYPE_CD==3] <- "3@Academic"
  NCDB_decoded$FACILITY_TYPE_CD_L[NCDB_decoded$FACILITY_TYPE_CD==9] <- NA
  NCDB_decoded$FACILITY_TYPE_CD_L <- factor(NCDB_decoded$FACILITY_TYPE_CD_L)
  label(NCDB_decoded$FACILITY_TYPE_CD_L) <- "Facility Type"
  
  NCDB_decoded$URBAN <- NA
  NCDB_decoded$URBAN [NCDB_decoded$UR_CD_13 %in% c(1,2,3)] <- "1@Metro"
  NCDB_decoded$URBAN [NCDB_decoded$UR_CD_13 %in% c(4,5,6,7,8,9)] <- "2@Rural"
  NCDB_decoded$URBAN <- factor(NCDB_decoded$URBAN)
  label(NCDB_decoded$URBAN) <- "Metro-Rural"
  
  # for what each location mean: 
  # http://ncdbpuf.facs.org/?q=content/participant-use-file-%E2%80%93-facility-location-census-region
  NCDB_decoded$FACILITY_LOCATION_CD_L <-NA
  
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==1] <- "1@New England"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==2] <- "2@Middle Atlantic"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==3] <- "3@South Atlantic"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==4] <- "4@East North Central"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==5] <- "5@East South Central"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==6] <- "6@West North Central"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==7] <- "7@West South Central"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==8] <- "8@Mountain"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==9] <- "9@Pacific"
  NCDB_decoded$FACILITY_LOCATION_CD_L[NCDB_decoded$FACILITY_LOCATION_CD==0] <- NA
  NCDB_decoded$FACILITY_LOCATION_CD_L <- factor(NCDB_decoded$FACILITY_LOCATION_CD_L)
  label(NCDB_decoded$FACILITY_LOCATION_CD_L) <- "Facility Location"
  
  NCDB_decoded$CDCC_TOTAL_L <- NA
  NCDB_decoded$CDCC_TOTAL_L[NCDB_decoded$CDCC_TOTAL==0] <- "1@0"
  NCDB_decoded$CDCC_TOTAL_L[NCDB_decoded$CDCC_TOTAL==1] <- "2@1"
  NCDB_decoded$CDCC_TOTAL_L[NCDB_decoded$CDCC_TOTAL>=2] <- "3@2+"
  NCDB_decoded$CDCC_TOTAL_L <- factor(NCDB_decoded$CDCC_TOTAL_L)
  label(NCDB_decoded$CDCC_TOTAL_L) <- "Charlson-Deyo Comorbidity Index"
  
  NCDB_decoded$YEAR_OF_DIAGNOSIS_L <- NA
  NCDB_decoded$YEAR_OF_DIAGNOSIS_L <- NCDB_decoded$YEAR_OF_DIAGNOSIS
  NCDB_decoded$YEAR_OF_DIAGNOSIS_L[NCDB_decoded$YEAR_OF_DIAGNOSIS==99] <- NA
  NCDB_decoded$YEAR_OF_DIAGNOSIS_L
  NCDB_decoded$YEAR_OF_DIAGNOSIS_L <- factor(NCDB_decoded$YEAR_OF_DIAGNOSIS_L)
  label(NCDB_decoded$YEAR_OF_DIAGNOSIS_L) <- "Year of Diagnosis"
  
  label(NCDB_decoded$PRIMARY_SITE) <- "Primary Site"
  label(NCDB_decoded$LATERALITY) <- "Laterality"
  
  NCDB_decoded$GRADE_L <- NA
  NCDB_decoded$GRADE_L[NCDB_decoded$GRADE %in% c(1,2)] <- "1@Well to Moderately differentiated"
  NCDB_decoded$GRADE_L[NCDB_decoded$GRADE %in% c(3,4)] <- "2@Poorly or Undifferentiated"
  NCDB_decoded$GRADE_L <- factor(NCDB_decoded$GRADE_L)
  label(NCDB_decoded$GRADE_L) <- "Tumor Grade"
  
  
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L <- NA
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="0"] <- "0"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1A"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1A1"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1A2"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1B"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="1C"] <- "1"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="2"] <- "2"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="2A"] <- "2"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="2B"] <- "2"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="2C"] <- "2"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="3"] <- "3"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="3A"] <- "3"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="3B"] <- "3"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="3C"] <- "3"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="4"] <- "4"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="4A"] <- "4"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="4B"] <- "4"
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="88"] <- NA
  NCDB_decoded$TNM_CLIN_STAGE_GROUP_L[NCDB_decoded$TNM_CLIN_STAGE_GROUP=="99"] <- NA
  label(NCDB_decoded$TNM_CLIN_STAGE_GROUP_L) <- "Clinical Stage Group"
  
  label(NCDB_decoded$TNM_CLIN_T) <- "Clinical T"
  NCDB_decoded$TNM_CLIN_T[NCDB_decoded$TNM_CLIN_T=="88"] <- NA
  NCDB_decoded$TNM_CLIN_T[NCDB_decoded$TNM_CLIN_T=="X"] <- NA
  NCDB_decoded$TNM_CLIN_T <- factor(NCDB_decoded$TNM_CLIN_T)
  
  label(NCDB_decoded$TNM_CLIN_N) <- "Clinical N"
  NCDB_decoded$TNM_CLIN_N[NCDB_decoded$TNM_CLIN_N=="88"] <- NA
  NCDB_decoded$TNM_CLIN_N[NCDB_decoded$TNM_CLIN_N=="X"] <- NA
  NCDB_decoded$TNM_CLIN_N <- factor(NCDB_decoded$TNM_CLIN_N)
  
  label(NCDB_decoded$TNM_CLIN_M) <- "Clinical M"
  NCDB_decoded$TNM_CLIN_M[NCDB_decoded$TNM_CLIN_M=="88"] <- NA
  NCDB_decoded$TNM_CLIN_M[NCDB_decoded$TNM_CLIN_M=="X"] <- NA
  NCDB_decoded$TNM_CLIN_M <- factor(NCDB_decoded$TNM_CLIN_M)
  
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L <- NA
  label(NCDB_decoded$TNM_PATH_STAGE_GROUP_L) <- "Path Stage Group"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="0"] <- "0"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="1"] <- "1"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="2"] <- "2"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="2A"] <- "2"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="2B"] <- "2"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="2C"] <- "2"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="3"] <- "3"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="3A"] <- "3"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="3B"] <- "3"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="3C"] <- "3"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="4"] <- "4"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="4A"] <- "4"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="4B"] <- "4"
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="88"] <- NA
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L[NCDB_decoded$TNM_PATH_STAGE_GROUP=="99"] <- NA
  NCDB_decoded$TNM_PATH_STAGE_GROUP_L <- as.factor(NCDB_decoded$TNM_PATH_STAGE_GROUP_L)
  
  label(NCDB_decoded$TNM_PATH_T) <- "Path T"
  NCDB_decoded$TNM_PATH_T[NCDB_decoded$TNM_PATH_T=="88"] <- NA
  NCDB_decoded$TNM_PATH_T[NCDB_decoded$TNM_PATH_T=="X"] <- NA
  NCDB_decoded$TNM_PATH_T <- as.factor(NCDB_decoded$TNM_PATH_T)
  
  label(NCDB_decoded$TNM_PATH_N) <- "Path N"
  NCDB_decoded$TNM_PATH_N[NCDB_decoded$TNM_PATH_N=="88"] <- NA
  NCDB_decoded$TNM_PATH_N[NCDB_decoded$TNM_PATH_N=="X"] <- NA
  NCDB_decoded$TNM_PATH_N <- as.factor(NCDB_decoded$TNM_PATH_N)
  
  label(NCDB_decoded$TNM_PATH_M) <- "Path M"
  NCDB_decoded$TNM_PATH_M[NCDB_decoded$TNM_PATH_M=="88"] <- NA
  NCDB_decoded$TNM_PATH_M[NCDB_decoded$TNM_PATH_M=="X"] <- NA
  NCDB_decoded$TNM_PATH_M <- as.factor(NCDB_decoded$TNM_PATH_M)
  
  # Surgical procedures -----------------------------------------------------
  
  NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L <- NA
  label(NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L) <- "Surgical Procedure"
  
  if (NCDB_decoded$PRIMARY_SITE[1] == "C209")    # RECTUM
  {
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 0] <- "NONE"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(10:19)] <- "Local Destruction"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(20:29)] <- "Local Excision"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(30:39)] <- "Segmental (inclu. LAR)"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(40:49)] <- "Segmental with pull thru"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(50:59)] <- "Total Proctectomy (inclu APR)"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(60)] <- "Total Proctectomy (inclu APR)"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 70] <- "Total Proctectomy (inclu APR)"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 80] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 90] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 99] <- "Unknown"
  }
  if (NCDB_decoded$PRIMARY_SITE[1] == "C199")    # RECTOSIGMOID
  {

  }
  if (NCDB_decoded$PRIMARY_SITE[1] %in% c("C180","C181","C182","C183","C184","C185","C186","C187","C188","C189"))   # COLON
  {
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 0] <- "NONE"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(20:29)] <- "Local"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(30:39)] <- "Segmental"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(40:49)] <- "Subtotal"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(50:59)] <- "Total Colectomy"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(60:69)] <- "Total Proctocolectomy"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 70] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 80] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 90] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 99] <- NA
  }
  if (NCDB_decoded$PRIMARY_SITE[1] %in% c("C150","C151","C152","C153","C154","C155","C158","C159"))   # ESOPHAGUS
  {
    NCDB_decoded$PRIMARY_SITE_L <- NA
    label(NCDB_decoded$PRIMARY_SITE_L) <- "Primary Site"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C150"] <- "Cervical"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C151"] <- "Thoracic"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C152"] <- "Abdominal"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C153"] <- "Upper 1/3"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C154"] <- "Middle 1/3"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C155"] <- "Lower 1/3"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C158"] <- "Overlapping"
    NCDB_decoded$PRIMARY_SITE_L[NCDB_decoded$PRIMARY_SITE == "C159"] <- "Unknown"
    
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 0] <- "None"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(10:29)] <- "Local"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(30)] <- "Partial Esophagectomy"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(40)] <- "Total Esophagectomy"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(50:55)] <- "Esophagectomy with laryngectomy and/or gastrectomy"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE %in% c(80)] <- "Esophagectomy NOS"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 90] <- "Unknown"
    NCDB_decoded$RX_SUMM_SURG_PRIM_SITE_L[NCDB_decoded$RX_SUMM_SURG_PRIM_SITE == 99] <- NA
  }
  
 
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L <- NA
  label(NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L) <- "Radiation Therapy"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 0] <- "1None"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 2] <- "2Neoadjuvant"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 3] <- "3Adjuvant"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 4] <- "4Sandwich"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 5] <- "5Intraop"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 6] <- "6Intraop with sandwich"
  NCDB_decoded$RX_SUMM_SURGRAD_SEQ_L[NCDB_decoded$RX_SUMM_SURGRAD_SEQ == 9] <- NA
  
  NCDB_decoded$PREOP_XRT <- NA
  NCDB_decoded$PREOP_XRT [NCDB_decoded$RX_SUMM_SURGRAD_SEQ %in% c(0,3,5)] <- "No"
  NCDB_decoded$PREOP_XRT [NCDB_decoded$RX_SUMM_SURGRAD_SEQ %in% c(2,4)] <- "Yes"
  NCDB_decoded$PREOP_XRT <- factor(NCDB_decoded$PREOP_XRT)
  label(NCDB_decoded$PREOP_XRT) <- "Preoperative Radiotherapy"
  
  NCDB_decoded$POSTOP_XRT <- NA
  NCDB_decoded$POSTOP_XRT [NCDB_decoded$RX_SUMM_SURGRAD_SEQ %in% c(0,2,5)] <- "No"
  NCDB_decoded$POSTOP_XRT [NCDB_decoded$RX_SUMM_SURGRAD_SEQ %in% c(3,4)] <- "Yes"
  NCDB_decoded$POSTOP_XRT <- factor(NCDB_decoded$POSTOP_XRT)
  label(NCDB_decoded$POSTOP_XRT) <- "Postoperative Radiotherapy"
  
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L <- NA
  label(NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L) <- "Systemic Therapy"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 0] <- "1None"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 2] <- "2Neoadjuvant"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 3] <- "3Adjuvant"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 4] <- "4Sandwich"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 5] <- "5Intraop"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 6] <- "6Intraop with sandwich"
  NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ_L[NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ == 9] <- NA
  
  NCDB_decoded$PREOP_CHEMO <- NA
  NCDB_decoded$PREOP_CHEMO [NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ %in% c(0,3,5)] <- "No"
  NCDB_decoded$PREOP_CHEMO [NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ %in% c(2,4)] <- "Yes"
  NCDB_decoded$PREOP_CHEMO <- factor(NCDB_decoded$PREOP_CHEMO)
  label(NCDB_decoded$PREOP_CHEMO) <- "Preoperative Chemotherapy"
  
  NCDB_decoded$POSTOP_CHEMO <- NA
  NCDB_decoded$POSTOP_CHEMO [NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ %in% c(0,2,5)]<- "No"
  NCDB_decoded$POSTOP_CHEMO [NCDB_decoded$RX_SUMM_SYSTEMIC_SUR_SEQ %in% c(3,4)] <- "Yes"
  NCDB_decoded$POSTOP_CHEMO <- factor(NCDB_decoded$POSTOP_CHEMO)
  label(NCDB_decoded$POSTOP_CHEMO) <- "Postoperative Chemotherapy"
  
  # preop chemo AND xrt
  NCDB_decoded$PREOP_CHEMOXRT <- NA
  NCDB_decoded$PREOP_CHEMOXRT[NCDB_decoded$PREOP_XRT == "No" | NCDB_decoded$PREOP_CHEMO == "No"] <- "No"
  NCDB_decoded$PREOP_CHEMOXRT[NCDB_decoded$PREOP_XRT == "Yes" & NCDB_decoded$PREOP_CHEMO == "Yes"] <- "Yes"
  NCDB_decoded$PREOP_CHEMOXRT <- factor(NCDB_decoded$PREOP_CHEMOXRT)
  label(NCDB_decoded$PREOP_CHEMOXRT) <- "Preop Chemo AND XRT"
  
  # preop chemo OR xrt
  NCDB_decoded$PREOP_CHEMO_OR_XRT <- NA
  NCDB_decoded$PREOP_CHEMO_OR_XRT[NCDB_decoded$PREOP_XRT == "No" & NCDB_decoded$PREOP_CHEMO == "No"] <- "No"
  NCDB_decoded$PREOP_CHEMO_OR_XRT[NCDB_decoded$PREOP_XRT == "Yes" | NCDB_decoded$PREOP_CHEMO == "Yes"] <- "Yes"
  NCDB_decoded$PREOP_CHEMO_OR_XRT <- factor(NCDB_decoded$PREOP_CHEMO_OR_XRT)
  label(NCDB_decoded$PREOP_CHEMO_OR_XRT) <- "Preop Chemo OR XRT"
  
  # postop chemo And xrt
  NCDB_decoded$POSTOP_CHEMO_AND_XRT <- NA
  NCDB_decoded$POSTOP_CHEMO_AND_XRT[NCDB_decoded$POSTOP_XRT == "No" | NCDB_decoded$POSTOP_CHEMO == "No"] <- "No"
  NCDB_decoded$POSTOP_CHEMO_AND_XRT[NCDB_decoded$POSTOP_XRT == "Yes" & NCDB_decoded$POSTOP_CHEMO == "Yes"] <- "Yes"
  NCDB_decoded$POSTOP_CHEMO_AND_XRT <- factor(NCDB_decoded$POSTOP_CHEMO_AND_XRT)
  label(NCDB_decoded$POSTOP_CHEMO_AND_XRT) <- "Postop Chemo AND XRT"
  
  # postop chemo OR xrt
  NCDB_decoded$POSTOP_CHEMO_OR_XRT <- NA
  NCDB_decoded$POSTOP_CHEMO_OR_XRT[NCDB_decoded$POSTOP_XRT == "No" & NCDB_decoded$POSTOP_CHEMO == "No"] <- "No"
  NCDB_decoded$POSTOP_CHEMO_OR_XRT[NCDB_decoded$POSTOP_XRT == "Yes" | NCDB_decoded$POSTOP_CHEMO == "Yes"] <- "Yes"
  NCDB_decoded$POSTOP_CHEMO_OR_XRT <- factor(NCDB_decoded$POSTOP_CHEMO_OR_XRT)
  label(NCDB_decoded$POSTOP_CHEMO_OR_XRT) <- "Postop Chemo OR XRT"
  
  
  
  NCDB_decoded$TUMOR_SIZE_L <- NCDB_decoded$TUMOR_SIZE
  label(NCDB_decoded$TUMOR_SIZE_L) <- "Tumor Size (mm)"
  NCDB_decoded$TUMOR_SIZE_L[NCDB_decoded$TUMOR_SIZE %in% c(0,990:999)] <- NA
  
  NCDB_decoded$TUMOR_SIZE_CAT <- NA
  NCDB_decoded$TUMOR_SIZE_CAT[NCDB_decoded$TUMOR_SIZE==0] <- '1@No primary found'
  NCDB_decoded$TUMOR_SIZE_CAT[(NCDB_decoded$TUMOR_SIZE>=0 & NCDB_decoded$TUMOR_SIZE<10) | NCDB_decoded$TUMOR_SIZE==990 | NCDB_decoded$TUMOR_SIZE==991] <- '2@< 1 cm'
  NCDB_decoded$TUMOR_SIZE_CAT[(NCDB_decoded$TUMOR_SIZE>=10 & NCDB_decoded$TUMOR_SIZE<20) | NCDB_decoded$TUMOR_SIZE==992] <- '3@1-1.9 cm'
  NCDB_decoded$TUMOR_SIZE_CAT[(NCDB_decoded$TUMOR_SIZE>=20 & NCDB_decoded$TUMOR_SIZE<50) | NCDB_decoded$TUMOR_SIZE==993 | NCDB_decoded$TUMOR_SIZE==994 | NCDB_decoded$TUMOR_SIZE==995] <- '4@2-4.9 cm'
  NCDB_decoded$TUMOR_SIZE_CAT[NCDB_decoded$TUMOR_SIZE>=50 & NCDB_decoded$TUMOR_SIZE<990] <- '5@> 4.9 cm'
  NCDB_decoded$TUMOR_SIZE_CAT[NCDB_decoded$TUMOR_SIZE>=996 & NCDB_decoded$TUMOR_SIZE<1000] <- NA
  NCDB_decoded$TUMOR_SIZE_CAT <- as.factor(NCDB_decoded$TUMOR_SIZE_CAT)
  label(NCDB_decoded$TUMOR_SIZE_CAT) <- "Tumor Size"
  
  label(NCDB_decoded$REGIONAL_NODES_EXAMINED) <- "Lymph Nodes Examined (number)"
  NCDB_decoded$REGIONAL_NODES_EXAMINED[NCDB_decoded$REGIONAL_NODES_EXAMINED>=90] <-NA
  
  label(NCDB_decoded$REGIONAL_NODES_POSITIVE) <- "Lymph Nodes Positive (number)"
  NCDB_decoded$REGIONAL_NODES_POSITIVE[NCDB_decoded$REGIONAL_NODES_POSITIVE>=90] <-NA
  
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L <- NA
  
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 0] <- "1@Negative"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 1] <- "2@Positive"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 2] <- "2@Positive"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 3] <- "2@Positive"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L <- factor(NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L)
  label(NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L) <- "Surgical Margins"

  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v <- NA
  
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 0] <- "1@Negative"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 1] <- "2@Positive-Macroscopic"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 2] <- "3@Positive-Microscopic"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v[NCDB_decoded$RX_SUMM_SURGICAL_MARGINS == 3] <- "2@Positive-Macroscopic"
  NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v <- factor(NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v)
  label(NCDB_decoded$RX_SUMM_SURGICAL_MARGINS_L_v) <- "Surgical Margins"
  
  NCDB_decoded$SURG_DISCHARGE_DAYS_L <- NCDB_decoded$SURG_DISCHARGE_DAYS
  label(NCDB_decoded$SURG_DISCHARGE_DAYS_L) <- "Hospital Length of Stay (days)"
  NCDB_decoded$SURG_DISCHARGE_DAYS_L[NCDB_decoded$SURG_DISCHARGE_DAYS==0] <-NA
  #NCDB_decoded$SURG_DISCHARGE_DAYS_L[NCDB_decoded$SURG_DISCHARGE_DAYS==1] <-NA
  
  NCDB_decoded$READM_HOSP_30_DAYS_L <- NA
  NCDB_decoded$READM_HOSP_30_DAYS_L[NCDB_decoded$READM_HOSP_30_DAYS==0] <-"No"
  NCDB_decoded$READM_HOSP_30_DAYS_L[NCDB_decoded$READM_HOSP_30_DAYS==1] <-"Yes"
  NCDB_decoded$READM_HOSP_30_DAYS_L[NCDB_decoded$READM_HOSP_30_DAYS==2] <-"No"
  NCDB_decoded$READM_HOSP_30_DAYS_L[NCDB_decoded$READM_HOSP_30_DAYS==3] <-"Yes"
  NCDB_decoded$READM_HOSP_30_DAYS_L[NCDB_decoded$READM_HOSP_30_DAYS==9] <-NA
  NCDB_decoded$READM_HOSP_30_DAYS_L <- factor(NCDB_decoded$READM_HOSP_30_DAYS_L)
  label(NCDB_decoded$READM_HOSP_30_DAYS_L) <- "30-Day Unplanned Readmission"
  
  
  NCDB_decoded$PUF_30_DAY_MORT_CD_L <- NA
  NCDB_decoded$PUF_30_DAY_MORT_CD_L[NCDB_decoded$PUF_30_DAY_MORT_CD==0] <-"No"
  NCDB_decoded$PUF_30_DAY_MORT_CD_L[NCDB_decoded$PUF_30_DAY_MORT_CD==1] <-"Yes"
  NCDB_decoded$PUF_30_DAY_MORT_CD_L[NCDB_decoded$PUF_30_DAY_MORT_CD==9] <-NA
  NCDB_decoded$PUF_30_DAY_MORT_CD_L <- factor(NCDB_decoded$PUF_30_DAY_MORT_CD_L)
  label(NCDB_decoded$PUF_30_DAY_MORT_CD_L) <- "30-Day Mortality"
  
  
  NCDB_decoded$PUF_90_DAY_MORT_CD_L <- NA
  NCDB_decoded$PUF_90_DAY_MORT_CD_L[NCDB_decoded$PUF_90_DAY_MORT_CD==0] <-"No"
  NCDB_decoded$PUF_90_DAY_MORT_CD_L[NCDB_decoded$PUF_90_DAY_MORT_CD==1] <-"Yes"
  NCDB_decoded$PUF_90_DAY_MORT_CD_L[NCDB_decoded$PUF_90_DAY_MORT_CD==9] <-NA
  NCDB_decoded$PUF_90_DAY_MORT_CD_L <- factor(NCDB_decoded$PUF_90_DAY_MORT_CD_L)
  label(NCDB_decoded$PUF_90_DAY_MORT_CD_L) <- "90-Day Mortality"
  
  #recode the PUF vital status variable for R survival functions
  NCDB_decoded$PUF_VITAL_STATUS_RFORMAT <- NA
  NCDB_decoded$PUF_VITAL_STATUS_RFORMAT[NCDB_decoded$PUF_VITAL_STATUS == 1 ] <- 0    #alive
  NCDB_decoded$PUF_VITAL_STATUS_RFORMAT[NCDB_decoded$PUF_VITAL_STATUS == 0 ] <- 1    #dead
  
  NCDB_decoded$DX_DEFSURG_STARTED_MONTH <- NCDB_decoded$DX_DEFSURG_STARTED_DAYS/30.25
  
  NCDB_decoded$FOLLOWUP_AFTER_SURGERY <- NCDB_decoded$DX_LASTCONTACT_DEATH_MONTHS - NCDB_decoded$DX_DEFSURG_STARTED_MONTH
  NCDB_decoded$FOLLOWUP_AFTER_SURGERY [NCDB_decoded$FOLLOWUP_AFTER_SURGERY < 0] <- NA
  
  return(NCDB_decoded) 
}  

DecodeNCDBTNMStage <- function (DB) {
  

  DB$YEAR_OF_DIAGNOSIS_L <- factor(DB$YEAR_OF_DIAGNOSIS_L)
  label(DB$YEAR_OF_DIAGNOSIS_L) <- "Year of Diagnosis"
  
  label(DB$TNM_CLIN_STAGE_GROUP_L) <- "Clinical Stage"
  
  # clin M
  
  DB$TNM_CLIN_T_L <- NA
  DB$TNM_CLIN_T_L[DB$TNM_CLIN_T %in% c("0","IS")]           <- "T0/is"
  DB$TNM_CLIN_T_L[DB$TNM_CLIN_T %in% c("1","1A","1B","1C")] <- "T1"
  DB$TNM_CLIN_T_L[DB$TNM_CLIN_T %in% c('2',"2A",'2B','2C')] <- "T2"
  DB$TNM_CLIN_T_L[DB$TNM_CLIN_T %in% c('3','3A','3B','3C')] <- "T3"
  DB$TNM_CLIN_T_L[DB$TNM_CLIN_T %in% c("4","4A","4B")]      <- "T4"
  label(DB$TNM_CLIN_T_L) <- "Clinical T Stage"
  
  DB$TNM_CLIN_T_NUM <- NA
  DB$TNM_CLIN_T_NUM[DB$TNM_CLIN_T %in% c("0","IS")] <- 0
  DB$TNM_CLIN_T_NUM[DB$TNM_CLIN_T %in% c("1","1A","1B","1C")] <- 1
  DB$TNM_CLIN_T_NUM[DB$TNM_CLIN_T %in% c('2',"2A",'2B','2C')] <- 2
  DB$TNM_CLIN_T_NUM[DB$TNM_CLIN_T %in% c('3','3A','3B','3C')] <- 3
  DB$TNM_CLIN_T_NUM[DB$TNM_CLIN_T %in% c("4","4A","4B")] <- 4
  
  # clin N
  
  DB$TNM_CLIN_N_L <- NA
  DB$TNM_CLIN_N_L[DB$TNM_CLIN_N %in% c("0")] <- "N0"
  DB$TNM_CLIN_N_L[DB$TNM_CLIN_N %in% c("1","1A","1B","1C")] <- "N1"
  DB$TNM_CLIN_N_L[DB$TNM_CLIN_N %in% c("2","2A","2B","2C")] <- "N2"
  DB$TNM_CLIN_N_L[DB$TNM_CLIN_N %in% c("3","3A","3B","3C")] <- "N3"
  DB$TNM_CLIN_N_L <- factor(DB$TNM_CLIN_N_L)
  label(DB$TNM_CLIN_N_L) <- "Clinical N Stage"
    
  DB$TNM_CLIN_N_NUM <- NA
  DB$TNM_CLIN_N_NUM[DB$TNM_CLIN_N %in% c("0")] <- 0
  DB$TNM_CLIN_N_NUM[DB$TNM_CLIN_N %in% c("1","1A","1B","1C")] <- 1
  DB$TNM_CLIN_N_NUM[DB$TNM_CLIN_N %in% c("2","2A","2B","2C")] <- 2
  DB$TNM_CLIN_N_NUM[DB$TNM_CLIN_N %in% c("3","3A","3B","3C")] <- 3

  # path T
  
  DB$TNM_PATH_T_L <- NA
  DB$TNM_PATH_T_L[DB$TNM_PATH_T %in% c("0","IS")]           <- "T0/is"
  DB$TNM_PATH_T_L[DB$TNM_PATH_T %in% c("1","1A","1B","1C")] <- "T1"
  DB$TNM_PATH_T_L[DB$TNM_PATH_T %in% c('2',"2A",'2B','2C')] <- "T2"
  DB$TNM_PATH_T_L[DB$TNM_PATH_T %in% c('3','3A','3B','3C')] <- "T3"
  DB$TNM_PATH_T_L[DB$TNM_PATH_T %in% c("4","4A","4B")]      <- "T4"
  DB$TNM_PATH_T_L <- factor(DB$TNM_PATH_T_L)
  label(DB$TNM_PATH_T_L) <- "Pathologic T Stage"
  
  DB$TNM_PATH_T_NUM <- NA
  DB$TNM_PATH_T_NUM[DB$TNM_PATH_T %in% c("0","IS")]           <- 0
  DB$TNM_PATH_T_NUM[DB$TNM_PATH_T %in% c("1","1A","1B","1C")] <- 1
  DB$TNM_PATH_T_NUM[DB$TNM_PATH_T %in% c('2',"2A",'2B','2C')] <- 2
  DB$TNM_PATH_T_NUM[DB$TNM_PATH_T %in% c('3','3A','3B','3C')] <- 3
  DB$TNM_PATH_T_NUM[DB$TNM_PATH_T %in% c("4","4A","4B")]      <- 4
  
  # path N
  
  DB$TNM_PATH_N_L <- NA
  DB$TNM_PATH_N_L[DB$TNM_PATH_N %in% c("0")] <- "N0"
  DB$TNM_PATH_N_L[DB$TNM_PATH_N %in% c("1","1A","1B","1C")] <- "N1"
  DB$TNM_PATH_N_L[DB$TNM_PATH_N %in% c("2","2A","2B","2C")] <- "N2"
  DB$TNM_PATH_N_L[DB$TNM_PATH_N %in% c("3","3A","3B","3C")] <- "N3"
  DB$TNM_PATH_N_L <- factor(DB$TNM_PATH_N_L)
  label(DB$TNM_PATH_N_L) <- "Pathologic N Stage"
  
  DB$TNM_PATH_N_NUM <- NA
  DB$TNM_PATH_N_NUM[DB$TNM_PATH_N %in% c("0")] <- 0
  DB$TNM_PATH_N_NUM[DB$TNM_PATH_N %in% c("1","1A","1B","1C")] <- 1
  DB$TNM_PATH_N_NUM[DB$TNM_PATH_N %in% c("2","2A","2B","2C")] <- 2
  DB$TNM_PATH_N_NUM[DB$TNM_PATH_N %in% c("3","3A","3B","3C")] <- 3
  
  DB$TNM_PATH_M_L <- NA
  DB$TNM_PATH_M_L <- "1@M0"
  DB$TNM_PATH_M_L[DB$TNM_PATH_M %in% c("1","1A","1B")] <- "2@M1"
  DB$TNM_PATH_M_L <- factor(DB$TNM_PATH_M_L)
  label(DB$TNM_PATH_M_L) <- "Pathologic M Stage"
  
  
  
  label(DB$TNM_PATH_STAGE_GROUP_L) <- "Pathologic Stage"
  
  
  return (DB)
}

GetVarName <- function(x) { deparse(substitute(x)) }
  # Outputs variable name as a string.

CreateSummaryTable <- function(formula, data, title="Table1", save=TRUE, print.sd=T, na.include=F, exclude1=F, round=2,
                               continous.threshold = 3, prn=TRUE,long=TRUE,fisher=F, overall=T, test=T) {
  library("Hmisc")
  
  # fisher function
  u<-function(a,b){ 
    
    if (any(dim(a) < 2L)) {
      p <- list(P=9999, stat=1, df=1, testname="fisher", statname ="")  #won't output anything if variable does not have at least 2 rows and columns
    } else {
      j <- fisher.test(a) 
      p <- list(P=j$p.value, stat=1, df=1, testname=j$method, statname="") 
    }
    return(p) 
  } 
  
  if(fisher==F) {
    Table <-summaryM(formula=formula,  data=data, overall=overall ,test=test, na.include = na.include, continuous = continous.threshold, conTest = conTestkw, catTest = catTestchisq)
  }
  if(fisher==T) {
    Table <-summaryM(formula=formula,  data=data, overall=overall ,test=test, na.include = na.include, continuous = continous.threshold, conTest = conTestkw, catTest = u)
  }
  
  Table.print <- print(Table, what="%",  prn=prn,  prmsd=print.sd,   long=long,
                         exclude1=exclude1,   prtest='P', pctdig=1, round=round)

  if (save==TRUE) {
    write.table(Table.print, sep="," , file=paste(title,".csv",sep=""), col.names=F)
   
    print(paste("Table was saved in:",getwd()))
  } else { print("Table was not saved!")}
}

  
  
CreateOddsRatioTable <- function (gfit, save=TRUE, filename="Table_glm_OR.csv", round=3) {
  # create odds ratio table w/ Var name/OR/low95/hi95/pvalue from glm.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,4])
    or.table <- rbind(or.table, x)
  }
  
  or.table <- round(or.table, digits=round)
  # or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Odds Ratio', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  if(save==TRUE) { write.table(or.table, sep=',', file=filename, row.names = FALSE) }
  #print(or.table)
  return (or.table)
}

CreateLmTable <- function (gfit, save=TRUE, filename="Table_lm_estimates.csv", decimal_point=1) {
  # create table w/ Var name/estimate/low95/hi95/pvalue from lm, using a continuous outcome var.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=round(summary(gfit)$coef[i,1], decimal_point),
               round (confint.default(gfit)[i,1], decimal_point), 
               round (confint.default(gfit)[i,2], decimal_point),
               p.value=round(summary(gfit)$coef[i,4],3))
    or.table <- rbind(or.table, x)
  }
  #or.table <- round(or.table, digits=3)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Estimate', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  or.table <- or.table[-1,]
  
  if(save==TRUE) { write.table(or.table, sep=',', file=filename, row.names = FALSE) }
  return (or.table)
}

CreateLogLmTable <- function (gfit, save=TRUE, filename="Table_Loglm_estimates.csv", decimal_point=1) {
  # create table w/ Var name/OR/low95/hi95/pvalue from lm, using a log-transfromed outcome var.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory

  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,4])
    or.table <- rbind(or.table, x)
  }
  or.table <- round(or.table, digits=decimal_point)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('VAR', 'Estimate_OrigUnit', 'LO95_L', 'HI95_L', 'PVAL')
  or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  
  Intercept <- coef(gfit)["(Intercept)"]
  Estimate_OrigUnit <- round ((as.numeric(paste(or.table$Estimate_OrigUnit)) - 1) * exp(Intercept) , decimal_point)
  LO95_L <-   round ((as.numeric(paste(or.table$LO95_L)) - 1) * exp(Intercept) , decimal_point)
  HI95_L <- round((as.numeric(paste(or.table$HI95_L)) - 1) * exp(Intercept) , decimal_point)

  FinalTable <- data.frame(or.table$VAR, Estimate_OrigUnit,LO95_L,HI95_L,or.table$PVAL)
  colnames(FinalTable) <- c('Variables', 'Estimates', 'Lower 95% CI', 'Upper 95% CI', 'p-values')   # final column names
  
  #print(FinalTable)
  if(save==TRUE) { write.table(FinalTable, sep=',', file=filename, row.names = FALSE) }
  return (FinalTable)
}

CreateHazardRatioTable <- function (gfit, filename="Table_Cox_HR.csv", save = T,round=3) {
  # create hazard ratio table w/ Var name/OR/low95/hi95/pvalue from glm.
  # 
  # Args:
  #   gfit: Any variable.
  #   filename: Output csv file name
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  library (Hmisc)
  or.table <- NULL
  for (i in 1:length(names(gfit$coef))) {
    x <- cbind(or=exp(summary(gfit)$coef[i,1]),
               exp(confint.default(gfit)[i,1]), 
               exp(confint.default(gfit)[i,2]),
               p.value=summary(gfit)$coef[i,5])
    or.table <- rbind(or.table, x)
  }
  or.table <- round(or.table, digits=round)
  #or.table[or.table>100] <- 100
  or.table <- as.data.frame(cbind(names(gfit$coef), or.table))
  or.table$p.value <- as.numeric(as.character(or.table$p.value))
  or.table$p.value[or.table$p.value<0.001] <- "<0.001"
  colnames(or.table) <- c('Variables', 'Hazard Ratio', 'Lower 95% CI', 'Upper 95% CI', 'p-value')
  #or.table <- or.table[-1,]
  rownames(or.table) <- NULL
  write.table(or.table, sep=',', file=filename, row.names=FALSE)
  return (or.table)
}

ggkmTable <- function(sfit, risktable=TRUE,returns = T,
                      xlabs = "Time (unit)", ylabs = "Survival Probability",
                      ystratalabs = NULL, ystrataname = NULL, legend_xloc = 0.35,
                      xlim = -1, timeby = 12, 
                      main = "", 
                      pval = TRUE, pval_xloc=0.6, pval_yloc=0.1 , 
                      linetype.manual = c("solid","dashed","dotted","dotdash","twodash","longdash"), line.size = 0.4, line.color,
                      save = FALSE, file.name = "Figure.pdf", file.height = 6, file.width = 7, 
                      color = FALSE) {
  # Create ggplot-based KM table w/ numbers at risk at the bottom
  # 
  # Args:
  #   sfit: survival function
  #   risktable: Boolean whether to display risk table
  #   returns: Boolean whether to return ggplot image
  #   xlab: x-axis label
  #   ylab: y-axis label
  #   ystratalabs: label for the legends to the strata
  #   ystrataname: vector of labels for different strata
  #   xlim: limits for x-axis
  #   timeby: x-axis display increments in (months)
  #   main: figure title
  #   pval: Whether to display log-rank test p-value
  #
  # Returns:
  #   Table of the properly formatted table, and saves the table to a csv file into working directory
  library("ggplot2")
  library("plyr")
  library("gridExtra")
  library("gtable")
  library("grid")
  
  surv <- NULL
  n.risk <- NULL
  if(is.null(ystratalabs)) {
    ystratalabs <- as.character(levels(summary(sfit)$strata))
  }
  m <- max(nchar(ystratalabs))
  if(is.null(ystrataname)) ystrataname <- ""
  
  if (xlim == -1){
    times <- seq(0, max(sfit$time), by = timeby)
  } else {
    times <- seq(0, xlim, by = timeby)
  }
  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,
                    upper = sfit$upper, lower = sfit$lower)
  levels(.df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1, strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  if (color==FALSE)
  {
  p <- ggplot(.df, aes(time, surv, group = strata)) +
    
    # Plot Lines
    geom_step(aes(linetype = strata), size = line.size) +  # line size
    scale_linetype_manual(values=linetype.manual)+         # line type
    
    # Axis and Grid
    theme_bw() +
    
    theme(axis.title.x = element_text(vjust = 0.5,face="bold",size=14, family="Franklin Gothic Medium")) +
    theme(axis.text.x  = element_text(colour="black",size=10, family ="Franklin Gothic Book")) +
    theme(axis.title.y = element_text(face="bold",size=14, family="Franklin Gothic Medium")) +
    theme(axis.text.y  = element_text(colour="black",size=10, family = "Franklin Gothic Book")) +
    #theme(axis.line = element_line(colour = "black",size = 1)) +
    theme(axis.line.x = element_line(color="black")) +
    theme(axis.line.y = element_line(color="black")) +
    
    scale_x_continuous(xlabs, breaks = times, limits = c(0, max(times))) +  #max(sfit$time)
    scale_y_continuous(ylabs, limits = c(0, 1)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank()) +
    #theme(panel.border = element_rect(colour = "black")) +
    theme(panel.background = element_blank()) +
    
    # Legend
    theme(legend.justification = c(0,0), legend.background = element_rect(fill = "transparent",colour = NA), 
          legend.position = c(0,0), legend.background = element_blank()) +
    theme(legend.title = element_text(colour="black",face="bold",size=12, family = "Franklin Gothic Book"))+
    theme(legend.text = element_text(colour="black",face="bold",size=12, family = "Franklin Gothic Medium")) +
    labs(linetype = ystrataname, name=ystrataname, colour=ystrataname) +   
    
    # Margin ( m = max char length of strata)
    #theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 1.5, 5)), "lines")) +
    theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 2.5, 5)), "lines")) +
    
    # Title
    ggtitle(main) 
  }
  else {  # color == TRUE
    p <- ggplot(.df, aes(time, surv, group = strata)) +
    
      # Plot Lines
      geom_step(aes(color=strata), size = line.size) +  # line size and color
      scale_linetype_manual(values=linetype.manual)+         # line type
      
      # Axis and Grid
      theme(axis.title.x = element_text(vjust = 0.5,face="bold",size=14, family="Franklin Gothic Medium")) +
      theme(axis.text.x  = element_text(colour="black",size=10, family ="Franklin Gothic Book")) +
      theme(axis.title.y = element_text(face="bold",size=14, family="Franklin Gothic Medium")) +
      theme(axis.text.y  = element_text(colour="black",size=10, family = "Franklin Gothic Book")) +
      #theme(axis.line = element_line(colour = "black",size = 1)) +
      theme(axis.line.x = element_line(color="black")) +
      theme(axis.line.y = element_line(color="black")) +
      
      scale_x_continuous(xlabs, breaks = times, limits = c(0, max(times))) +  #max(sfit$time)
      scale_y_continuous(ylabs, limits = c(0, 1)) +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(panel.border = element_blank()) +
      #theme(panel.border = element_rect(colour = "black")) +
      theme(panel.background = element_blank()) +
      
      # Legend
      theme(legend.justification = c(0,0), legend.background = element_rect(fill = "transparent",colour = NA), 
            legend.position = c(0,0), legend.background = element_blank()) +
      theme(legend.title = element_text(colour="black",face="bold",size=12, family = "Franklin Gothic Book"))+
      theme(legend.text = element_text(colour="black",face="bold",size=12, family = "Franklin Gothic Medium")) +
      labs(linetype = ystrataname, name=ystrataname, colour=ystrataname) +   
      
      # Margin ( m = max char length of strata)
      theme(plot.margin = unit(c(0, 1, 1, ifelse(m < 10, 2.5, 5)), "lines")) +
      
      # Title
      ggtitle(main) 
  }

  if(pval) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n)-1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval < 0.001, "p < 0.001", paste("p =", round(pval, digits=3)))
    p <- p + 
      # P-value Text
      annotate("text", 
          x = max(times), y = 0, hjust = 1,       #right justified
          label = pvaltxt, family = "Franklin Gothic Medium")
  }
  
  ## Create a blank plot for place-holding
  ## .df <- data.frame()
  blank.pic <- ggplot(.df, aes(time, surv)) +
    geom_blank() +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank())
  
  if(risktable) {
    ## Create table graphic to include at-risk numbers
    risk.data <- data.frame(
      strata = factor(summary(sfit, times = times, extend = TRUE)$strata),
      time = summary(sfit, times = times, extend = TRUE)$time,
      n.risk = summary(sfit, times = times, extend = TRUE)$n.risk)
    
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
  
    data.table <- ggplot(risk.data, aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3, family = "Franklin Gothic Book", hjust=0.7) +
      
      theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = rev(ystratalabs)) +
      # scale_y_discrete(#format1ter = abbreviate,  # breaks = 1:3,    # labels = ystratalabs) +
      scale_x_continuous("Numbers At Risk", limits = c(0, max(times))) +

      theme(panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            
            axis.text.x = element_blank(), 
            axis.ticks = element_blank(),
      # Numbers at risk table themes      
            axis.title.x = element_text(size = 14, vjust = 1, family = "Franklin Gothic Medium"), 
            axis.text.y = element_text(size = 12, face = "bold", hjust = 1,family = "Franklin Gothic Medium"))
    
    data.table <- data.table + theme(legend.position = "none") +  xlab(NULL) + ylab(NULL)
    data.table <- data.table + theme(plot.margin = unit(c(1, 1, 0.1, ifelse(m < 10, 2.5, 5)), "lines"))
      #theme(plot.margin = unit(c(-2, 1, 0.1, ifelse(m < 10, 2.5, 7)-0.19 * m), "lines"))
    
    grid.newpage()
    #plot_grid(p, data.table, align = 'v', rel_widths = c(7, 1))
    
    #Extract Grobs
    g1 <- ggplotGrob(p)
    g2 <- ggplotGrob(data.table)
    #Bind the tables
    g <- gtable:::rbind_gtable(g1, g2, "first")
    #Remove a row between the plots
    g <- gtable_add_rows(g, unit(-1,"cm"), pos=nrow(g1))
    # relative heights
    panels <- g$layout$t[grep("panel", g$layout$name)]
    g$heights[panels[1]] <- unit(7, 'null')
    g$heights[panels[2]] <- unit(1, 'null')
    

    # clipping
    g$layout$clip[g$layout$name == "panel"] <- "off"
    #draw
    grid.newpage()
    grid.draw(g)
    
    if (save) {
      print("Saving Figure & Summary Table")
      if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
        dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
        embed_fonts(file.name)  # embed true type fonts
      }
      else if (last(unlist(strsplit(file.name, "[.]"))) %in% c("png","PNG")) {
        dev.copy(png,file ==file.name,width=file.width,height=file.height,units="in",res=300); dev.off()
      }
      sink(file = paste(file.name,".txt",sep=""))
      print(summary(sfit, time=times))
      sink()
    }
    if (returns) {
#       a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3, ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
      return(g)
    }
  } #end if risktable
  else {   # if no risktable
    # draw figure
    print(p)
    # save figure
    if (save == TRUE) {
      print("Saving Figure & Summary Table")
      if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
        dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
        embed_fonts(file.name)  # embed true type fonts
      }
      else if (last(unlist(strsplit(file.name, "[.]"))) %in% c("png","PNG")) {
        dev.copy(png,file.name,width=file.width,height=file.height,units="in",res=300)
        dev.off()
      }
      sink(file = paste(file.name,".txt",sep=""))
      print(summary(sfit, time=times ))
      sink()
    }
    # return figure
    if(returns) return(p)
  }
}


meandiffplot <- function (x) {
  mdiff<-data.frame(x$sum.matched)
  names(mdiff)[4]<-c("m_mean_diff")
  diffplot<-ggplot(mdiff, aes(m_mean_diff) )
  diffplot<- diffplot+ geom_histogram (fill="grey")
  diffplot<- diffplot+ geom_density (colour="red")
  diffplot 
}

meandifftable<- function (x){
  post<-data.frame(x$sum.matched[4])
  matchID <- as.vector (row.names (post) )
  names(post)[1]<-c("m_mean_diff")
  post$absolute<- abs(post[1])
  total2<-post[order (-post$absolute, na.last=NA) ,]
  meandiffover1<- subset(total2[1], total2[1]> .1 | total2[1]< -.1)
  meandiffover1
}

SaveUniqueValues <- function(x, var.list, ordered = T, show = F, filename = "Unique_Var.csv") {
  # Get and save unique values of a dataset
  # 
  # Args:
  #   DB: dataset
  #   VARIABLE: element of interest
  #   filename: output file
  #
  # Returns: n/a
  df <- unique(x[var.list])
  if (show == T ) print(df)
  write.csv(df, file=filename, row.names=FALSE)
}

termplot2 <- function (model, data = NULL, envir = environment(formula(model)), 
                       partial.resid = FALSE, rug = FALSE, terms = NULL, se = FALSE, 
                       xlabs = NULL, ylabs = NULL, main = NULL, col.term = 2, lwd.term = 1.5, 
                       col.se = "orange", lty.se = 2, lwd.se = 1, col.res = "gray", 
                       cex = 1, pch = par("pch"), col.smth = "darkred", lty.smth = 2, 
                       span.smth = 2/3, ask = dev.interactive() && nb.fig < n.tms, 
                       use.factor.levels = TRUE, smooth = NULL, ylim = "common",
                       rug.type = "rug", yscale="regular",
                       col.dens="#80808033", se.type="line", density.proportion = .1, log="",
                       ...) 
{
  # Options for new variables
  # rug.type: "rug", "density"
  # yscale: "regular", "exponential"
  # se.type: "line", "polygon"
  
  #   model  : fitted model object
  #   
  #   data  : data frame in which variables in model can be found
  #   
  #   envir	:environment in which variables in model can be found
  #   
  #   partial.resid	: logical; should partial residuals be plotted?
  #   
  #   rug	: add rugplots (jittered 1-d histograms) to the axes?
  #   
  #   terms	
  #   which terms to plot (default NULL means all terms); a vector passed to predict(.., type = "terms", terms = *).
  #   
  #   se	
  #   plot pointwise standard errors?
  #   
  #   xlabs	
  #   vector of labels for the x axes
  #   
  #   ylabs	
  #   vector of labels for the y axes
  #   
  #   main	
  #   logical, or vector of main titles; if TRUE, the model's call is taken as main title, NULL or FALSE mean no titles.
  #   
  #   col.term, lwd.term	
  #   color and line width for the ‘term curve’, see lines.
  #   
  #   col.se, lty.se, lwd.se	
  #   color, line type and line width for the ‘twice-standard-error curve’ when se = TRUE.
  #   
  #   col.res, cex, pch	
  #   color, plotting character expansion and type for partial residuals, when partial.resid = TRUE, see points.
  #   
  #   ask	
  #   logical; if TRUE, the user is asked before each plot, see par(ask=.).
  #   
  #   use.factor.levels	
  #   Should x-axis ticks use factor levels or numbers for factor terms?
  #   
  #   smooth	
  #   NULL or a function with the same arguments as panel.smooth to draw a smooth through the partial residuals for non-factor terms
  #   
  #   lty.smth, col.smth, span.smth	
  #   Passed to smooth
  #   
  #   ylim	
  #   an optional range for the y axis, or "common" when a range sufficient for all the plot will be computed, or "free" when limits are computed for each plot.
  #   
  #   plot	
  #   if set to FALSE plots are not produced: instead a list is returned containing the data that would have been plotted.
  #   
  #   transform.x	
  #   logical vector; if an element (recycled as necessary) is TRUE, partial residuals for the corresponding term are plotted against transformed values. The model response is then a straight line, allowing a ready comparison against the data or against the curve obtained from smooth-panel.smooth.
  #   
  #   ...	
  #   other graphical parameters.
  
  # Basic functions used by termplot
  se.lines <- function(x, iy, i, ff = 2) {
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    lines(x, upper_ci, lty = lty.se, lwd = lwd.se, 
          col = col.se)
    lines(x, lower_ci, lty = lty.se, lwd = lwd.se, 
          col = col.se)
  }
  # The iy variable contains the ordering of the y-variable
  # the x-variable is already ordered 
  se.polygon <- function(x, iy, i, ff = 2){
    tt <- ff * terms$se.fit[iy, i]
    upper_ci <- tms[iy, i] + tt
    lower_ci <- tms[iy, i] - tt
    
    if (identical(yscale, "exponential")){
      upper_ci <- exp(upper_ci)
      lower_ci <- exp(lower_ci)
    }
    
    current_i.backw <- order(x, decreasing = TRUE)
    current_i.forw <- order(x, decreasing = FALSE)
    
    # The x-axel is always the same
    x.poly <- c(x[current_i.forw] , x[current_i.backw])
    # The y axel is based upin the current model
    y.poly <- c(upper_ci[current_i.forw], lower_ci[current_i.backw])
    polygon(x.poly , y.poly , col = col.se, border = NA)
  }
  plot.density <- function(xx){
    # calculate the coordinates of the density function
    density <- density( xx )
    # the height of the densityity curve
    max.density <- max(density$y)
    
    # transform the y-coordinates of the density
    if (density.proportion >= 1){
      warning("Can't have a density proportion of 100 % of the plot, recommended is less than 0.2")
      density.proportion <- .1
    }
    
    # Get the boundaries of the plot to
    # put the density polygon at the x-line
    yscale <- par("usr")[3:4]
    # get the "length" and range of the y-axis
    yspan <- max(yscale) - min(yscale)
    
    density_percent <- density$y/max.density
    height <- density.proportion * density_percent * yspan  + min(yscale)
    if (par("ylog")){
      # For some odd reason the default log scale is 10-based
      # when the y-scale is logarithmic
      height <- 10^(height)
    }
    
    ## plot the polygon
    polygon( density$x , height, border = F, col = col.dens)
  }
  plot.rug <- function(xx){
    n <- length(xx)
    lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1L] + 
                                                        c(0, 0.05, NA) * diff(ylims), n))
    if (partial.resid) 
      lines(rep.int(xlims[1L] + c(0, 0.05, NA) * diff(xlims), 
                    n), rep.int(pres[, i], rep.int(3, n)))
  }
  
  plot.factor <- function(i, ff, xx, 
                          xlab, ylab, main){
    
    if (!is.null(model$na.action)) 
      ff <- naresid(model$na.action, ff)
    ll <- levels(ff)
    xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
    
    if (rug) {
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
      xlims[2L] <- xlims[2L] + 0.03 * diff(xlims)
    }
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(1, 0, type = "n", xlab = xlab, 
         ylab = ylab, log=log,
         xlim = xlims, ylim = tmp_ylims, 
         main = main, xaxt = "n", 
         ...)
    
    if (use.factor.levels) 
      axis(1, at = seq_along(ll), labels = ll, ...)
    else axis(1)
    for (j in seq_along(ll)) {
      ww <- which(ff == ll[j])[c(1, 1)]
      jf <- j + c(-0.4, 0.4)
      
      # Plot confidence interval
      if (se){
        if (se.type == "polygon"){
          se.polygon(jf, iy = ww, i = i)
        }else{
          se.lines(jf, iy = ww, i = i)
        }
      }
      
      yvalues <- tms[ww, i]
      if (identical(yscale, "exponential")){
        yvalues <- exp(yvalues)
      }
      lines(jf, yvalues, col = col.term, lwd = lwd.term,
            ...)
    }
  }
  
  plot.continuous <- function(i,
                              xx, 
                              xlab, ylab, main){
    
    
    if (!is.null(use.rows)) 
      xx <- xx[use.rows]
    xlims <- range(xx, na.rm = TRUE)
    if (rug && rug.type != "density") 
      xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
    oo <- order(xx)
    
    yvalues <- tms[, i]
    tmp_ylims <- ylims
    if (identical(yscale, "exponential")){
      yvalues <- exp(yvalues)
      tmp_ylims <- exp(tmp_ylims)
    }
    plot(range(xx), range(yvalues), type = "n", xlab = xlab,  log=log,
         ylab = ylab, xlim = xlims, ylim = tmp_ylims, 
         main = main[i], 
         ...)
    
    # Plot confidence interval
    if (se){
      if (se.type == "polygon"){
        se.polygon(xx[oo], iy = oo, i = i)
      }else{
        se.lines(xx[oo], iy = oo, i = i)
      }
      
    }
    
    lines(xx[oo], yvalues[oo], 
          col = col.term, lwd = lwd.term)
  }
  
  # Get the the terms that are of interest
  which.terms <- terms
  terms <- if (is.null(terms)) 
    predict(model, type = "terms", se.fit = se)
  else predict(model, type = "terms", se.fit = se, terms = terms)
  
  # Get the data used for the rug
  mf <- model.frame(model)
  if (is.null(data)) 
    data <- eval(model$call$data, envir)
  if (is.null(data)) 
    data <- mf
  
  tms <- as.matrix(if (se) 
    terms$fit
    else terms)
  
  use.rows <- if (NROW(tms) < NROW(data)) 
    match(rownames(tms), rownames(data))
  
  nmt <- colnames(tms)
  # Remove interaction terms
  if (any(grep(":", nmt) > 0)){
    for(i in grep(":", nmt)){
      warning(sprintf("The interaction term '%s' won't be displayed", nmt[i]))
      nmt <- nmt[-i]
    }
  }
  
  n.tms <- length(nmt)
  
  cn <- parse(text = nmt)
  if (!is.null(smooth)) 
    smooth <- match.fun(smooth)
  if (is.null(ylabs)) 
    ylabs <- paste("Partial for", nmt)
  if (is.null(main)) 
    main <- ""
  else if (is.logical(main)) 
    main <- if (main) 
      deparse(model$call, 500)
  else ""
  else if (!is.character(main)) 
    stop("'main' must be TRUE, FALSE, NULL or character (vector).")
  main <- rep(main, length.out = n.tms)
  pf <- envir
  carrier <- function(term) {
    if (length(term) > 1L) 
      carrier(term[[2L]])
    else eval(term, data, enclos = pf)
  }
  carrier.name <- function(term) {
    if (length(term) > 1L) 
      carrier.name(term[[2L]])
    else as.character(term)
  }
  if (is.null(xlabs)) 
    xlabs <- unlist(lapply(cn, carrier.name))
  if (partial.resid || !is.null(smooth)) {
    pres <- residuals(model, "partial")
    if (!is.null(which.terms)) 
      pres <- pres[, which.terms, drop = FALSE]
  }
  is.fac <- sapply(nmt, function(i){
    if (i %in% colnames(mf)){
      return(is.factor(mf[, i]))
    }
    cn <- grep(sprintf("(^%s$|^[a-zA-Z0-9]+[(][ ]*%s[, 0-9a-zA-Z\"\']+[)])", i, i), colnames(mf))
    if (length(cn)==0){
      stop(sprintf("Could not find '%s' in dataset", i))
    }else if (length(cn) > 1){
      cn <- cn[1]
      warning("More than one name match found")
    }
    return(is.factor(mf[, cn]))
  })
  
  nb.fig <- prod(par("mfcol"))
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  ylims <- ylim
  if (identical(ylims, "common")) {
    ylims <- if (!se) 
      range(tms, na.rm = TRUE)
    else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 * 
                 2 * terms$se.fit, na.rm = TRUE)
    if (partial.resid) 
      ylims <- range(ylims, pres, na.rm = TRUE)
    if (rug) 
      ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
  }
  
  for (i in 1L:n.tms) {
    if (identical(ylim, "free")) {
      ylims <- range(tms[, i], na.rm = TRUE)
      if (se) 
        ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[, 
                                                                 i], tms[, i] - 1.05 * 2 * terms$se.fit[, i], 
                       na.rm = TRUE)
      if (partial.resid) 
        ylims <- range(ylims, pres[, i], na.rm = TRUE)
      if (rug) 
        ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    }
    
    if (is.fac[i]) {
      ff <- mf[, nmt[i]]
      xx <- as.numeric(mf[, nmt[i]])
      plot.factor(i, 
                  ff=ff,
                  xx=xx,
                  xlab=xlabs[i], ylab=ylabs[i],
                  main=main[i])
    }
    else {
      xx <- carrier(cn[[i]])
      plot.continuous(i, xx=xx, 
                      xlab=xlabs[i], 
                      ylab=ylabs[i],
                      main=main[i])
      
    }
    if (partial.resid) {
      if (!is.fac[i] && !is.null(smooth)) {
        smooth(xx, pres[, i], lty = lty.smth, cex = cex, 
               pch = pch, col = col.res, col.smooth = col.smth, 
               span = span.smth)
      }
      else points(xx, pres[, i], cex = cex, pch = pch, 
                  col = col.res)
    }
    
    if (rug){
      if (rug.type == "density") {
        plot.density(xx)
      }else {
        plot.rug(xx)
      }
      
    }
  }
  invisible(n.tms)
}

PropensityMatch <- function(DB, variables.list, formula.match, grouping.variable = "GROUP", merge.variable = "CASEID",
                            control.group = "", treat.group = "", ratio = 1 , save.stdiff = T, save.stdiff.title = "StDiff.csv",
                            formula.summary=NULL, save.summary = T, save.summary.title = "PS.csv") {
  library("MatchIt") 
  set.seed(100)
  
  # note: <<- is needed because for some reason, match.data requires a global variable
  DB.pre <<- na.omit(DB[,variables.list])

  DB.pre$treatment <<- NA
  DB.pre$treatment[eval(parse(text=paste("DB.pre$",grouping.variable,"==","\"",control.group,"\"",sep = "" )))] <<- 0
  DB.pre$treatment[eval(parse(text=paste("DB.pre$",grouping.variable,"==","\"",treat.group  ,"\"",sep = "" )))] <<- 1
  
  m.out <- matchit(formula.match, method="nearest", data=DB.pre, ratio = ratio)
  m.final <- match.data(m.out)

  if (save.stdiff == T) {
     m.s <- summary(m.out, standardize=T)$sum.match
     write.table(m.s, file=save.stdiff.title, sep = ",")
  }


  DB.postmatch <- subset(DB, eval(parse(text = merge.variable)) %in% eval(parse(text = paste("m.final$",merge.variable,sep=""))))
  
  if (save.summary == T) {
    CreateSummaryTable(formula.summary, DB.postmatch, title = save.summary.title , save = save.summary)  
  }
  return(DB.postmatch)
}

# Custom user-defined theme for ggplot2
theme_jeff_nyt <- function(base_size = 12, base_family = "Helvetica", font.regular ="Franklin Gothic Book", font.bold="Franklin Gothic Medium") {
  
  library(extrafont)    # for fonts
  
  theme(
    line =               element_line(colour = "black", size = 0.5, linetype = 1,lineend = "butt"),
    rect =               element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text =               element_text(family = font.regular, face="plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin=margin(), debug = F),
    #axis.text =          element_text(size = rel(0.8), colour = "white"),
    #strip.text =         element_text(size = rel(0.8), colour = "white"),
    
    axis.line =          element_line(colour = "black"),
    axis.text.x  =       element_text(colour="black", size=10, family=font.regular),
    axis.text.y  =       element_text(colour="black", size=10, family=font.regular),
    #axis.ticks =         element_line(colour = "white", size = 0.2),
    #axis.title =         element_text(colour = "white"),
    axis.title.x =       element_text(vjust = 0.5,face="bold",size=14, family=font.bold),
    axis.title.y =       element_text(angle=90, face="bold",size=14, family=font.bold),
    #axis.ticks.length =  unit(0.3, "lines"),
    #axis.ticks.margin =  unit(0.5, "lines"),
    
    legend.background =  element_rect(fill = "transparent",colour = NA),
    #legend.margin =      unit(0.2, "cm"),
    #legend.key =         element_rect(fill = "black", colour = "white"),
    #legend.key.size =    unit(1.2, "lines"),
    #legend.key.height =  NULL,
    #legend.key.width =   NULL,
    legend.text =        element_text(colour="black",face="bold",size=12, family = font.bold),
    #legend.text.align =  NULL,
    legend.title =       element_text(colour="black",face="bold",size=12, family = font.regular),
    #legend.title.align = NULL,
    legend.position =    c(0,0),
    #legend.direction =   "vertical",
    legend.justification = c(0,0),
    #legend.box =         NULL,
    
    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    #panel.margin =       unit(0.25, "lines"),
    
    #strip.background =   element_rect(fill = "grey30", colour = "grey10"),
    #strip.text.x =       element_text(),
    #strip.text.y =       element_text(angle = -90),
    
    #plot.background =    element_rect(colour = "black", fill = "black"),
    plot.title =         element_text(colour="black",face="bold",size=rel(1.2), family = font.bold),
    #plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
    
    complete = TRUE
  )
}

ggForestPlot <- function(d.raw, xlab="Odds Ratio", ylab="Factors", 
                         labels=NULL, line.size, x.tick=c(0,0.5,1,1.5,2),  x.max=2.5,
                         remove=NULL, center=1,
                         save = FALSE, file.name = "Figure.pdf", file.height = 4, file.width = 8){
  
  require(ggplot2)
  require(stringr)
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  
  # error checking
  if( !is.null(labels) & (length(labels) != nrow(d.raw)))  break("Error: Length of labels must equal length of data frame!")
  if( is.null(labels)) labels <- d.raw[1]
  
  xlab <- str_c("\n",xlab,"\n")
  ylab <- str_c("\n",ylab,"\n")

  d <- data.frame( x= labels,
                   y1= d.raw[2],          
                   ylo1 = d.raw[3],       
                   yhi1 = d.raw[4], 
                   pval = d.raw[5],
                   order = rev(letters[1:nrow(d.raw)])
                   )
  colnames(d) <- c("x","y1","ylo1","yhi1", "pval","order")

  d$y <- as.numeric(paste(d$y1)) 
  d$ylo <- as.numeric(paste(d$ylo1)) 
  d$yhi <- as.numeric(paste(d$yhi1)) 
  
  if (!is.null(remove)) d<-d[-remove,]        # remove whichever row that I don't want

  # print(d)    # debugging purposes

    p <- ggplot(data=d,aes(x=y,y=order))+
         geom_point()+   # draw center point
         geom_errorbarh(aes(xmin=ylo,xmax=yhi),height=0.667)+   # draw error bar
         geom_vline(xintercept=center,linetype="dashed") +   # center line
         ylab(ylab) +
         xlab(xlab) +
         scale_x_continuous(breaks=x.tick, limits=c(0,x.max)) +         # tick markers at breaks
         scale_y_discrete(label=rev(d$x))        # labelling      

  p <- p + theme_jeff_nyt() + 
           theme(axis.ticks.y = element_blank(), axis.text.y = element_text(size = 12, face="bold", hjust = 1, vjust=0.5), legend.position = "none") +
           theme(axis.text.x=element_text(size=12)) +
           theme(axis.line.x = element_line(color="black")) +
           theme(axis.line.y = element_line(color="black"))

  if (save == TRUE) {
    print(paste("Saving Figure & Summary Table in", getwd()))
    if (last(unlist(strsplit(file.name, "[.]"))) %in% c("pdf","PDF")) {
      #dev.copy(pdf,file = file.name, width = file.width, height = file.height); dev.off()
      #embed_fonts(file.name)  # embed true type fonts
       ggsave(filename = file.name,width = file.width,height = file.height)
      }
    d <- d[,-c(6:9)]  # remove extra columns
    colnames(d) <- c("Variables","Odds Ratio","Lower 95% CI","Upper 95% CI","p-value")
    write.csv(d,file = paste0(file.name,".csv"),row.names = F)
  }
  return(p)
}

### unused ####
