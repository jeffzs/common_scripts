# COPYRIGHT: 2014-present
# AUTHOR: Zhifei Sun, zhifei.sun@duke.edu
# PURPOSE: Repository for custom functions used in every project


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
  
  colnames(NCDB_decoded) <- toupper(colnames(NCDB_decoded))
  
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
