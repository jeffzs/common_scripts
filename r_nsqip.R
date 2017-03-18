# NSQIP Data Manipulation


#decode peds nsqip
Peds_NSQIP_Decode <- function(db) {
  
  
  db$AGE_DAYS_L <- db$AGE_DAYS
  label(db$AGE_DAYS_L) <- "Age (days)"
  
  db$AGE_YEAR <- floor(db$AGE_DAYS_L/365)
  label(db$AGE_YEAR) <- "Age (year)"
  
  db$SEX_L <- NA
  db$SEX_L[db$SEX %in% c("Male")] <- "1@Male"
  db$SEX_L[db$SEX %in% c("Female")] <- "2@Female"
  db$SEX_L <- factor(db$SEX_L)
  label(db$SEX_L) <- "Sex"
  
  db$RACE_L <- NA
  db$RACE_L[db$RACE %in% c("White")] <- "1@White"
  db$RACE_L[db$RACE %in% c("Black or African American")] <- "2@Black"
  db$RACE_L[db$RACE %in% c("American Indian or Alaska Native","Asian","Native Hawaiian or Other Pacific Islander","Unknown/Not Reported")] <- "3@Other"
  db$RACE_L <- factor(db$RACE_L)
  label(db$RACE_L) <- "Race"
  
  db$WEIGHT_lb <- db$WEIGHT
  db$WEIGHT_lb[db$WEIGHT == -99] <- NA
  db$WEIGHT_kg <- db$WEIGHT_lb * 0.453592
  label(db$WEIGHT_kg) <- "Weight At Surgery (kg)"
  
  db$PREM_BIRTH_L <- NA
  db$PREM_BIRTH_L <- "2@Yes"
  db$PREM_BIRTH_L[db$PREM_BIRTH %in% c("No", "Unknown")] <- "1@No"
  db$PREM_BIRTH_L <- factor(db$PREM_BIRTH_L)
  label(db$PREM_BIRTH_L) <- "Premature Birth"
  
  db$ASACLAS_L <- NA
  db$ASACLAS_L[db$ASACLAS %in% c("ASA 1 - No Disturb","ASA 2 - Mild Disturb")] <- "1@<3"
  db$ASACLAS_L[db$ASACLAS %in% c("ASA 3 - Severe Disturb","ASA 4 - Life Threat","ASA 5 - Moribund")] <- "2@>=3"
  db$ASACLAS_L <- factor(db$ASACLAS_L)
  label(db$ASACLAS_L) <- "ASA Classification"
  
  label(db$WTLOSS) <- "Recent Weight Loss"
  label(db$VENTILAT) <- "Currently on Ventilator"
  label(db$CPNEUMON) <- "Preoperative Pneumonia"
  label(db$ASTHMA) <- "History of Asthma"
  label(db$CYSTIC_FIB) <- "History of Cystic Fibrosis"
  label(db$HXCLD) <- "Bronchopulmonary Dysplasia"
  label(db$OXYGEN_SUP) <- "Oxygen Support At Surgery"
  label(db$TRACHEOSTOMY) <- "Tracheostomy"
  label(db$STRUCT_PULM_AB) <- "Structural Pulmonary Abnormalities"
  label(db$LBP_DISEASE) <- "Liver/Biliary/Pancreatic Disease"
  
  db$CRF_L <- NA
  db$CRF_L[db$CRF %in% c("No cardiac risk factors")] <- "1@None"
  db$CRF_L[db$CRF %in% c("Minor cardiac risk factors")] <- "2@Minor"
  db$CRF_L[db$CRF %in% c("Major cardiac risk factors","Severe cardiac risk factors")] <- "3@Major or Severe"
  db$CRF_L <- factor(db$CRF_L)
  label(db$CRF_L) <- "Cardiac Risk Factors"
  
  label(db$PRVPCS) <- "Previous Cardiac Surgery"
  label(db$RENAFAIL) <- "Preoperative Acute Renal Failure"
  label(db$DIALYSIS) <- "Currently on Dialysis"
  label(db$COMA) <- "Coma >24 hours"
  label(db$IMPCOGSTAT) <- "Developmental Delay"
  label(db$SEIZURE) <- "Seizure Disorder"
  label(db$CEREBRAL_PALSY) <- "Cerebral Palsy"
  label(db$NEUROMUSCDIS) <- "Neuromuscular Disorder"
  label(db$IMMUNE_DIS) <- "Immune Disease/Immunosuppression"
  label(db$STEROID) <- "Steroid Use"
  label(db$NUTR_SUPPORT) <- "Needing Intravenous or Enteral Nutritional Support"
  label(db$BLEEDDIS) <- "Bleeding Disorder"
  label(db$HEMODISORDER) <- "Hematologic Disorder"
  label(db$BONE_MARROW_TRANS) <- "Bone Marrow Transplant"
  label(db$ORGAN_TRANS) <- "Solid Organ Transplant"
  
  label(db$MALIGNANCY) <- "Cancer Status"
  
  db$CONG_MALFORM_L <- NA
  db$CONG_MALFORM_L[db$CONG_MALFORM %in% c("No")] <- "1@No"
  db$CONG_MALFORM_L[db$CONG_MALFORM %in% c("Yes, Neonate < 1500 grams at the time of surgery", "Yes, Neonate >=1500 grams at the time of surgery or infant/child/teenager with a history of a congenital defect at the time of surgery")] <- "2@Yes"
  db$CONG_MALFORM_L <- factor(db$CONG_MALFORM_L)
  label(db$CONG_MALFORM_L) <- "Congenital Malformations"
  
  db$PRSODM_L <- db$PRSODM
  db$PRSODM_L[db$PRSODM == -99] <- NA
  label(db$PRSODM_L) <- "Preoperative Sodium"
  
  db$PRCREAT_L <- db$PRCREAT
  db$PRCREAT_L[db$PRCREAT == -99] <- NA
  label(db$PRCREAT_L) <- "Preoperative Creatinine"
  
  db$PRBUN_L <- db$PRBUN
  db$PRBUN_L[db$PRBUN == -99] <- NA
  label(db$PRBUN_L) <- "Preoperative Blood Urea Nitrogen"
  
  db$PRWBC_L <- db$PRWBC
  db$PRWBC_L[db$PRWBC == -99] <- NA
  label(db$PRWBC_L) <- "Preoperative WBC Count"
  
  db$PRHCT_L <- db$PRHCT
  db$PRHCT_L[db$PRHCT == -99] <- NA
  label(db$PRHCT_L) <- "Preoperative Hematocrit"
  
  db$PRALBUM_L <- db$PRALBUM
  db$PRALBUM_L[db$PRALBUM == -99] <- NA
  label(db$PRALBUM_L) <- "Preoperative Albumin"
  
  db$TOTHLOS_L <- db$TOTHLOS
  db$TOTHLOS_L[db$TOTHLOS == -99] <- NA
  label(db$TOTHLOS_L) <- "Total Hospital Length of Stay (days)"
  
  db$DOPTODIS_L <- db$DOPTODIS
  db$DOPTODIS_L[db$DOPTODIS == -99] <- NA
  label(db$DOPTODIS_L) <- "Time to Discharge After Surgery (days)"
  
  db$DOPERTOD_L2 <- db$DOPTODIS_L
  db$DOPERTOD_L2[db$DOPTODIS_L == 0] <- 1   # this is so that log lm models work, change '0' LOS to 1s
  
  db$OUT_ANYCOMPLICATION <- NA
  db$OUT_ANYCOMPLICATION <- "1@No"
  db$OUT_ANYCOMPLICATION [ db$SUPINFEC == "Superficial Incisional SSI"] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$WNDINFD == "Deep Incisional SSI"] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$ORGSPCSSI == "Organ/Space SSI"] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$DEHIS == "Wound Disruption"] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OUPNEUMO == "Pneumonia"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$REINTUB  == "Unplanned Intubation"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$PULEMBOL == "Pulmonary Embolism"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$RENAINSF == "Progressive Renal Insufficiency"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OPRENAFL == "Acute Renal Failure" ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$URNINFEC == "Urinary Tract Infection"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CNSCOMA == "Coma greater than 24 hours"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CNSCVA == "Stroke/CVA with neurological deficit"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CSZRE == "Seizure"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$NEURODEF == "Nerve injury"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CIVHG1 == "IVH Grade 1"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CIVHG2 == "IVH Grade 2"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CIVHG3 == "IVH Grade 3"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CIVHG4 == "IVH Grade 4"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [  db$CIVHGUNK == "Unknown/Specific Grade"   ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$CDARREST == "Cardiac Arrest Requiring CPR"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHBLEED == "Bleeding/Transfusions"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHGRAFL   == "Graft/Prosthesis/FF" ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHSESHOCK == "Septic Shock"  ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHVT   == "VT Requiring Therapy" ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHSYSEP == "Systemic Sepsis" ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION [ db$OTHCLAB == "CL Associated Bloodstream Infection" ] <- "2@Yes"
  db$OUT_ANYCOMPLICATION <- factor(db$OUT_ANYCOMPLICATION)
  label(db$OUT_ANYCOMPLICATION) <- "Overall Complications"
  
  db$POSTOP_WOUND <- NA
  db$POSTOP_WOUND <- "1@No"
  db$POSTOP_WOUND [ db$SUPINFEC == "Superficial Incisional SSI"] <- "2@Yes"
  db$POSTOP_WOUND [ db$WNDINFD == "Deep Incisional SSI"] <- "2@Yes"
  db$POSTOP_WOUND [ db$ORGSPCSSI == "Organ/Space SSI"] <- "2@Yes"
  db$POSTOP_WOUND [ db$DEHIS == "Wound Disruption"] <- "2@Yes"
  db$POSTOP_WOUND <- factor(db$POSTOP_WOUND)
  label(db$POSTOP_WOUND) <- "Postop Wound Complications"
  
  label(db$OPTIME) <- "Operative Time (minutes)"
  label(db$SUPINFEC) <- "Superficial SSI"
  label(db$WNDINFD) <- "Deep Incisional SSI"
  label(db$ORGSPCSSI) <- "Organ/Space SSI"
  label(db$DEHIS) <- "Wound Disruption"
  label(db$OUPNEUMO) <- "Pneumonia"
  label(db$REINTUB) <- "Unplanned Reintubation"
  label(db$PULEMBOL) <- "Pulmonary Embolism"
  label(db$OTHVT) <- "DVT"
  label(db$RENAINSF) <- "Renal Insufficiency"
  label(db$OPRENAFL) <- "Renal Failure"
  label(db$URNINFEC) <- "UTI"
  label(db$CDARREST) <- "Cardiac Arrest"
  label(db$OTHBLEED) <- "Bleeding"
  label(db$OTHSYSEP) <- "Systemic Sepsis"
  label(db$OTHSESHOCK) <- "Septic Shock"
  label(db$OTHCLAB) <- "Catheter-Related Infection"
  label(db$DEATH30YN) <- "30-Day Mortality"
  label(db$NUTRITION_AT_DISCHARGE) <- "TPN At Discharge"
  label(db$OXYGEN_AT_DISCHARGE) <- "Oxygen At Dischrage"
  label(db$READMISSION1) <- "30-Day Readmission"
  
  db$ALL <- "Entire Cohort"
  
  return(db)
}


# functions
NSQIPDecodeData <- function(DB, Colectomy.Targeted = F) {
  
  #break("Need to fix overall complication decoding")
  
  names(DB) <- toupper(names(DB))
  
  # RECOMMEND USE THIS FUNCTION AFTER SUBSET TO INCREASE SPEED #
  
  DB$AGE_L <- NA
  DB$AGE_L <- as.numeric(DB$AGE)
  DB$AGE_L [DB$AGE == "90+"] <- 90
  label(DB$AGE_L) <- "Age (years)"
  
  DB$AGE_DECADE_L <- round(DB$AGE_L/10, digits = 0)
  label(DB$AGE_DECADE_L) <- "Age (decade)"
  
  DB$AGE_CAT_L <- NA
  DB$AGE_CAT_L[DB$AGE < 65] <- "<65"
  DB$AGE_CAT_L[DB$AGE >= 65 & DB$AGE < 75] <- "65-74"
  DB$AGE_CAT_L[DB$AGE >= 75 & DB$AGE < 85] <- "75-84"
  DB$AGE_CAT_L[DB$AGE >= 85] <- "85+"
  DB$AGE_CAT_L <- as.factor(DB$AGE_CAT_L)
  
  DB$SEX_L <- NA
  DB$SEX_L [DB$SEX %in% c("male") ] <- "1@Male"
  DB$SEX_L [DB$SEX %in% c("female") ] <- "2@Female"
  DB$SEX_L <- factor (DB$SEX_L)
  label(DB$SEX_L) <- "Sex"
  
  DB$RACE_L <- NA
  DB$RACE_L [DB$RACE_NEW %in% c("White") ] <- "1@White"
  DB$RACE_L [DB$RACE_NEW %in% c("Black or African American") ] <- "2@Black"
  DB$RACE_L [DB$RACE_NEW %in% c("American Indian or Alaska Native","Asian","Native Hawaiian or Pacific Islander") ] <- "3@Other"
  DB$RACE_L <- factor (DB$RACE_L)
  label(DB$RACE_L) <- "Race"
  
  DB$BMI <- 703 * DB$WEIGHT / (DB$HEIGHT)^2
  label(DB$BMI) <- "BMI (kg/m^2)"
  
  DB$ASA_L <- NA
  DB$ASA_L [DB$ASACLAS == "None assigned"] <- NA
  DB$ASA_L [DB$ASACLAS == "1-No Disturb" ] <- "1@1"
  DB$ASA_L [DB$ASACLAS == "2-Mild Disturb"] <- "2@2"
  DB$ASA_L [DB$ASACLAS == "3-Severe Disturb"] <- "3@3"
  DB$ASA_L [DB$ASACLAS == "4-Life Threat" | DB$ASACLAS == "5-Moribund"] <- "4@4+"
  DB$ASA_L <- factor(DB$ASA_L)
  label(DB$ASA_L) <- "ASA Classification"
  
  DB$YEAR <- DB$OPERYR
  DB$YEAR <- factor(DB$YEAR)
  label(DB$YEAR) <- "Operative Year"
  
  DB$OPTIME_L <- DB$OPTIME
  DB$OPTIME_L [ DB$OPTIME == -99] <-NA
  label(DB$OPTIME_L) <- "Operative Time (minutes)"
  
  
  DB$PREOP_SEPSIS <- NA
  DB$PREOP_SEPSIS [DB$PRSEPIS == "None" ] <- "1@None"
  DB$PREOP_SEPSIS [DB$PRSEPIS == "SIRS" ] <- "2@SIRS"
  DB$PREOP_SEPSIS [DB$PRSEPIS == "Sepsis" ] <- "3@Sepsis"
  DB$PREOP_SEPSIS [DB$PRSEPIS == "Septic Shock" ] <- "4@Septic Shock"
  DB$PREOP_SEPSIS <- factor(DB$PREOP_SEPSIS)
  label(DB$PREOP_SEPSIS) <- "Preop Sepsis"
  
  DB$PREOP_EMERGENCY <- NA
  DB$PREOP_EMERGENCY [DB$EMERGNCY == "No" ] <- "1@No"
  DB$PREOP_EMERGENCY [DB$EMERGNCY == "Yes" ] <- "2@Yes"
  DB$PREOP_EMERGENCY <- factor(DB$PREOP_EMERGENCY)
  label(DB$PREOP_EMERGENCY) <- "Emergency Case"
  
  DB$PREOP_WBC <- DB$PRWBC
  DB$PREOP_WBC [DB$PRWBC == -99] <- NA
  label(DB$PREOP_WBC) <- "Preoperative WBC (x10^9 cells/L)"
  
  DB$PREOP_CR <- DB$PRCREAT
  DB$PREOP_CR [DB$PRCREAT == -99] <- NA
  label(DB$PREOP_CR) <- "Preoperative Creatine (mg/dL)"
  
  DB$PREOP_ALB <- DB$PRALBUM
  DB$PREOP_ALB [DB$PRALBUM == -99] <- NA
  label(DB$PREOP_ALB) <- "Preoperative Albumin (mg/dL)"
  
  DB$OUT_UTI <- NA
  DB$OUT_UTI [DB$URNINFEC == "No Complication"] <- "No"
  DB$OUT_UTI [DB$URNINFEC == "Urinary Tract Infection"] <- "Yes"
  DB$OUT_UTI <- factor(DB$OUT_UTI)
  label(DB$OUT_UTI) <- "Urinary Tract Infection"
  
  DB$OUT_BLEED <- NA
  DB$OUT_BLEED [DB$OTHBLEED == "No Complication"] <- "1@No"
  DB$OUT_BLEED [DB$OTHBLEED == "Transfusions/Intraop/Postop"] <- "2@Yes"
  DB$OUT_BLEED <- factor(DB$OUT_BLEED)
  label(DB$OUT_BLEED) <- "Intraop/Postop Transfusions"
  
  DB$OUT_SEPSIS <- NA
  DB$OUT_SEPSIS <- "1@No"
  DB$OUT_SEPSIS [DB$OTHSYSEP == "Sepsis"] <- "2@Sepsis"
  DB$OUT_SEPSIS [DB$OTHSESHOCK == "Septic Shock"] <- "3@Septic Shock"
  DB$OUT_SEPSIS <- factor(DB$OUT_SEPSIS)
  label(DB$OUT_SEPSIS) <- "Septic Complications"
  
  DB$OUT_SEPSIS_SIMPLE <- NA
  DB$OUT_SEPSIS_SIMPLE <- "1@No"
  DB$OUT_SEPSIS_SIMPLE [DB$OUT_SEPSIS != "1@No"] <- "2@Yes"
  DB$OUT_SEPSIS_SIMPLE <- factor(DB$OUT_SEPSIS_SIMPLE)
  label(DB$OUT_SEPSIS_SIMPLE) <- "Septic Complications"
  
  
  DB$RETURNOR <- factor(DB$RETURNOR)
  label(DB$RETURNOR) <- "Reoperation"
  
  
  
  DB$DOPTODIS_L <- NA
  DB$DOPTODIS_L <- DB$DOPTODIS
  DB$DOPTODIS_L [ DB$DOPTODIS %in% c(-99,0)] <- NA
  label(DB$DOPTODIS_L) <- "Length of Stay (days)"
  
  DB$OUT_READMISSION <- NA
  DB$OUT_READMISSION <- "1@No"
  DB$OUT_READMISSION[DB$UNPLANNEDREADMISSION1 == "Yes"|DB$UNPLANREADMISSION == "Yes"] <- "2@Yes"
  DB$OUT_READMISSION <- factor(DB$OUT_READMISSION)
  label(DB$OUT_READMISSION) <- "Readmission"
  
  
  DB$DOPERTOD_L <- NA
  DB$DOPERTOD_L <- "2@Yes"
  DB$DOPERTOD_L [ DB$DOPERTOD == -99] <- "1@No"
  DB$DOPERTOD_L <- factor (DB$DOPERTOD_L)
  label(DB$DOPERTOD_L) <- "30-Day Mortality"
  
  DB$OUT_WOUND <- NA
  DB$OUT_WOUND <- "1@No"
  DB$OUT_WOUND [DB$SUPINFEC == "Superficial Incisional SSI" | DB$WNDINFD == "Deep Incisional SSI" | DB$ORGSPCSSI == "Organ/Space SSI" | DB$DEHIS == "Wound Disruption"] <- "2@Yes"
  DB$OUT_WOUND <- factor(DB$OUT_WOUND)
  label(DB$OUT_WOUND) <- "Wound Complications"
  
  DB$OUT_WOUND_CAT <- NA
  DB$OUT_WOUND_CAT <- "1@None"
  DB$OUT_WOUND_CAT [DB$SUPINFEC == "Superficial Incisional SSI"] <- "2@Superficial SSI"
  DB$OUT_WOUND_CAT [DB$WNDINFD == "Deep Incisional SSI"] <- "3@Deep Incisional SSI"
  DB$OUT_WOUND_CAT [DB$ORGSPCSSI == "Organ/Space SSI"] <- "4@Organ/Space SSI"
  DB$OUT_WOUND_CAT [DB$DEHIS == "Wound Disruption"] <- "5@Wound Dehiscence"
  DB$OUT_WOUND_CAT <- factor(DB$OUT_WOUND_CAT)
  label(DB$OUT_WOUND_CAT) <- "Wound Complications"
  
  DB$OUT_CARDIAC_CAT <- NA
  DB$OUT_CARDIAC_CAT <- "1@None"
  DB$OUT_CARDIAC_CAT [DB$CDARREST == "Cardiac Arrest Requiring CPR"] <- "2@Cardiac Arrest"
  DB$OUT_CARDIAC_CAT [DB$CDMI == "Myocardial Infarction"] <- "3@Myocardial Infarction"
  DB$OUT_CARDIAC_CAT <- factor(DB$OUT_CARDIAC_CAT)
  label(DB$OUT_CARDIAC_CAT) <- "Cardiac Complications"
  
  DB$OUT_VTE_CAT <- NA
  DB$OUT_VTE_CAT <- "1@None"
  DB$OUT_VTE_CAT [DB$OTHDVT == "DVT Requiring Therapy"] <- "2@DVT"
  DB$OUT_VTE_CAT [DB$PULEMBOL == "Pulmonary Embolism"] <- "3@PE"
  DB$OUT_VTE_CAT <- factor(DB$OUT_VTE_CAT)
  label(DB$OUT_VTE_CAT) <- "VTE Complications"
  
  DB$OUT_RESP_CAT <- NA
  DB$OUT_RESP_CAT <- "1@None"
  DB$OUT_RESP_CAT [DB$OUPNEUMO == "Pneumonia"] <- "2@Pneumonia"
  DB$OUT_RESP_CAT [DB$REINTUB == "Unplanned Intubation"] <- "3@Unplanned reintubation"
  DB$OUT_RESP_CAT [DB$FAILWEAN == "On Ventilator greater than 48 Hours"] <- "3@Fail to wean from vent"
  DB$OUT_RESP_CAT <- factor(DB$OUT_RESP_CAT)
  label(DB$OUT_RESP_CAT) <- "Respiratory Complications"
  
  DB$OUT_RENAL_CAT <- NA
  DB$OUT_RENAL_CAT <- "1@None"
  DB$OUT_RENAL_CAT [DB$RENAINSF == "Progressive Renal Insufficiency"] <- "2@Renal Insufficiency"
  DB$OUT_RENAL_CAT [DB$OPRENAFL == "Acute Renal Failure"] <- "3@Renal Failure"
  DB$OUT_RENAL_CAT <- factor(DB$OUT_RENAL_CAT)
  label(DB$OUT_RENAL_CAT) <- "Renal Complications"
  
  DB$OUT_ANYCOMPLICATION <- NA
  DB$OUT_ANYCOMPLICATION <- "1@No"
  DB$OUT_ANYCOMPLICATION [DB$OUPNEUMO == "Pneumonia" | 
                            DB$REINTUB  == "Unplanned Intubation" | 
                            DB$OTHDVT   == "DVT Requiring Therapy" |
                            DB$PULEMBOL == "Pulmonary Embolism" |
                            DB$FAILWEAN == "On Ventilator greater than 48 Hours" | 
                            DB$RENAINSF == "Progressive Renal Insufficiency" |
                            DB$OPRENAFL == "Acute Renal Failure" |
                            DB$URNINFEC == "Urinary Tract Infection" |
                            DB$CNSCVA   == "Stroke/CVA" |
                            #DB$CNSCOMA  == "" |
                            #DB$NEURODEF == "" |
                            DB$CDARREST == "Cardiac Arrest Requiring CPR" |
                            DB$CDMI     == "Myocardial Infarction" |
                            DB$OTHBLEED == "Transfusions/Intraop/Postop" |
                            DB$OTHSYSEP == "Sepsis" |
                            DB$OTHSESHOCK == "Septic Shock" |
                            DB$RETURNOR == "Yes"] <- "2@Yes"
  DB$OUT_ANYCOMPLICATION <- factor(DB$OUT_ANYCOMPLICATION)
  label(DB$OUT_ANYCOMPLICATION) <- "Overall Complications"
  

  if (Colectomy.Targeted == T)
  {
    DB$BOWEL_PREP <- NA
    DB$BOWEL_PREP <- "1@None"
    DB$BOWEL_PREP [DB$COL_MECH_BOWEL_PREP == "Yes"] <- "2@Mechanical"
    DB$BOWEL_PREP [DB$COL_ORAL_ANTIBIOTIC == "Yes"] <- "3@Antibiotic"
    DB$BOWEL_PREP [DB$COL_MECH_BOWEL_PREP == "Yes" & DB$COL_ORAL_ANTIBIOTIC == "Yes"] <- "4@Mech + Abx"
    DB$BOWEL_PREP <- factor(DB$BOWEL_PREP)
    label(DB$BOWEL_PREP) <- "Bowel Prep"
    
    DB$ILEUS <- NA
    DB$ILEUS [DB$COL_ILEUS == "No"] <- "1@No"
    DB$ILEUS [DB$COL_ILEUS == "Yes"] <- "2@Yes"
    DB$ILEUS <- factor(DB$ILEUS)
    label(DB$ILEUS) <- "Postoperative Ileus"
    
    DB$LEAK <- NA
    DB$LEAK [DB$COL_ANASTOMOTIC %in% c("No","No definitive diagnosis of leak/leak related abscess")]<- "1@No"
    DB$LEAK [DB$COL_ANASTOMOTIC %in% c("Leak, no treatment intervention documented","Leak, treated w/ non-interventional/non-operative means","Yes-no intervention required")]  <- "2@Yes-observed"
    DB$LEAK [DB$COL_ANASTOMOTIC %in% c("Leak, treated w/ interventional means","Yes-percutaneous intervention")]  <- "3@Yes-interventionally treated"
    DB$LEAK [DB$COL_ANASTOMOTIC %in% c("Leak, treated w/ reoperation","Yes-reoperation")]  <- "4@Yes-reoperation"
    DB$LEAK <- factor (DB$LEAK)
    label(DB$LEAK) <- "Anastomotic Leak"
    
    DB$LEAK_SIMPLE <- NA
    DB$LEAK_SIMPLE[DB$LEAK %in% c("1@No")] <- "1@No"
    DB$LEAK_SIMPLE[DB$LEAK %in% c("4@Yes-reoperation", "2@Yes-observed","3@Yes-interventionally treated")] <- "2@Yes"
    DB$LEAK_SIMPLE <- factor (DB$LEAK_SIMPLE)
    label(DB$LEAK_SIMPLE) <- "Anastomotic Leak"
    
  }
  
  return(DB)
}



# Selector from NSQIP datasets
NSQIPSelectCases <- function(by.cpt = F, cpt.codes=c(0),
                             by.icd = F, icd.codes=c(""), 
                             years.selected = c(0), 
                             save = F, save.name="DB.Combined.Rdata",
                             file.dir = "") {
  library("plyr")
  library("data.table")
  # error checking
  if (0 %in% years.selected)
    stop("No years selected: use years.selected = c()")
  
  DB.combined <- data.frame()
  DB.selected <- data.frame()
  for (i in 2005:2015){
    if (i %in% years.selected){
      
      if (i %in% c(2005,2006)) {
        years.text <- "_05_06_vr1"
      } else if(i == 2007) {
        years.text <- "07_TXT"
      } else if(i == 2008) {
        years.text <- "08_TXT"
      } else if(i == 2009) {
        years.text <- "09_TXT"
      } else if(i == 2010) {
        years.text <- "10_TXT"
      } else if(i == 2011) {
        years.text <- "11_TXT"
      } else if(i == 2012) {
        years.text <- "12"
      } else if(i == 2013) {
        years.text <- "13"
      } else if(i == 2014) {
        years.text <- "14"
      } else if(i == 2015) {
        years.text <- "15"
      }
      
      if (! (i == 2006 & 2005 %in% years.selected)) {
        DB.init <- fread(paste(file.dir, paste("ACS_NSQIP_PUF", years.text,".txt",sep=""),sep=""))
        names(DB.init) <- toupper(names(DB.init))

        if (by.cpt == T) {
          DB.selected <- subset(DB.init, CPT %in% cpt.codes)
        } else if (by.icd == T) {
          DB.selected <- subset(DB.init, PODIAG %in% icd.codes)
        } else {
          #stop("Need to select by cpt or icd code")
          DB.selected <- DB.init
        }
        
        DB.combined <- rbind.fill(DB.combined,DB.selected)
      }
    }
    
  }

  if (save == T) 
    save(DB.combined, file=save.name)

  return(DB.combined)
}


