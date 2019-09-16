

DecodeNSQIPThyroid <- function (db) {
  
  colnames(db) <- toupper(colnames(db))
  
  db$THY_INDICATION_GROUP <- NA
  db$THY_INDICATION_GROUP[db$THY_INDICATION %in% c("Any goiter and Graves Disease", "Goiter, multinodular", "Goiter, severe", "Goiter, with substernal component")] <- "1@Goiter"
  db$THY_INDICATION_GROUP[db$THY_INDICATION %in% c("Graves Disease")] <- "2@Graves' disease"
  db$THY_INDICATION_GROUP[db$THY_INDICATION %in% c("Known differentiated malignancy","Known poorly or undifferentiated malignancy","Other malignancy (lymphoma, sarcoma)","Single Nodule or Neoplasm / Single Nodule Goiter")] <- "3@Malignancy"
  db$THY_INDICATION_GROUP[db$THY_INDICATION %in% c("Other specified indication","Unknown")] <- "4@Other or unknown"
  db$THY_INDICATION_GROUP <- factor(db$THY_INDICATION_GROUP)
  label(db$THY_INDICATION_GROUP) <- "Indication for Surgery"
  
  db$THY_PROCEDURE <- NA
  db$THY_PROCEDURE[db$CPT %in% c(60210,60212,60220,60225)] <- "1@Lobectomy"
  db$THY_PROCEDURE[db$CPT %in% c(60240,60252,60254,60270,60271)] <- "2@Total Thyroidectomy"
  db$THY_PROCEDURE[db$CPT %in% c(60260)] <- "3@Completion Thyroidectomy"
  db$THY_PROCEDURE <- factor(db$THY_PROCEDURE)
  label(db$THY_PROCEDURE) <- "Surgery Type"
  
  db$THY_CLINTOX_L <- NA
  db$THY_CLINTOX_L [db$THY_CLINTOX %in% c('Not clinically ""toxic""')] <- "1@No"
  db$THY_CLINTOX_L [db$THY_CLINTOX %in% c('Clinically ""toxic""')] <- "2@Yes"
  db$THY_CLINTOX_L [db$THY_CLINTOX %in% c("N/A","Unknown")] <- NA
  db$THY_CLINTOX_L<- factor(db$THY_CLINTOX_L)
  label(db$THY_CLINTOX_L) <- "Clinical Toxicity" 
  
  db$THY_NECKDISSECT_L <- NA
  db$THY_NECKDISSECT_L[db$THY_NECKDISSECT == "No"] <- "1@No"
  db$THY_NECKDISSECT_L[db$THY_NECKDISSECT == "Yes"] <- "2@Yes"
  db$THY_NECKDISSECT_L <- factor(db$THY_NECKDISSECT_L)
  label(db$THY_NECKDISSECT_L) <- "Neck Dissection"
  
  db$THY_NECKSURG_L <- NA
  db$THY_NECKSURG_L[db$THY_NECKSURG  %in% c("No-no specific evidence of such prior surgery")] <- "1@No"
  db$THY_NECKSURG_L[db$THY_NECKSURG  %in% c('Yes-midline in the neck only, not extending to either side')] <- "2@Yes, midline"
  db$THY_NECKSURG_L[db$THY_NECKSURG  %in% c('Yes-on one side of the neck only, ""contralateral""')] <- "2@Yes, contralateral"
  db$THY_NECKSURG_L[db$THY_NECKSURG  %in% c('Yes-on both sides of the neck previously, ""bilateral""','Yes-on one side of the neck only, ""ipsilateral""')] <- "2@Yes, ipsilateral or bilateral"
  db$THY_NECKSURG_L[db$THY_NECKSURG  %in% c('Yes-other, not specifically described')] <- "2@Yes, not specified"
  db$THY_NECKSURG_L <- factor(db$THY_NECKSURG_L)
  label(db$THY_NECKSURG_L) <- "Prior Neck Surgery"
  
  db$THY_ELECTRO_L <- NA
  db$THY_ELECTRO_L[db$THY_ELECTRO %in% c("No")] <- "1@No Nerve Monitor" 
  db$THY_ELECTRO_L[db$THY_ELECTRO %in% c("Yes")] <- "2@Yes Nerve Monitor"
  db$THY_ELECTRO_L <- factor(db$THY_ELECTRO_L)
  label(db$THY_ELECTRO_L) <- "Use of Intraop Nerve Monitor"
  
  db$THY_SCALPEL_L <- NA
  db$THY_SCALPEL_L[db$THY_SCALPEL == "No"] <- "1@No"
  db$THY_SCALPEL_L[db$THY_SCALPEL == "Yes"] <- "2@Yes"
  db$THY_SCALPEL_L <- factor(db$THY_SCALPEL_L)
  label(db$THY_SCALPEL_L) <- "Use of vessel sealant device"
  
  db$THY_CALCIUM_L <- NA
  db$THY_CALCIUM_L[db$THY_CALCIUM == "No/unknown"] <- "1@No"
  db$THY_CALCIUM_L[db$THY_CALCIUM == "Yes"] <- "2@Yes"
  db$THY_CALCIUM_L <- factor(db$THY_CALCIUM_L)
  label(db$THY_CALCIUM_L) <- "Postop Calcium Checked"
  
  db$THY_PARA_L <- NA
  db$THY_PARA_L[db$THY_PARA == "No/unknown"] <- "1@No"
  db$THY_PARA_L[db$THY_PARA == "Yes"] <- "2@Yes"
  db$THY_PARA_L <- factor(db$THY_PARA_L)
  label(db$THY_PARA_L) <- "Postop PTH Checked"
  
  db$THY_CALCIUMD_REP_L <- NA
  db$THY_CALCIUMD_REP_L[db$THY_CALCIUMD_REP  %in% c("No-no calcium or vitamin D pills", "Unknown")] <- "1@No/unknown"
  db$THY_CALCIUMD_REP_L[db$THY_CALCIUMD_REP  %in% c("Yes-oral calcium pills")] <- "2@Yes, calcium"
  db$THY_CALCIUMD_REP_L[db$THY_CALCIUMD_REP  %in% c("Yes-oral vitamin D pills")] <- "3@Yes, vit D"
  db$THY_CALCIUMD_REP_L[db$THY_CALCIUMD_REP  %in% c("Yes-both oral calcium and oral vitamin D pills")] <- "4@Yes, both calcium and vit D"
  db$THY_CALCIUMD_REP_L <- factor(db$THY_CALCIUMD_REP_L)
  label(db$THY_CALCIUMD_REP_L) <- "Postop Calcium and Vitamin D Replacement"
  
  db$THY_NECK_HEMATOMA_L <- NA
  db$THY_NECK_HEMATOMA_L[db$THY_NECK_HEMATOMA  %in% c("No","Unknown")] <- "1@No"
  db$THY_NECK_HEMATOMA_L[db$THY_NECK_HEMATOMA  %in% c("Yes-hematoma noted, addit","Yes-hematoma noted, no ob","Yes-hematoma noted, trach","Yes-other intervention no")] <- "2@Yes"
  db$THY_NECK_HEMATOMA_L <- factor(db$THY_NECK_HEMATOMA_L)
  label(db$THY_NECK_HEMATOMA_L) <- "Neck hematoma"
  
  db$THY_LARYNGEAL_L <- NA
  db$THY_LARYNGEAL_L[db$THY_LARYNGEAL %in% c("No","Unknown")] <- "1@No"
  db$THY_LARYNGEAL_L[db$THY_LARYNGEAL %in% c("Yes-hoarseness/vocal cord","Yes-severe hoarseness/voc")] <- "2@Yes"
  db$THY_LARYNGEAL_L <- factor(db$THY_LARYNGEAL_L)
  label(db$THY_LARYNGEAL_L) <- "Recurrent laryngeal nerve injury"
  
  db$READMISSION_L <- NA
  db$READMISSION_L <- "1@No"
  db$READMISSION_L[(db$READMISSION1 == "Yes") | (db$READMISSION2 == "Yes") ] <- "2@Yes"
  db$READMISSION_L <- factor(db$READMISSION_L)
  label(db$READMISSION_L) <- "Readmission"
  
  db$OPTIME_L2 <- db$OPTIME_L / 10
  label(db$OPTIME_L2) <- "Operative time (10 min intervals)"
  
  db$OPTIME_L3 <- NA
  db$OPTIME_L3[db$OPTIME_L <= 105] <- "1@ Less/Equal to 105 mins"
  db$OPTIME_L3[db$OPTIME_L > 105] <- "2@ Greater than 105 mins"
  db$OPTIME_L3 <- factor(db$OPTIME_L3)
  label(db$OPTIME_L3) <- "Operative time"
  
  db$OUTCOME_HYPOCALCEMIA_BEFORE_DC <- NA
  db$OUTCOME_HYPOCALCEMIA_BEFORE_DC <- "1@No Post-op Hypocalcemia"
  db$OUTCOME_HYPOCALCEMIA_BEFORE_DC[(db$THY_HYPOCALC == "Yes" )] <- "2@Post-op Hypocalcemia Before DC"
  db$OUTCOME_HYPOCALCEMIA_BEFORE_DC <- factor(db$OUTCOME_HYPOCALCEMIA_BEFORE_DC)
  label(db$OUTCOME_HYPOCALCEMIA_BEFORE_DC) <- "Hypocalcemia Event Before Discharge"
  
  db$OUTCOME_HYPOCALCEMIA_30D <- NA
  db$OUTCOME_HYPOCALCEMIA_30D <- "1@No Post-op Hypocalcemia"
  db$OUTCOME_HYPOCALCEMIA_30D[(db$THY_HYPOCALC30 == "Yes") ] <- "2@Post-op Hypocalcemia Within 30 Days"
  db$OUTCOME_HYPOCALCEMIA_30D <- factor(db$OUTCOME_HYPOCALCEMIA_30D)
  label(db$OUTCOME_HYPOCALCEMIA_30D) <- "Hypocalcemia Event Within 30 Days"
  
  db$OUTCOME_HYPOCALCEMIA_ANY <- NA
  db$OUTCOME_HYPOCALCEMIA_ANY <- "1@No Post-op Hypocalcemia"
  db$OUTCOME_HYPOCALCEMIA_ANY[(db$THY_HYPOCALC == "Yes" ) | (db$THY_HYPOCALC30 == "Yes") | (db$THY_HYPOCALC_EVENT == "Yes") ] <- "2@Post-op Hypocalcemia"
  db$OUTCOME_HYPOCALCEMIA_ANY <- factor(db$OUTCOME_HYPOCALCEMIA_ANY)
  label(db$OUTCOME_HYPOCALCEMIA_ANY) <- "Any Hypocalcemia Event"
  
  db$OUTCOME_HYPOCALCEMIA_IVCAL <- NA
  db$OUTCOME_HYPOCALCEMIA_IVCAL <- "1@No IV Calcium Required"
  db$OUTCOME_HYPOCALCEMIA_IVCAL[db$THY_HYPOCALC_EVENTTYPE %in% c("IV calcium supplementation","IV calcium supplementation,Emergent evaluation in clinical office/emergency dept","IV calcium supplementation,Emergent evaluation in clinical office/emergency dept,Readmitted for low calcium","IV calcium supplementation,Readmitted for low calcium") ] <- "2@IV Calcium Required"
  db$OUTCOME_HYPOCALCEMIA_IVCAL <- factor(db$OUTCOME_HYPOCALCEMIA_IVCAL)
  label(db$OUTCOME_HYPOCALCEMIA_IVCAL) <- "Hypocalcemia Requiring IV Caclium"
  
  return (db)
}