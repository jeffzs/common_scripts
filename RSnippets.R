# (c) 2014-present
# Author: Zhifei Sun, zhifei.sun@duke.edu
#
# Purpose: Repository for frequently used R codes

# HEADER FOR ALL PROJECTS
cat("\014")
rm(list = ls())
setwd("~")
DIR.LIB <- "/users/zhifeisun/dropbox/Jeff/Work - Active/[Analysis]/"
source(paste0(DIR.LIB,"CustomScripts.R"))
source(paste0(DIR.LIB,"CustomScripts_NSQIP.R"))
# LOADING AND MANIPULATING DATA ####

# Loading data #
  # NCDB
  NCDB_RAW <- read.csv(paste(DIR.DB,"db_NCDB/NCDB_XXXX_2012.csv",sep=""),header=TRUE,sep=',',na.strings=c("", "NA"))
  NCDB_DECODED <- DecodeNCDBDataset(NCDB_RAW)
  NCDB_DECODED <- DecodeNCDBTNMStage(NCDB_DECODED)
  DB$YEAR_OF_DIAGNOSIS_L <- factor(DB$YEAR_OF_DIAGNOSIS_L)
  label(DB$YEAR_OF_DIAGNOSIS_L) <- "Year of Diagnosis"
  # NSQIP
  DB <- NSQIPSelectCases(by.cpt = T, cpt.codes=c(44150),years.selected = c(2005,2006,2007,2008),save = F)
  DB <- NSQIPDecodeData(DB)
  
  # KID
  # please see one of the KID db code files
  
  # load gdata package for reading excel sheets
  library(gdata)                   
  DB <- read.xls("EXCEL.xlsx",sheet = "SHEETNAME",na.strings = c("-",""),pattern="FIRST LINE THAT YOU WANT")  
  
  
# Manipulating data #
  # find any value in a row that belongs to a dataframe Criteria_df and set Var1 to Value1
  DB$Var1 [ apply( DB, 1, function(x) any(x %in% Criteria_df ))] <- "Value1"
  
  # rename columns
  colnames(DB)[colnames(DB)=="Column_Name"] <- "New_Column_Name"

  # decoding variables
  DB$X2 <- NA
  DB$X2[DB$X1 == 0] <- "A"
  DB$X2[DB$X1 == 1] <- "B"
  DB$X2 <- factor(DB$X2)
  
  # change factors to numbers
  DB$X1 <- as.numeric(paste(DB$X1))
  
# Displaying Data #
  # Hmisc summary table
  Table1.Formula <- Var1 + Var2 + Var3 ~ Group
  CreateSummaryTable(formula = Table1.Formula, data = DB, title = "Table.1", save=T)

  # display median of Y1, displayed in a 2x2 table between X1 and X2
  aggdata <-aggregate(DB$Y1, by=list(DB$X1,DB$X2), FUN=median, na.rm=TRUE)

  # unique data
  unique(t(apply(DB[,c("COLUMN 1","COLUMN 2")], 1, sort)))
  
  
# Formula Manipulation
  Formula.Common <- OUT_WOUND ~ APPROACH +AGE_L + SEX + RACE_L +BMI + ASACLAS + BOWEL_PREP+ OPERATION
  
  Formula.Wound <- reformulate(termlabels = attr(terms(Formula.Common), 'term.labels'), response = 'OUT_WOUND')
  
  
# MULTIVARIABLE MODELING ####

# Linear regression and log-transformed linear regression
  Model.Formula <- Log(Y) ~ X1 + X2
  DB.Model <- lm(Model.Formula, data=DB.AVM.Complete)
  summary(DB.Model)
  CreateLogLmTable(DB.Model, save = T, filename = "AVM-Cost-Pred.csv", decimal_point = 0)
  

# Logistic regression, predictors of outcomes
  Formula.Margin <- RX_SUMM_SURGICAL_MARGINS_L ~ SURGERY+AGE+SEX_L+PRIMARY_SITE_L+FACILITY_TYPE_CD_L+BRESLOW+TUMOR_SIZE_CAT
  Model.Margin.SAll <- glm(Formula.Margin, data=DB.PedSurg, family = "binomial")
  CreateOddsRatioTable(gfit = Model.Margin.SAll, filename = "Model.Margin.SAll.csv")

   # predictors of using guideline ChemoXRT
   DB.Predictor = as.data.frame(na.omit(DB[,c("HADCHEMOXRT","AGE", "SEX_L", "RACE_L","INSURANCE_STATUS_L","CDCC_TOTAL_L","FACILITY_TYPE_CD_L","INCOME_MEDIAN","EDUCATION_MEDIAN","ANALYTIC_STAGE_GROUP_L")]))
   Formula.Predictor <- HADCHEMOXRT~AGE+SEX_L+RACE_L+INSURANCE_STATUS_L+CDCC_TOTAL_L+FACILITY_TYPE_CD_L+INCOME_MEDIAN+EDUCATION_MEDIAN+ANALYTIC_STAGE_GROUP_L 
   
   Model.Predictor<- glm(Formula.Predictor, data=DB.Predictor, family="binomial")
   Model.Predictor.step <- stepAIC(Model.Predictor, direction="backward")
   #Model.Predictor.step$anova        # display results
   Model.Predictor.post<- glm(Model.Predictor.step$formula, data=DB.Predictor, family="binomial")
   
   Predictor.Results <- CreateOddsRatioTable(Model.Predictor, filename = "Predictor.HadChemoXRT-full.csv")
   # CreateOddsRatioTable(Model.Predictor.post, filename = "Predictor.HadChemoXRT-reduced.csv")
   ggForestPlot(Predictor.Results, x.tick=c(0,0.5,1,1.5,2), remove=4, xlab="Odds Ratio of Receving Neoadjuvant Chemoradiation",
                save = T, file.name = "Fig.FP.HadChemoXRT.pdf", file.height = 4, file.width = 8,
                labels=c("Age",
                         "Female vs. Male",
                         "Black vs. White",
                         "Other vs. White",
                         "Private vs. No Insurance",
                         "Government vs. No Insurance",
                         "Charlson Deyo Score 1 vs. 0",
                         "Charlson Deyo Score 2+ vs. 0",
                         "Comprehensive vs. Community Hospital",
                         "Academic vs. Community Hospital",
                         "Income Above Median",
                         "Education Above Median",
                         "Treatment At Different Hospital",
                         "Stage III vs. II"))


# Cox proportional hazard model
  
  
# PROPENSITY MATCHING ####
set.seed(100) # need to set seed to get the same results every time
match.out <- matchit(Category ~ FactorA + FactorB, Data, 
                     method = 'nearest', distance = 'logit', caliper = .10, ratio = 1)
###
DB.prematch = as.data.frame(na.omit(DB.Seg[,c("CASEID","APPROACH","AGE_L", "SEX", "RACE_L","BMI","ASA_L","INDICATION","BOWEL_PREP")]))
DB.prematch$treatment <- NA
DB.prematch$treatment[DB.prematch$APPROACH == "1@Open"] <-1
DB.prematch$treatment[DB.prematch$APPROACH == "2@Lap-HandAssist"] <-0

m.out <- matchit(treatment~AGE_L+SEX+RACE_L+BMI+ASA_L+INDICATION+BOWEL_PREP,
                 method="nearest",data=DB.prematch, ratio = 1)
m.s <- summary(m.out, standardize=T)$sum.match
write.table(m.s, file="StDiff.Open-LapHandAssist-Seg.csv",sep = ",")
m.final <- match.data(m.out)

DB.LapHandAssist.postmatch <- subset(DB.Seg, CASEID %in% m.final$CASEID)
CreateSummaryTable(Formula.Table.1,DB.LapHandAssist.postmatch,title = "Table.3.PS-Open-LapHandAssist-Seg",save = T)  

# ONE FUNCTION  
DB.postmatch <- PropensityMatch(DB = DB, 
                                variables.list = c("CASEID","EXTENT_OF_SURGERY","AGE_L", "SEX_L", "RACE_L","BMI","ASA_L","PREOP_WBC","PREOP_SEPSIS","OSTOMY"),
                                formula.match = treatment~AGE_L+SEX_L+RACE_L+BMI+ASA_L+PREOP_WBC+PREOP_SEPSIS+OSTOMY,
                                grouping.variable = "EXTENT_OF_SURGERY", control.group = "1@TAC", treat.group = "2@Segmental",
                                merge.variable = "CASEID",
                                save.stdiff = T, save.stdiff.title = "StDiff.Cdiff.csv",
                                formula.summary = Formula.Table.1, save.summary.title = "PS-Cdiff.csv")
  
# PLOTTING ####

  #KM
KM.Survival<- survfit(Surv(DX_LASTCONTACT_DEATH_MONTHS, PUF_VITAL_STATUS_RFORMAT) ~ PREOP_THERAPY , data=DB.SURV)
KM.Survival.Fig   <- ggkmTable(KM.Survival,timeby=12, xlab="Time (months)", xlim = 90,
                               ystrataname = "Neoadjuvant Therapy",
                               ystratalabs=c("None",
                                             "Chemotherapy Only",
                                             "Chemoradiotherapy"), 
                               main = "", risktable=T, save = T, 
                               file.name = "Figure.KM.Overall_ver2.pdf", file.height = 6, file.width = 7 )
  
  
# spline plots
  
  
# PLOT FACETS
# change labels of ggplot facets
malformation_groups <- list(
  '1Hemangioma'="Hemangioma",
  '2AV Malformations'="AVM",
  '4Lymphangioma'="Lymphangioma")
malformation_labeller <- function(variable,value){return(malformation_groups[value])}

# TREND BARPLOT
ggplot(DB,aes(x=factor(YEAR.x)))+
  geom_bar()+
  facet_grid(~MALFORMATION_TYPE, labeller = malformation_labeller) + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x  = element_text(colour="black",face="bold",size=14),
        axis.text.y  = element_text(colour="black",face="bold",size=14)) +
  xlab("Year of Diagnosis") +
  ylab("Number of Hospital Admissions") +
  ggtitle("")


# ggForestPlot
ggForestPlot(a, x.tick=c(0,0.5,1,1.5,2), remove=4, xlab="Odds Ratio of Receving Neoadjuvant Chemoradiation",
             labels=c("Age",
                      "Female vs. Male",
                      "Black vs. White",
                      "Other vs. White",
                      "Private vs. No Insurance",
                      "Government vs. No Insurance",
                      "Charlson Deyo Score 1 vs. 0",
                      "Charlson Deyo Score 2+ vs. 0",
                      "Comprehensive vs. Community Hospital",
                      "Academic vs. Community Hospital",
                      "Income Above Median",
                      "Education Above Median",
                      "Treatment At Different Hospital",
                      "Stage III vs. II"))


# ggplot EXTRAS #########
ggplot(DB_VATS, aes(factor(HOSPITAL_ID))) +
  geom_bar(colour="black", fill="black",width=0.4) + 
  theme(axis.text.x = element_text(colour="black",face="bold",size=14), 
        axis.text.y = element_text(colour="black",face="bold",size=14),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("Hospitals") +
  scale_x_discrete(breaks=NULL)+
  ylab("Case Count at Each Hospital\nOver Study Period") +
  ylim(0,max(table(DB_VATS$HOSPITAL_ID)))+
  theme(axis.text.y  = element_text(colour="black",face="bold",size=14))+
  ggtitle("VATS")

## ggplot labeller for facets
ggFacetPlotLabeller <- function(variable,value) {
  if (variable=='facet1') {
    return(facet1_names[value])
  } else if (variable=='facet2') {
    return(facet2_names[value])
  } else {
    return(as.character(value))
  }
}

