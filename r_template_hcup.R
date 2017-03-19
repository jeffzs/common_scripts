source(paste0(github_url,"r_hcup.R"))



# define index and readmission cases selection criteria
icd <-  c("0000")
icd.readmit <- c("0000")


core.variables <- c("KEY","AGE","YEAR","FEMALE", "RACE", "PAY1",
                    "DX1","DX2","DX3","DX4","DX5","DX6","DX7","DX8","DX9","DX10","DX11","DX12","DX13","DX14","DX15",
                    "PR1","PR2","PR3","PR4","PR5","PR6","PR7","PR8","PR9","PR10","PR11","PR12","PR13","PR14","PR15",
                    "LOS","TOTCHG", "DIED","DSHOSPID")

case.selection.expression <- "AGE < 18 & (DX1  %in% icd.appendicitis | 
    DX2 %in% icd.appendicitis | 
    DX3 %in% icd.appendicitis | 
    DX4 %in% icd.appendicitis | 
    DX5 %in% icd.appendicitis |
    DX6 %in% icd.appendicitis |
    DX7 %in% icd.appendicitis |
    DX8 %in% icd.appendicitis |
    DX9 %in% icd.appendicitis |
    DX10 %in% icd.appendicitis |
    DX11 %in% icd.appendicitis |
    DX12 %in% icd.appendicitis |
    DX13 %in% icd.appendicitis |
    DX14 %in% icd.appendicitis |
    DX15 %in% icd.appendicitis)"

case.readmission.expression <- "AGE < 23"

# & (DX1  %in% icd.appendicitis.readmit | 
#     DX2 %in% icd.appendicitis.readmit | 
#     DX3 %in% icd.appendicitis.readmit | 
#     DX4 %in% icd.appendicitis.readmit | 
#     DX5 %in% icd.appendicitis.readmit |
#     DX6 %in% icd.appendicitis.readmit |
#     DX7 %in% icd.appendicitis.readmit |
#     DX8 %in% icd.appendicitis.readmit |
#     DX9 %in% icd.appendicitis.readmit |
#     DX10 %in% icd.appendicitis.readmit |
#     DX11 %in% icd.appendicitis.readmit |
#     DX12 %in% icd.appendicitis.readmit |
#     DX13 %in% icd.appendicitis.readmit |
#     DX14 %in% icd.appendicitis.readmit |
# #    DX15 %in% icd.appendicitis.readmit )

case.selection.expression.ut <- "AGE < 18 & (DX1  %in% icd.appendicitis | 
    DX2 %in% icd.appendicitis | 
DX3 %in% icd.appendicitis | 
DX4 %in% icd.appendicitis | 
DX5 %in% icd.appendicitis |
DX6 %in% icd.appendicitis |
DX7 %in% icd.appendicitis |
DX8 %in% icd.appendicitis |
DX9 %in% icd.appendicitis)"

case.readmission.expression.ut <- "AGE < 23" 
# & (DX1  %in% icd.appendicitis.readmit | 
# DX2 %in% icd.appendicitis.readmit | 
# DX3 %in% icd.appendicitis.readmit | 
# DX4 %in% icd.appendicitis.readmit | 
# DX5 %in% icd.appendicitis.readmit |
# DX6 %in% icd.appendicitis.readmit |
# DX7 %in% icd.appendicitis.readmit |
# DX8 %in% icd.appendicitis.readmit |
# DX9 %in% icd.appendicitis.readmit)"


# load data
dropbox_path = "/Public/research/data/HCUP/SID/California/"
file = "CA_SID_2009_CORE.CSV"
drop_get(dtoken = token, paste0(dropbox_path,file)) # get file from dropbox
#data <- fread(file)



data <- select_hcup_sid_core_cases(core.file = file, core.variables = core.variables ,revisit.file="" )
data <- link_hcup_sid_core_with_hospital(core.data=data, aha.file="")
data <- link_hcup_sid_core_with_cost(core.data=data, cost.file="")
data <- link_hcup_sid_core_with_severity(core.data=data, severity.file="")
data <- link_hcup_sid_core_with_dxprgrp(core.data=data, dxprgrp.file="")

# CALIFORNIA
ca.07 <- HCUPSelectCase(core.file = paste0(dir.sid,"California/CA_SID_2007_CORE.csv"),
                            core.variables = core.variables,
                            aha.file = paste0(dir.sid,"California/CA_SID_2007_AHAL.csv"),
                            revisit.file = paste0(dir.sid,"California/CA_2007_daystoevent.csv")) 
ca.08 <- HCUPSelectCase(core.file = paste0(dir.sid,"California/CA_SID_2008_CORE.csv"),
                            core.variables = core.variables,
                            aha.file = paste0(dir.sid,"California/CA_SID_2008_AHAL.csv"),
                            revisit.file = paste0(dir.sid,"California/CA_2008_daystoevent.csv")) 
ca.09<- HCUPSelectCase(core.file = paste0(dir.sid,"California/CA_SID_2009_CORE.csv"),
                            core.variables = core.variables,
                            aha.file = paste0(dir.sid,"California/CA_SID_2009_AHAL.csv")) 
ca.10 <- HCUPSelectCase(core.file = paste0(dir.sid,"California/CA_SID_2010_CORE.csv"),
                            core.variables = core.variables,
                            aha.file = paste0(dir.sid,"California/CA_SID_2010_AHAL.csv")) 
df.ca <- rbindlist(list(ca.07,ca.08,ca.09,ca.10), fill=T, use.names = T)
rm(ca.07,ca.08,ca.09,ca.10)  
gc() # garbage collect to free up memory

ca.a <- df.ca[eval(parse(text=case.selection.expression))]          # select index admission cases
ca.c <- df.ca[eval(parse(text=case.readmission.expression))]        # select readmission cases
rm(df.ca)
gc()

# New York
ny.07 <- HCUPSelectCase(core.file = paste0(dir.sid,"New York/ny_SID_2007_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"New York/ny_SID_2007_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"New York/ny_2007_daystoevent.csv")) 
ny.08 <- HCUPSelectCase(core.file = paste0(dir.sid,"New York/ny_SID_2008_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"New York/ny_SID_2008_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"New York/ny_2008_daystoevent.csv")) 
ny.09<- HCUPSelectCase(core.file = paste0(dir.sid,"New York/ny_SID_2009_CORE.csv"),
                       core.variables = core.variables,
                       aha.file = paste0(dir.sid,"New York/ny_SID_2009_AHAL.csv")) 
ny.10 <- HCUPSelectCase(core.file = paste0(dir.sid,"New York/ny_SID_2010_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"New York/ny_SID_2010_AHAL.csv")) 
df.ny <- rbindlist(list(ny.07,ny.08,ny.09,ny.10), fill=T, use.names = T)
rm(ny.07,ny.08,ny.09,ny.10)  
gc()
ny.a <- df.ny[eval(parse(text=case.selection.expression))]          # select index admission nyses
ny.c <- df.ny[eval(parse(text=case.readmission.expression))]        # select readmission nyses
rm(df.ny)
gc()
# Florida
fl.07 <- HCUPSelectCase(core.file = paste0(dir.sid,"Florida/fl_SID_2007_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Florida/fl_SID_2007_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"Florida/fl_2007_daystoevent.csv")) 
fl.08 <- HCUPSelectCase(core.file = paste0(dir.sid,"Florida/fl_SID_2008_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Florida/fl_SID_2008_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"Florida/fl_2008_daystoevent.csv")) 
fl.09<- HCUPSelectCase(core.file = paste0(dir.sid,"Florida/fl_SID_2009_CORE.csv"),
                       core.variables = core.variables,
                       aha.file = paste0(dir.sid,"Florida/fl_SID_2009_AHAL.csv")) 
fl.10 <- HCUPSelectCase(core.file = paste0(dir.sid,"Florida/fl_SID_2010_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Florida/fl_SID_2010_AHAL.csv")) 
df.fl <- rbindlist(list(fl.07,fl.08,fl.09,fl.10), fill=T, use.names = T)
rm(fl.07,fl.08,fl.09,fl.10)  
gc()
fl.a <- df.fl[eval(parse(text=case.selection.expression))]          # select index admission flses
fl.c <- df.fl[eval(parse(text=case.readmission.expression))]        # select readmission flses
rm(df.fl)
gc()
# Utah
ut.07 <- HCUPSelectCase(core.file = paste0(dir.sid,"Utah/ut_SID_2007_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Utah/ut_SID_2007_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"Utah/ut_2007_daystoevent.csv")) 
ut.08 <- HCUPSelectCase(core.file = paste0(dir.sid,"Utah/ut_SID_2008_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Utah/ut_SID_2008_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"Utah/ut_2008_daystoevent.csv")) 
ut.09<- HCUPSelectCase(core.file = paste0(dir.sid,"Utah/ut_SID_2009_CORE.csv"),
                       core.variables = core.variables,
                       aha.file = paste0(dir.sid,"Utah/ut_SID_2009_AHAL.csv")) 
ut.10 <- HCUPSelectCase(core.file = paste0(dir.sid,"Utah/ut_SID_2010_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"Utah/ut_SID_2010_AHAL.csv")) 
df.ut <- rbindlist(list(ut.07,ut.08,ut.09,ut.10), fill=T, use.names = T)
rm(ut.07,ut.08,ut.09,ut.10)  
gc()
ut.a <- df.ut[eval(parse(text=case.selection.expression.ut))]          # select index admission utses
ut.c <- df.ut[eval(parse(text=case.readmission.expression.ut))]        # select readmission utses
rm(df.ut)
gc()

# North Carolina
nc.07 <- HCUPSelectCase(core.file = paste0(dir.sid,"North Carolina/nc_SID_2007_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"North Carolina/nc_SID_2007_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"North Carolina/nc_2007_daystoevent.csv")) 
nc.08 <- HCUPSelectCase(core.file = paste0(dir.sid,"North Carolina/nc_SID_2008_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"North Carolina/nc_SID_2008_AHAL.csv"),
                        revisit.file = paste0(dir.sid,"North Carolina/nc_2008_daystoevent.csv")) 
nc.09<- HCUPSelectCase(core.file = paste0(dir.sid,"North Carolina/nc_SID_2009_CORE.csv"),
                       core.variables = core.variables,
                       aha.file = paste0(dir.sid,"North Carolina/nc_SID_2009_AHAL.csv")) 
nc.10 <- HCUPSelectCase(core.file = paste0(dir.sid,"North Carolina/nc_SID_2010_CORE.csv"),
                        core.variables = core.variables,
                        aha.file = paste0(dir.sid,"North Carolina/nc_SID_2010_AHAL.csv")) 
df.nc <- rbindlist(list(nc.07,nc.08,nc.09,nc.10), fill=T, use.names = T)
rm(nc.07,nc.08,nc.09,nc.10)  
gc()
nc.a <- df.nc[eval(parse(text=case.selection.expression))]          # select index admission ncses
nc.c <- df.nc[eval(parse(text=case.readmission.expression))]        # select readmission ncses
rm(df.nc)
gc()

# combine all files ######
a <- as.data.table(rbind.fill(ca.a,ny.a,fl.a, ut.a, nc.a))
c <- as.data.table(rbind.fill(ca.c,ny.c,fl.c, ut.c, nc.c))

save(a, file="a-first.Rdata")
save(c, file="c-first.Rdata")




##### end





a <- a[!is.na(a$VISITLINK)]      # remove missing visitlink patients
c <- c[!is.na(c$VISITLINK)]

a <- a[order(VISITLINK, DAYSTOEVENT)]     # sort by visitlink and days to event
a <- a[ !duplicated(a, by="VISITLINK")]   # subselect df of only the unqiue patients
a.visitlink.unique <- a$VISITLINK         # get the visitlink of all unique patients

# decode data frame ####
a[PR1 %in% icd.appy, PROC_A:="APPY"][PR1 %in% icd.drain, PROC_B:="DRAIN"]
a[PR2 %in% icd.appy, PROC_A:="APPY"][PR2 %in% icd.drain, PROC_B:="DRAIN"]
a[PR3 %in% icd.appy, PROC_A:="APPY"][PR3 %in% icd.drain, PROC_B:="DRAIN"]
a[PR4 %in% icd.appy, PROC_A:="APPY"][PR4 %in% icd.drain, PROC_B:="DRAIN"]
a[PR5 %in% icd.appy, PROC_A:="APPY"][PR5 %in% icd.drain, PROC_B:="DRAIN"]
a[PR6 %in% icd.appy, PROC_A:="APPY"][PR6 %in% icd.drain, PROC_B:="DRAIN"]
a[PR7 %in% icd.appy, PROC_A:="APPY"][PR7 %in% icd.drain, PROC_B:="DRAIN"]
a[PR8 %in% icd.appy, PROC_A:="APPY"][PR8 %in% icd.drain, PROC_B:="DRAIN"]
a[PR9 %in% icd.appy, PROC_A:="APPY"][PR9 %in% icd.drain, PROC_B:="DRAIN"]
a[PR10 %in% icd.appy, PROC_A:="APPY"][PR10 %in% icd.drain, PROC_B:="DRAIN"]
a[PR11 %in% icd.appy, PROC_A:="APPY"][PR11 %in% icd.drain, PROC_B:="DRAIN"]
a[PR12 %in% icd.appy, PROC_A:="APPY"][PR12 %in% icd.drain, PROC_B:="DRAIN"]
a[PR13 %in% icd.appy, PROC_A:="APPY"][PR13 %in% icd.drain, PROC_B:="DRAIN"]
a[PR14 %in% icd.appy, PROC_A:="APPY"][PR14 %in% icd.drain, PROC_B:="DRAIN"]
a[PR15 %in% icd.appy, PROC_A:="APPY"][PR15 %in% icd.drain, PROC_B:="DRAIN"]




a$READMIT <- NA
a$READMIT[a$READMIT_NUM == 0] <- "1@No"
a$READMIT[a$READMIT_NUM > 0] <-  "2@Yes"
a$READMIT <- factor(a$READMIT)

a$TOTCHG_2016[a$YEAR == 2007] <-  a$TOTCHG[a$YEAR == 2007]*1.14 #adjust for inflation - index hospitalization
a$TOTCHG_2016[a$YEAR == 2008] <-  a$TOTCHG[a$YEAR == 2008]*1.10
a$TOTCHG_2016[a$YEAR == 2009] <-  a$TOTCHG[a$YEAR == 2009]*1.11
a$TOTCHG_2016[a$YEAR == 2010] <-  a$TOTCHG[a$YEAR == 2010]*1.09

a$TOTCHG_SUM_2016[a$YEAR == 2007] <-  a$TOTCHG_SUM[a$YEAR == 2007]*1.14 #adjust for inflation - all hospitalization
a$TOTCHG_SUM_2016[a$YEAR == 2008] <-  a$TOTCHG_SUM[a$YEAR == 2008]*1.10
a$TOTCHG_SUM_2016[a$YEAR == 2009] <-  a$TOTCHG_SUM[a$YEAR == 2009]*1.11
a$TOTCHG_SUM_2016[a$YEAR == 2010] <-  a$TOTCHG_SUM[a$YEAR == 2010]*1.09

a.prev_admit <- a$VISITLINK[a$PREV_ADMIT == 1]   # Select out the readmission encounters and save to 'f'
f <- subset(f, !VISITLINK %in% a.prev_admit)
save(f, file="f.Rdata")



a <- subset(a, PREV_ADMIT == 0)      # remove cases where there was a previous admission from the index.


# hospital information, cost to charge

cc.2007 <- fread(paste0(dir.sid,"Cost-to-Charge/cc2007CD.csv"),na.strings = c(".","NA",""))
cc.2008 <- fread(paste0(dir.sid,"Cost-to-Charge/cc2008CD.csv"),na.strings = c(".","NA",""))
cc.2009 <- fread(paste0(dir.sid,"Cost-to-Charge/cc2009CD.csv"),na.strings = c(".","NA",""))
cc.2010 <- fread(paste0(dir.sid,"Cost-to-Charge/cc2010CD.csv"),na.strings = c(".","NA",""))
cc <- rbind.fill(cc.2007,cc.2008,cc.2009,cc.2010)
rm(cc.2007,cc.2008,cc.2009,cc.2010)
cc <- as.data.frame(sapply(cc, function(x) gsub("\'", "", x)))  # remove all the god damn single quotes
names(cc) <- sapply(names(cc), function(x) gsub("\'", "", x))  # remove all the god damn single quotes from column names
cc$HOSPID <-  sub("^[0]+", "", cc$HOSPID) 

g <- merge(a, cc, by = c("HOSPID","YEAR"), all.x = T)

g$TOTCOST_2016 <- g$TOTCHG_2016 * as.double(paste(g$GAPICC))   # get cost from cc ratio for index admission
label(g$TOTCOST_2016) <- "Index Admission Costs (2016 USD)"
g$TOTCOST_SUM_2016 <- g$TOTCHG_SUM_2016 * as.double(paste(g$GAPICC))   # get cost from cc ratio for ALL admission
label(g$TOTCOST_SUM_2016) <- "Cumulative Admission Costs (2016 USD)"

# backup data ####

save(a, file="a.Rdata")
save(c, file="c.Rdata")
save(g, file="g.Rdata")

#load(file = "a.Rdata")

# Descriptive Analysis: 1st admission ####
formula <- AGE+SEX_L+RACE_L+YEAR+HOSPST+PAY_L+
  HOSPITAL_VOLUME+HOSPITAL_URBAN+LOS+DIED+
  TOTCHG_2016+TOTCOST_2016+
  READMIT+READMIT_NUM+TOTCHG_SUM_2016+TOTCOST_SUM_2016 ~ PROC_TOTAL

CreateSummaryTable(formula, g, title="Table.Overall", print.sd=T, save = T,exclude1 = F,round = 0)



