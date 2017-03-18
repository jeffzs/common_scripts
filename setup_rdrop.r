# rdrop2
# https://github.com/karthik/rdrop2

# install
install.packages('rdrop2')
library(rdrop2)
token <- readRDS("~/credentials/droptoken.rds") # downloaded from local machine

# usage
drop_dir <- "/Jeff/work - active/"
current_dir <- "~/"

# read directors
drop_dir(dtoken = token) # must use dtoken=token for all

# get files
t <- drop_get(dtoken = token,'/Public/test.csv')

# drop_delete('mtcars.csv')
write.csv(mtcars, 'mtcars.csv')
drop_upload('mtcars.csv')


# test
# drop_dir(dtoken = token, path = "/Public/research/data/HCUP/SID/California")
path = "/Public/research/data/HCUP/SID/California/"
file = "CA_SID_2009_CORE.CSV"

start.time <- Sys.time()
big.file<- data.table(drop_read_csv(dtoken = token,paste0(path,file))) # takes forever
# init.file<- drop_get(dtoken = token,paste0(path,file)) 
# use dropget, then fread.
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken