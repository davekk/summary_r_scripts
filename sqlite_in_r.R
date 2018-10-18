#creating a basic db in R
getwd()
setwd("~/module2_R_biostats-master/Abu_data/")
install.packages(c("dbplyr", "RSQLite"))

library(dplyr)
library(dbplyr)

# reading the csv file into r
abu_d <- read.csv("clean_abu.csv",header = T)

#  create a db file

# use sqlite to create empty db

my_db <- src_sqlite("portal-database.sqlite", create = TRUE)
# copy df into db 
copy_to(my_db, abu_d)

# check if we can query the new sql db
tbl(my_db, sql("SELECT Year, Location FROM abu_d"))

## 