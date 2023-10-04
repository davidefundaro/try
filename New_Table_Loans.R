

library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(str2str)
library(conflicted)
library(rlang)


source('Functions.R')

Loans_Raw <- read_excel("Data/BCC.xlsx", sheet = "LOANS")
Loans_table <- Loans_Raw[-1, ] %>% row_to_names(1)


Loans <- Loans_table %>% select(c(1,3,6,8,10,12:15))



colname_function <- function(name) {
  name <- gsub('[/, ]', '.', name)
  name <- tolower(name)
  return(name)
}

names(Loans) <- colname_function(names(Loans))

Loans <- Loans %>% rename("id.bor" = 'ndg', "id.loan" = 'id.loans', "type" = "type.of.credit", 
                          "gbv.original" = "total.gbv", "principal" = "gbv.capital", "interest" = "gbv.interest", "expenses" = "gbv.expenses",
                          "gbv.residual" = "residual.position")


status <- Loans$default.date %>% as.character()
Loans$default.date <- excel_numeric_to_date(as.numeric(Loans$default.date)) # NAs introduced by coercion
Loans<- cbind(status,Loans)
Loans$status <- if_else(Loans$status =='UTP','UTP','Bad')

Loans$gbv.residual <- Loans$gbv.residual %>% gsub('-','0',.) %>% as.numeric()


Loans$principal <- Loans$principal %>% as.numeric() %>% round(., 2)
Loans$interest <- Loans$interest %>% as.numeric() %>% round(., 2)
Loans$gbv.original <- Loans$gbv.original %>% as.numeric() %>% round(., 2)
Loans$expenses <- Loans$expenses %>% as.numeric()
#check
#summary(Loans)

Loans$id.group <- NA
Loans$originator <- NA
Loans$ptf <- NA
Loans$cluster.ptf <- NA
Loans$penalties <- NA
Loans$date.origination <- NA
Loans$date.origination <- Loans$date.origination %>% as.Date()
Loans$date.last.act <- NA
Loans$date.last.act <- Loans$date.last.act %>% as.Date()
Loans$flag.imputed <- NA
Loans$flag.imputed <- Loans$flag.imputed %>% as.integer()

Loans <- Loans %>% rename("date.status" = "default.date")
Loans <- Loans[,c(3,2,11:14,5,1,10,6:8,15,9,16,4,17,18)]



#########################################################################################################################
#Modify all the column names:
names(Loans_table) <- gsub('[/, ]', '.', names(Loans_table))
Loans_table <- Loans_table %>% rename("id.bor" = 'NDG',"id.loan" = 'ID.Loans', "type" = "Type.of.Credit", )
names(Loans_table) <- names(Loans_table) %>% gsub('Gbv','gbv',.)
colnames(Loans_table) <- gsub("_+", ".", colnames(Loans_table))


#the dates are excel numbers, recuperate the real dates and create the UTP column: 
Loans_table$Database.Date <- excel_numeric_to_date(as.numeric(Loans_table$Database_Date))
status <- Loans_table$Default_Date %>% as.character()
Loans_table$Default_Date <- excel_numeric_to_date(as.numeric(Loans_table$Default_Date)) # NAs introduced by coercion
Loans_table <- cbind(status,Loans_table)
Loans_table$date.status <- if_else(Loans_table$status =='UTP','UTP','Bad')

#############################################################################################################################



