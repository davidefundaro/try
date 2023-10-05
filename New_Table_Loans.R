library(tidyr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(str2str)
library(conflicted)
library(rlang)

source("empty_column&tolower_functions.R")

###------------------------------------------###
#---               load the data        -----
###------------------------------------------###
Loans_Raw <- read_excel("Data/BCC.xlsx", sheet = "LOANS")
Loans_table <- Loans_Raw[-1, ] %>% row_to_names(1)

###------------------------------------------###
#---               data preparation       -----
###------------------------------------------###
Loans <- Loans_table %>% select(c(1,3,6,8,10,12:15))

names(Loans) <- colname_function(names(Loans))

Loans <- Loans %>% rename("id.bor" = 'ndg', "id.loan" = 'id.loans', "type" = "type.of.credit", 
                          "gbv.original" = "total.gbv", "principal" = "gbv.capital", 
                          "interest" = "gbv.interest", "expenses" = "gbv.expenses",
                          "gbv.residual" = "residual.position")
#create the status column (UTP, Bad)
status <- Loans$default.date %>% as.character()
Loans<- cbind(status,Loans)
Loans$status <- if_else(Loans$status =='UTP','UTP','Bad')

Loans$default.date <- excel_numeric_to_date(as.numeric(Loans$default.date)) # NAs introduced by coercion

Loans$gbv.residual <- Loans$gbv.residual %>% gsub('-','0',.) %>% as.numeric()

#turn as.numeric and round numeric values
vector_column <- Loans %>% select(principal, interest, gbv.original, expenses) %>% names()
Loans <- Loans %>% mutate_at(vars(all_of(vector_column)), funs(round(as.numeric(.), 2)))

#create the empty columns
vector_empty <- c("id.group","originator","ptf","cluster.ptf", "penalties",  "date.origination","date.last.act","flag.imputed")
Loans <- create_empty_columns(Loans, vector_empty)

#change column format
Loans$date.origination <- Loans$date.origination %>% as.Date()
Loans$date.last.act <- Loans$date.last.act %>% as.Date()
Loans$flag.imputed <- Loans$flag.imputed %>% as.integer()

#rename date column and rearrange
Loans <- Loans %>% rename("date.status" = "default.date")
Loans <- Loans[,c(3,2,11:14,5,1,10,6:8,15,9,16,4,17,18)]