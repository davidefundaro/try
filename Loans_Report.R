###------------------------------------------###
#---              Loan type        -----
###------------------------------------------###

source("Libraries.R")
source("empty_column&tolower_functions.R")
source("count_errors.R")

###------------------------------------------###
#---               load the data        -----
###------------------------------------------###
#Loans_Raw <- read_excel("C:/Users/eleonora.zarrilli/Documents/CORSI/Dati R/esercizi/Normalizzazione_git_R/DATA/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "LOANS")
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


#create a new dataset for the Loans report
Loan_Report <- Loans %>% select(id.loan, type)

#set the factors and apply the change value function
df <- Loan_Report
col<- "type"
pattern_replacement <- list("corrente" = "Bank Accounts", "mutuo ipotecario" = "Mortgages", 
                            "credito" = "Other", "mutuo chiro" = "Personal Loans", "fondiario" = "mortgages fondiario")
df <- change_Value_If_Pattern_Found(df, col, pattern_replacement)

#Loans report ready
Loans_Report <- df %>% group_by(type) %>% summarize(count= n())

#create a new dataset for the GBV report, create a rowname column and reorder the columns
GBV <- Loans %>% select(gbv.original, gbv.residual, principal, interest)

GBV_Report <- data.frame(
  sum = colSums(GBV),
  min = sapply(GBV, min),
  max = sapply(GBV, max)
)

GBV_Report$gbv <- row.names(GBV_Report)

GBV_Report <- GBV_Report[,c(4,1:3)]


l <- list(Loans_Report, GBV_Report)
openxlsx::write.xlsx(l, file = "Loans_Type_GBV_Report.xlsx")
