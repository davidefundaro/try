###------------------------------------------###
#---               loading        -----
###------------------------------------------###

#load the libraries
source("Libraries.R")

# load the function created in file: Functions.R:

source('Functions_updated.R')
source("empty_column&tolower_functions.R")

###------------------------------------------###
#---            data preparation        -----
###------------------------------------------###

########               ########
# first table # 
########               ########

#Load NDG table:
#NDG_table <- read_excel("Data/BCC.xlsx", sheet = "NDG")
NDG_table <- read_excel("C:/Users/eleonora.zarrilli/Documents/CORSI/Dati R/esercizi/Normalizzazione_git_R/DATA/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "NDG")
names(NDG_table) <- colname_function(names(NDG_table))

#Basic modifications for column names and NAs removal:
NDG_table <- NDG_table %>% rename(region=`borrower's.region`, name = 'borrowername')
NDG_table$group <- NDG_table$group %>% gsub('-',NA,.)

#Remove multivalued cells, using pivot_longer function and create Borrowers_table:
separated_table <- NDG_table %>% 
  separate(name,c('name1','name2','name3','name4'),sep = '[-,,]') %>% 
  separate(`tax.id`,c('tax.id1','tax.id2','tax.id3'),sep = '[-,,]')
Borrowers_table <-  separated_table %>% 
  pivot_longer(cols = matches('name|tax.id'),
               names_to = c(".value", "taxID"),
               names_pattern = "(name|tax.id)(.*)",values_drop_na =  TRUE)

#After "pivoting", we need to reshape the data:
Borrowers_table$`tax.id` <- Borrowers_table$`tax.id` %>% str_trim(.,'left')
Borrowers_table$name<- Borrowers_table$name %>% str_trim(.,'left')
Borrowers_table <- Borrowers_table %>% 
  select(- taxID)

#Create the primary key:
Borrowers_table <- Borrowers_table %>% mutate(id.bor = sprintf('%03d',199 + row_number()) )

#Table with address information (Address_table_final) with its primary key:
Address_table <- NDG_table %>% select(address, town, city, region)
Address_table <- Address_table %>%  mutate(id.address = sprintf('%03d',399 + row_number()) )
Address_table_final <- Address_table %>% select(id.address, address, town, city, region )


# join Borrowers_table and Address_table_final:
Borrowers_table <- Borrowers_table %>% select(-town,-city,-region)
Borrowers_table_1 <- left_join(Borrowers_table,Address_table_final, by = ('address'='address'), copy=FALSE) %>% 
  select(-address,-city,-town,-region)


#Final version of the Borrowers_table and NDG_table ordering the columns:
Borrowers_table_final <- Borrowers_table_1 %>% select(-category) %>% 
  relocate(group, .after = id.address) %>% 
  relocate(id.bor, .before = ndg) %>% 
  relocate(ndg, .after = group)

NDG_table_final <- Borrowers_table_1 %>% select(ndg,category) %>% distinct()

#Apply the detect primary key function to the Borrower table:

possible_keys <- detect_primary_keys(Borrowers_table)
print(possible_keys)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Borrowers_table_final,number_NAs=1,percentage=0.9)
print(possible_keys_NAs_perc)


########               ########
       # second table # 
########               ########

# Load the Loans:
#Loans_Raw <- read_excel("Data/BCC.xlsx", sheet = "LOANS")
Loans_Raw <- read_excel("C:/Users/eleonora.zarrilli/Documents/CORSI/Dati R/esercizi/Normalizzazione_git_R/DATA/DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = "LOANS")
Loans <- Loans_Raw[-1, ] %>% row_to_names(1)

names(Loans) <- colname_function(names(Loans))



Loans <- Loans %>% rename("id.loan" = 'id.loans', "type" = "type.of.credit", 
                          "gbv.original" = "total.gbv", "principal" = "gbv.capital", 
                          "interest" = "gbv.interest", "expenses" = "gbv.expenses",
                          "gbv.residual" = "residual.position", "type.debt.ndg" = 'sec..unsec.x.ndg',
                          "date.status" = "default.date", name = borrowername, "type.debt" = 'secured.unsecured',
                          "amount.guarantee" = "guarantee.amount", "consortium.guarantee" = `consortium.guarantee.(yes.no)`,
                          "consortium.guarantee.amount" = `consort..guarantee.amount.(importo.garantito)`, 
                          mortgage = "mortgage.yes.no")





#the dates are excel numbers, recuperate the real dates and create the UTP column: 
Loans$database.date <- excel_numeric_to_date(as.numeric(Loans$database.date))
status.debt <- Loans$date.status %>% as.character()
Loans$date.status <- excel_numeric_to_date(as.numeric(Loans$date.status))
Loans <- cbind(status.debt,Loans)
Loans$status.debt <- if_else(Loans$status.debt =='UTP','UTP','Bad')

#possible keys for the Loans:
possible_keys_loans <- detect_primary_keys(Loans)
print(possible_keys_loans)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Loans,number_NAs=1,percentage=0.4)
print(possible_keys_NAs_perc)


#Apply the 'check' function to the Loans to find the functional dependencies:
key <- 'ndg'

check_result <- check(Loans,key,FALSE)
length(check_result)

print_list(check_result)


#Apply the matrix function to the original Loans table:
Borrower_na <- Borrowers_table_final %>%  mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
Borrower_matrix <- find_dependencies_matrix(Borrower_na)
Borrower_matrix_rounded <- round(Borrower_matrix,2)

Loans_matrix <- find_dependencies_matrix(Loans)
Loans_matrix_rounded <- round(Loans_matrix, 2)


# add a row with the sum of 1 (dependencies) for each column and order the matrix for better visualization:
Loans_matrix_rounded  <- Loans_matrix_rounded %>% as.data.frame()
column_sums_ones <- sapply(Loans_matrix_rounded, function(col) sum(col == 1))

Loans_matrix_rounded <- rbind(Loans_matrix_rounded, column_sums_ones) 
rownames(Loans_matrix_rounded)[40] <- 'Sum_ones'
Loans_matrix_rounded <- Loans_matrix_rounded[c(40, 1:39), ]

Loans_matrix_ordered <- as.matrix(Loans_matrix_rounded)
col_index <- order(Loans_matrix_ordered[1,],  decreasing = TRUE)
Loans_matrix_ordered <- Loans_matrix_ordered[,col_index]

#create two tables, one NDG dependent and the other Loans_ID dependent:

# NDG dependent:
#create a empty dataframe with the same number of rows as LoansTable:
num_rows <- nrow(Loans)  
NDG_Dependent <- data.frame(matrix(NA, nrow = num_rows, ncol = 0))
i <- 1
data <- Loans_matrix_rounded[c(2:40),]      #eliminate first column (sums)
for (j in 1:length(data$ndg)) {
  if (data$ndg[j] == 1) {
    new_column <- Loans[, j]
    col_name <- colnames(Loans)[j]
    NDG_Dependent[, col_name] <- new_column
    i <- i + 1
  }
}

#rearrange the data:
NDG_Dependent <- NDG_Dependent %>% relocate(ndg, .before = status.debt)

# create new table from the remaining columns:
# Loans_ID dependent:
Loans_ID_Dependent <- Loans
for(col in names(NDG_Dependent)){
  Loans_ID_Dependent <- Loans_ID_Dependent %>% select(-col)
}
#Add the NDG column and inserting NAs:
Loans_ID_Dependent <- Loans_ID_Dependent %>% mutate(ndg = NDG_Dependent$ndg) %>% relocate(ndg, .after = id.loan )
Loans_ID_Dependent$asset.link <- Loans_ID_Dependent$asset.link %>% gsub("-", NA, .)

# work on NDG_Dependent:
NDG_Dependent <- NDG_Dependent %>% distinct()

# eliminate empty columns:
NDG_Dependent <- NDG_Dependent[,-c(7,9,10,17,18,20)]
NDG_Dependent <- NDG_Dependent %>% select(-group, -name)
NDG_Dependent$judicial.procedures <- NDG_Dependent$judicial.procedures %>% gsub('/',',',.)
NDG_Dependent$judicial.procedures.code <- NDG_Dependent$judicial.procedures.code %>% gsub('[+]',',',.)

#pivot table NDG_Dependent:
NDG_Dependent <- NDG_Dependent %>% mutate(judicial.procedures = strsplit(as.character(judicial.procedures),",")) %>% unnest(judicial.procedures)
NDG_Dependent <- NDG_Dependent %>% mutate(judicial.procedures.code = strsplit(as.character(judicial.procedures.code),",")) %>% unnest(judicial.procedures.code)

NDG_Dependent$judicial.procedures <- NDG_Dependent$judicial.procedures %>% str_trim(.,'left')
NDG_Dependent$judicial.procedures.code <- NDG_Dependent$judicial.procedures.code %>% str_trim(.,'left')



# create a Judicial table:
Judicial_table <- NDG_Dependent %>% select(ndg, judicial.procedures,judicial.procedures.code)
Judicial_table <- Judicial_table %>% mutate(id.link = paste0('JP', 500 + row_number())) %>% 
  relocate(id.link, .before = ndg) %>% relocate(ndg, .after = judicial.procedures.code)






# create NDG_Dependent_final:
# NDG_table_final Category = NDG_Dependent_final UTP
# so we can delete the NDG_table_final
NDG_Dependent_final <- NDG_Dependent %>% select(-judicial.procedures,-judicial.procedures.code) %>% distinct()



# work on Loans_ID_Dependent table:
#Link_Consortium <- Loans_ID_Dependent[,c(1,16)] 
Consortium_table <- Loans_ID_Dependent[,c(1,16:20)] %>% filter(consortium.guarantee == 'YES')
Consortium_table <- Consortium_table %>% mutate(id.consortium = paste0('C', 300 + row_number()))
Consortium_table <- Consortium_table[,c(7,3:6,1)] 

Loans_ID_Dependent <- Loans_ID_Dependent[,-c(16:20)]
Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Consortium_table, by = ('id.loan'='id.loan'), copy=FALSE) 
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(17:20)]

Consortium_table <- Consortium_table[,-6]
# create Guarantors table:

Guarantors_table <- Loans_ID_Dependent %>% select(1,9,10,11,12,13)
Guarantors_table$type.of.guarantee <- Guarantors_table$type.of.guarantee %>% gsub('J','I',.)



# Groups of Guarantors:
Guarantors_groups <- Loans_ID_Dependent %>% select(12,13) %>% distinct() %>% na.omit()
Guarantors_groups <- Guarantors_groups %>% mutate(id.group = paste0('G', row_number()) )

Guarantors_table <- left_join(Guarantors_table,Guarantors_groups, by = ("guarantors.name" = "guarantors.name"), copy=FALSE)
Guarantors_table <- Guarantors_table[,-c(5,6,7)]

Guarantors_table <- Guarantors_table %>% na.omit(.)
Guarantors_table <- Guarantors_table %>% mutate(id.loan.group = paste0("LG_", 550 + row_number()))

#pivot table Guarantors group:
separated_table_g <- Guarantors_groups %>% 
  separate(guarantors.name,c('guarantors.name1','guarantors.name2','guarantors.name3','guarantors.name4','guarantors.name5'),sep = ',') %>% 
  separate(tax.code.for.guarantors,c('tax.code.for.guarantors1','tax.code.for.guarantors2','tax.code.for.guarantors3','tax.code.for.guarantors4','tax.code.for.guarantors5'),sep = ',')
Guarantors_groups <-  separated_table_g %>% 
  pivot_longer(cols = matches('guarantors|tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(guarantors.name|tax.code.for.guarantors)(.*)",values_drop_na =  TRUE)

Guarantors_groups <- Guarantors_groups %>% select(-taxID)
Guarantors_groups$guarantors.name <- Guarantors_groups$guarantors.name %>% str_trim(.,'left')
Guarantors_groups$tax.code.for.guarantors <- Guarantors_groups$tax.code.for.guarantors %>% str_trim(.,'left')

Guarantors_groups <- Guarantors_groups %>% mutate(id.guarantor = paste0('GG', 300 + row_number())) 
Guarantors_groups <- Guarantors_groups[,c(4,2,3,1)]



# delete the columns of Guarantors from Loans_ID_Dependent:
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(9:13)]
Loans_ID_Dependent$gbv.original <- round(as.numeric(Loans_ID_Dependent$gbv.original), 2)
Loans_ID_Dependent$principal <- round(as.numeric(Loans_ID_Dependent$principal), 2)
Loans_ID_Dependent$interest <- round(as.numeric(Loans_ID_Dependent$interest), 2)


# create Asset, Mortgage, and Guarantors tables:
Asset_table <- Loans_ID_Dependent[,c(1,3,10)] %>% na.omit(.)

Type_Mortgage_table <- Loans_ID_Dependent %>% select(type.of.mortgage) %>% na.omit(.) %>% distinct()
Type_Mortgage_table <- Type_Mortgage_table %>% mutate(id.type = paste0('ToM_0',  + row_number())) 

Asset_table <- left_join(Asset_table,Type_Mortgage_table, by = ("type.of.mortgage"="type.of.mortgage") )
Asset_table <- Asset_table %>% select(-3)


Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Type_Mortgage_table, by = ("type.of.mortgage"="type.of.mortgage") )
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(3,9,10)]

Guarantors_Link <- Guarantors_groups %>% select(1,4)
Guarantors_groups <- Guarantors_groups[,-4]

Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Guarantors_table %>% 
                                  select(id.loan, id.loan.group),by = c("id.loan" = "id.loan"))

Guarantors_table <- Guarantors_table[,c(6,3:5)]
