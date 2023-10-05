install.packages("xlsx")
install.packages("str2str")
install.packages("conflicted")

###------------------------------------------###
#---               loading        -----
###------------------------------------------###

#load the libraries
source("Libraries.R")

# load the function created in file: Functions.R:
source('Functions.R')
source('Matrix_function_dependencies.R')

###------------------------------------------###
#---            data preparation        -----
###------------------------------------------###

########               ########
        # first table # 
########               ########

#Load NDG table:
NDG_table <- read_excel("BCC.xlsx", sheet = "NDG")

#Basic modifications for column names and NAs removal:
NDG_table <- NDG_table %>% rename(Region=`Borrower's Region`, Tax_ID = 'Tax ID', Name = 'BorrowerName')
NDG_table$Group <- NDG_table$Group %>% gsub('-',NA,.)


#Remove multivalued cells, using pivot_longer function and create Borrowers_table:
separated_table <- NDG_table %>% 
  separate(Name,c('BorrowerName1','BorrowerName2','BorrowerName3','BorrowerName4'),sep = '[-,,]') %>% 
  separate(`Tax_ID`,c('TaxID1','TaxID2','TaxID3'),sep = '[-,,]')
Borrowers_table <-  separated_table %>% 
  pivot_longer(cols = matches('BorrowerN|Tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(BorrowerName|TaxID)(.*)",values_drop_na =  TRUE)

#After "pivoting", we need to reshape the data:
Borrowers_table$TaxID <- Borrowers_table$TaxID %>% str_trim(.,'left')
Borrowers_table$BorrowerName <- Borrowers_table$BorrowerName %>% str_trim(.,'left')
Borrowers_table <- Borrowers_table %>% 
  select(- taxID)  %>%
  rename(Name = BorrowerName, Tax_ID = TaxID)

#Create the primary key:
Borrowers_table <- Borrowers_table %>% mutate(Borrower_ID = sprintf('%03d',199 + row_number()) )


#Table with address information (Address_table_final) with its primary key:
Address_table <- NDG_table %>% select(Address, Town, City, Region)
Address_table <- Address_table %>%  mutate(Address_ID = sprintf('%03d',399 + row_number()) )
Address_table_final <- Address_table %>% select(Address_ID,Address, Town, City, Region )


# join Borrowers_table and Address_table_final:
Borrowers_table <- Borrowers_table %>% select(-Town,-City,-Region)
Borrowers_table_1 <- left_join(Borrowers_table,Address_table_final, by = ('Address'='Address'), copy=FALSE) %>% 
  select(-Address,-City,-Town,-Region)


#Final version of the Borrowers_table and NDG_table ordering the columns:
Borrowers_table_final <- Borrowers_table_1 %>% select(-Category) %>% 
  relocate(Group, .after = Address_ID) %>% 
  relocate(Borrower_ID, .before = NDG) %>% 
  relocate(NDG, .after = Group)

NDG_table_final <- Borrowers_table_1 %>% select(NDG,Category) %>% distinct()


#Apply the detect primary key function to the Borrower table:

possible_keys <- detect_primary_keys(Borrowers_table)
print(possible_keys)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Borrowers_table_final,number_NAs=1,percentage=0.9)
print(possible_keys_NAs_perc)


########               ########
        # second table # 
########               ########

# Load the Loans_table:
Loans <- read_excel("BCC.xlsx", sheet = "LOANS")
Loans_table <- Loans[-1, ] %>% row_to_names(1)


#Modify all the column names:
names(Loans_table) <- gsub('[.,/, ]', '_', names(Loans_table))
colnames(Loans_table) <- sapply(colnames(Loans_table), capitalize_after_underscore)
Loans_table <- Loans_table %>% rename(NDG = 'Ndg',Loans_ID = 'Id_Loans',
                                      Borrower_Name = 'Borrowername', Secured_Unsecured_NDG = 'Sec__Unsec_X_Ndg')
names(Loans_table) <- names(Loans_table) %>% gsub('Gbv','GBV',.)
colnames(Loans_table) <- sub("(_\\()?_Yes_No(\\))?", "", colnames(Loans_table))
colnames(Loans_table) <- sub("(_\\()yes_No(\\))?", "", colnames(Loans_table)) 
colnames(Loans_table) <- gsub("_+", "_", colnames(Loans_table))


#the dates are excel numbers, recuperate the real dates and create the UTP column: 
Loans_table$Database_Date <- excel_numeric_to_date(as.numeric(Loans_table$Database_Date))
UTP <- Loans_table$Default_Date %>% as.character()
Loans_table$Default_Date <- excel_numeric_to_date(as.numeric(Loans_table$Default_Date))
Loans_table <- cbind(UTP,Loans_table)
Loans_table$UTP <- if_else(Loans_table$UTP =='UTP','UTP','Bad')


#possible keys for the Loans_table:
possible_keys_loans <- detect_primary_keys(Loans_table)
print(possible_keys_loans)

possible_keys_NAs_perc <- detect_primary_keys_NAs_perc(Loans_table,number_NAs=1,percentage=0.4)
print(possible_keys_NAs_perc)


#Apply the 'check' function to the Loans_table to find the functional dependencies:
key <- 'NDG'

check_result <- check(Loans_table,key,FALSE)
length(check_result)

print_list(check_result)


#Apply the matrix function to the original Loans table:
Borrower_na <- Borrowers_table_final %>%  mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
Borrower_matrix <- find_dependencies_matrix(Borrower_na)
Borrower_matrix_rounded <- round(Borrower_matrix,2)

Loans_matrix <- find_dependencies_matrix(Loans_table)
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
num_rows <- nrow(Loans_table)  
NDG_Dependent <- data.frame(matrix(NA, nrow = num_rows, ncol = 0))
i <- 1
data <- Loans_matrix_rounded[c(2:40),]      #eliminate first column (sums)
for (j in 1:length(data$NDG)) {
  if (data$NDG[j] == 1) {
    new_column <- Loans_table[, j]
    col_name <- colnames(Loans_table)[j]
    NDG_Dependent[, col_name] <- new_column
    i <- i + 1
  }
}

#rearrange the data:
NDG_Dependent <- NDG_Dependent %>% relocate(NDG, .before = UTP)


# create new table from the remaining columns:
# Loans_ID dependent:
Loans_ID_Dependent <- Loans_table
for(col in names(NDG_Dependent)){
  Loans_ID_Dependent <- Loans_ID_Dependent %>% select(-col)
}
#Add the NDG column and inserting NAs:
Loans_ID_Dependent <- Loans_ID_Dependent %>% mutate(NDG = NDG_Dependent$NDG) %>% relocate(NDG, .after = Loans_ID )
Loans_ID_Dependent$Asset_Link <- Loans_ID_Dependent$Asset_Link %>% gsub("-", NA, .)


# work on NDG_Dependent:
NDG_Dependent <- NDG_Dependent %>% distinct()

# eliminate empty columns:
NDG_Dependent <- NDG_Dependent[,-c(7,9,10,17,18,20)]
NDG_Dependent <- NDG_Dependent %>% select(-Group, -Borrower_Name)
NDG_Dependent$Judicial_Procedures <- NDG_Dependent$Judicial_Procedures %>% gsub('/',',',.)
NDG_Dependent$Judicial_Procedures_Code <- NDG_Dependent$Judicial_Procedures_Code %>% gsub('[+]',',',.)

#pivot table NDG_Dependent:
NDG_Dependent <- NDG_Dependent %>% mutate(Judicial_Procedures = strsplit(as.character(Judicial_Procedures),",")) %>% unnest(Judicial_Procedures)
NDG_Dependent <- NDG_Dependent %>% mutate(Judicial_Procedures_Code = strsplit(as.character(Judicial_Procedures_Code),",")) %>% unnest(Judicial_Procedures_Code)

NDG_Dependent$Judicial_Procedures <- NDG_Dependent$Judicial_Procedures %>% str_trim(.,'left')
NDG_Dependent$Judicial_Procedures_Code <- NDG_Dependent$Judicial_Procedures_Code %>% str_trim(.,'left')



# create a Judicial table:
Judicial_table <- NDG_Dependent %>% select(NDG, Judicial_Procedures,Judicial_Procedures_Code)
Judicial_table <- Judicial_table %>% mutate(Link_ID = paste0('JP', 500 + row_number())) %>% 
  relocate(Link_ID, .before = NDG) %>% relocate(NDG, .after = Judicial_Procedures_Code)



# create NDG_Dependent_final:
# NDG_table_final Category = NDG_Dependent_final UTP
# so we can delete the NDG_table_final
NDG_Dependent_final <- NDG_Dependent %>% select(-Judicial_Procedures,-Judicial_Procedures_Code) %>% distinct()



# work on Loans_ID_Dependent table:
#Link_Consortium <- Loans_ID_Dependent[,c(1,16)] 
Consortium_table <- Loans_ID_Dependent[,c(1,16:20)] %>% filter(Consortium_Guarantee == 'YES')
Consortium_table <- Consortium_table %>% mutate(Consortium_ID = paste0('C', 300 + row_number()))
Consortium_table <- Consortium_table[,c(7,3:6,1)] 

Loans_ID_Dependent <- Loans_ID_Dependent[,-c(16:20)]
Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Consortium_table, by = ('Loans_ID'='Loans_ID'), copy=FALSE) 
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(17:20)]

Consortium_table <- Consortium_table[,-6]
# create Guarantors table:

Guarantors_table <- Loans_ID_Dependent %>% select(1,9,10,11,12,13)
Guarantors_table$Type_Of_Guarantee <- Guarantors_table$Type_Of_Guarantee %>% gsub('J','I',.)



# Groups of Guarantors:
Guarantors_groups <- Loans_ID_Dependent %>% select(12,13) %>% distinct() %>% na.omit()
Guarantors_groups <- Guarantors_groups %>% mutate(Group_ID = paste0('G', row_number()) )

Guarantors_table <- left_join(Guarantors_table,Guarantors_groups, by = ("Guarantors_Name" = "Guarantors_Name"), copy=FALSE)
Guarantors_table <- Guarantors_table[,-c(5,6,7)]

Guarantors_table <- Guarantors_table %>% na.omit(.)
Guarantors_table <- Guarantors_table %>% mutate(Loans_Group_ID = paste0("LG-", 550 + row_number()))

#pivot table Guarantors group:
separated_table_g <- Guarantors_groups %>% 
  separate(Guarantors_Name,c('Guarantors_Name1','Guarantors_Name2','Guarantors_Name3','Guarantors_Name4','Guarantors_Name5'),sep = ',') %>% 
  separate(Tax_Code_For_Guarantors,c('Tax_Code_For_Guarantors1','Tax_Code_For_Guarantors2','Tax_Code_For_Guarantors3','Tax_Code_For_Guarantors4','Tax_Code_For_Guarantors5'),sep = ',')
Guarantors_groups <-  separated_table_g %>% 
  pivot_longer(cols = matches('Guarantors|Tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(Guarantors_Name|Tax_Code_For_Guarantors)(.*)",values_drop_na =  TRUE)

Guarantors_groups <- Guarantors_groups %>% select(-taxID)
Guarantors_groups$Guarantors_Name <- Guarantors_groups$Guarantors_Name %>% str_trim(.,'left')
Guarantors_groups$Tax_Code_For_Guarantors <- Guarantors_groups$Tax_Code_For_Guarantors %>% str_trim(.,'left')

Guarantors_groups <- Guarantors_groups %>% mutate(Guarantor_ID = paste0('GG', 300 + row_number())) 
Guarantors_groups <- Guarantors_groups[,c(4,2,3,1)]



# delete the columns of Guarantors from Loans_ID_Dependent:
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(9:13)]
Loans_ID_Dependent$Total_GBV <- round(as.numeric(Loans_ID_Dependent$Total_GBV), 2)
Loans_ID_Dependent$GBV_Capital <- round(as.numeric(Loans_ID_Dependent$GBV_Capital), 2)
Loans_ID_Dependent$GBV_Interest <- round(as.numeric(Loans_ID_Dependent$GBV_Interest), 2)


# create Asset, Mortgage, and Guarantors tables:
Asset_table <- Loans_ID_Dependent[,c(1,3,10)] %>% na.omit(.)

Type_Mortgage_table <- Loans_ID_Dependent %>% select(Type_Of_Mortgage ) %>% na.omit(.) %>% distinct()
Type_Mortgage_table <- Type_Mortgage_table %>% mutate(Type_ID = paste0('ToM-0',  + row_number())) 

Asset_table <- left_join(Asset_table,Type_Mortgage_table, by = ("Type_Of_Mortgage"="Type_Of_Mortgage") )
Asset_table <- Asset_table %>% select(-3)


Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Type_Mortgage_table, by = ("Type_Of_Mortgage"="Type_Of_Mortgage") )
Loans_ID_Dependent <- Loans_ID_Dependent[,-c(3,9,10)]

Guarantors_Link <- Guarantors_groups %>% select(1,4)
Guarantors_groups <- Guarantors_groups[,-4]

Loans_ID_Dependent <- left_join(Loans_ID_Dependent,Guarantors_table %>% 
                                  select(Loans_ID, Loans_Group_ID),by = c("Loans_ID" = "Loans_ID"))

Guarantors_table <- Guarantors_table[,c(6,3:5)]