source("Libraries.R")
source("empty_column&tolower_functions.R")
source("count_errors.R")

Loans_Raw <- read_excel("Data/BCC.xlsx", sheet = "LOANS")
Loans_table <- Loans_Raw[-1, ] %>% row_to_names(1)

Loans <- Loans_table %>% select(c(1,3,6:8,10,12:15,19,20))

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


############### COUNTERPARTIES


#create counterparties table
People <- Loans %>% select(id.loan, borrowername, guarantors.name)
People$borrowername <- People$borrowername %>% gsub("-", ",", .) %>% tolower() 
People$guarantors.name <- tolower(People$guarantors.name) 

People_1 <- People %>% select(borrowername) %>% rename("name" = "borrowername")
People_1$role <- "Borrower"
People_2 <- People %>% select(guarantors.name) %>% rename ("name" = "guarantors.name")
People_2$role <- "Guarantor"

Counterparties <- rbind(People_1, People_2)
Counterparties<-Counterparties[complete.cases(Counterparties),]
Counterparties <- Counterparties %>% distinct()

Counterparties <- Counterparties %>%
  mutate(n.entities = str_count(name, ","))
Counterparties$n.entities <- Counterparties$n.entities + 1

#create an id code for counterparties
Counterparties <- Counterparties %>% mutate(id.counterparties = paste0('CP_', 500 + row_number())) %>% 
  relocate(id.counterparties, .before = name)

People_1 <- People %>% select(id.loan, borrowername) %>% rename("name" = "borrowername")
People_1$role <- "Borrower"
People_2 <- People %>% select(id.loan, guarantors.name) %>% rename ("name" = "guarantors.name")
People_2$role <- "Guarantor"

Counterparties_Link <- rbind(People_1, People_2)
Counterparties_Link<-Counterparties_Link[complete.cases(Counterparties_Link),]
Counterparties_Link<- merge(Counterparties_Link, Counterparties[,c('name', 'id.counterparties')], by= 'name', all.x= TRUE)
Counterparties_Link <- Counterparties_Link %>% select(-c(name, role))


#create the empty columns
vector_empty <- c("id.bor", "id.group", "flag.imputed")
Counterparties <- create_empty_columns(Counterparties, vector_empty)



################################ ENTITIES and COUNTERPARTIES_ENTITIES_LINK



#create the entities table from the counterparties table, split the rows and delete spaces
Entities <- Counterparties %>% select(id.counterparties, name)

Entities <- Entities %>% mutate(name = strsplit(as.character(name), ",")) %>% 
  unnest(name)

Entities$name <- Entities$name %>% str_trim(side = "both")


#link table for counterparties and entities
Entities_Counterparties_LInk <- Entities %>% mutate(id.entities = paste0('EN_', 200 + row_number()))
Entities_Counterparties_LInk <- Entities_Counterparties_LInk %>% select(-name)


#load the ndg table in order to get the information about address and tax id

ndg <- read_excel("Data/BCC.xlsx", sheet = "NDG")

#convert - into ,
X <- as.data.frame(gsub("[-]", ",", as.matrix(ndg)))

#create a column for address id and set it as numeric
X$ID_Address <- seq(nrow(X))
X$ID_Address <- X$ID_Address %>% as.numeric()

#separate rows by , for BorrowsName and place the problematic rows as last
X2 <- X %>% mutate(BorrowerName = strsplit(as.character(BorrowerName), ",")) %>% 
  unnest(BorrowerName) %>% slice(c(1:2, 7:n(), 3:6))
X2 <-  X2 %>% select(BorrowerName)

#separate rows by , for Tax ID and place the problematic rows (multinamed) as last
X3 <- X %>% mutate(`Tax ID` = strsplit(as.character(`Tax ID`), ",")) %>%
  unnest(`Tax ID`) %>% slice(c(1:2, 6:n(), 3:5))
X3 <-  X3 %>% select(`Tax ID`)

#unify X2 (names) with X3 (tax id)
X_name_id <- merge(X2, X3, by = "row.names", all = TRUE)
X_name_id <- X_name_id %>% select(-`Row.names`)

#split names in X
X <- X %>% mutate(BorrowerName = strsplit(as.character(BorrowerName), ",")) %>% 
  unnest(BorrowerName)

#eliminate tax id in X
X <- X %>% select(-`Tax ID`)

#merge name-id dataset with X
X_fine <- merge(X, X_name_id, by = "BorrowerName", all = TRUE)
X <- X_fine


#eliminate commas
X <- as.data.frame(gsub("[,]", "", as.matrix(X)))

#convert empty spaces in NA
X[X == ""] <- NA

#delete spaces at the beginning of the BorrowerName and Tax ID
X$BorrowerName <- X$BorrowerName %>% str_trim(side = "both")
X$`Tax ID` <- X$`Tax ID` %>% str_trim(side = "both")

NDG_table <- X

#give new names to columns select and set as lowercase
NDG_table<- NDG_table %>% rename(name = BorrowerName,  cf.piva= `Tax ID`, city = Town, 
                                 province = City, region = `Borrower's Region`)


NDG_table <- NDG_table %>% select(name, cf.piva, city, province, region)
NDG_table$name <- tolower(NDG_table$name)


Entities_table <- Entities %>% left_join(NDG_table, by = ("name" = "name"))



Loans_for_entities <- Loans_table
names(Loans_for_entities) <- colname_function(names(Loans_for_entities))
Loans_for_entities <- Loans_for_entities %>% rename("name" = "guarantors.name", "cf.piva" = "tax.code.for.guarantors")

Loans_for_entities <- Loans_for_entities %>% select(name, cf.piva)

Loans_for_entities$name <- tolower(Loans_for_entities$name) 



#separate rows by , for BorrowsName and place the problematic rows as last
X2 <- Loans_for_entities %>% mutate(name = strsplit(as.character(name), ",")) %>% 
  unnest(name)
X2 <-  X2 %>% select(name)

#separate rows by , for Tax ID and place the problematic rows (multinamed) as last
X3 <- Loans_for_entities %>% mutate(cf.piva = strsplit(as.character(cf.piva), ",")) %>%
  unnest(cf.piva)
X3 <-  X3 %>% select(cf.piva)

#unify X2 (names) with X3 (tax id)
X_name_id <- merge(X2, X3, by = "row.names", all = TRUE)
X_name_id <- X_name_id %>% select(-`Row.names`)

X_name_id$name <- X_name_id$name %>% str_trim(side = "both")
X_name_id$cf.piva <- X_name_id$cf.piva %>% str_trim(side = "both")

#X_name_id<-X_name_id[complete.cases(X_name_id),]
X_name_id <- X_name_id %>% distinct()

#merge the names and ids from the ndg and the loans table to create the entities table and clean
Entities_table <- Entities_table %>% left_join(X_name_id, by = ("name" = "name"))
Entities_table[is.na(Entities_table)] <- ""
Entities_table$cf.piva <- paste(Entities_table$cf.piva.x, Entities_table$cf.piva.y)
Entities_table$cf.piva <- Entities_table$cf.piva %>% str_trim(side = "both")
Entities_table<- Entities_table %>% select(-c(cf.piva.x, cf.piva.y))
Entities <- Entities_table

#determine if the entity is corporate or individual according to the cf.piva
Entities <- Entities %>%
  mutate(type.subject = ifelse(grepl("^[0-9]+$", cf.piva), 'Corporate', NA))
Entities$type.subject <- ifelse(is.na(Entities$type.subject), 'Individual', Entities$type.subject)

#function to extract the month from fiscal code
df <- Entities
df$cf.piva <- as.character(df$cf.piva)

extract_digit <- function(df, new_col, type, position_1, position_2) {
  df[[new_col]] <- NA
  for (i in 1:nrow(df)) {
    if (df[[type]][i] == 'Individual') {
      df[[new_col]][i] <- substr(df$cf.piva[i], position_1, position_2)
    }
  }
  
  # Return the updated dataframe outside the loop
  return(df)
}

position_1<- 9
position_2<- 9
new_col <- "month"
type <- "type.subject"
df <- extract_digit(df, new_col, type, position_1, position_2)

#since the month is represented by a letter, this code will convert the letter into the month number
col<- "month"
pattern_replacement <- list("A" = "01", "B" = "02", "C" = "03", "D" = "04", "E" = "05", "H" = "06",
                            "L" = "07", "M" = "08", "P" = "09", "R" = "10", "S" = "11", "T" = "12")
df <- change_Value_If_Pattern_Found(df, col, pattern_replacement)
df$month<- as.numeric(df$month)

#extract the year
position_1<- 7
position_2<- 8
new_col <- "year"
type <- "type.subject"
df <- extract_digit(df, new_col, type, position_1, position_2)
df$year<- as.numeric(df$year)

#extract the day
position_1<- 10
position_2<- 11
new_col <- "day"
type <- "type.subject"
df <- extract_digit(df, new_col, type, position_1, position_2)
df$day<- as.numeric(df$day)

#determine the sex from fiscal code
check_sex <- function(day) {
  if (!is.na(day) && day >= 32) {
    return("F")  
  } else {
    return("M") 
  }
}

# Apply the function to create the new "sex" column
df$sex <- sapply(df$day, check_sex)

#clean the wrong sex applied to corporate
eliminate_wrong_sex <- function(type.subject, sex) {
  return(ifelse(type.subject == 'Corporate', NA, sex))
}

# Apply the function using dplyr's mutate
df <- df %>%
  mutate(sex = eliminate_wrong_sex(type.subject, sex))



#convert the birth day of females
df$day <- df$day %>% as.numeric()

for (i in 1:nrow(df)) {
  if (!is.na(df$sex[i]) && df$sex[i] == "F") {
    df$day[i] <- df$day[i] - 40
  }
}

for (i in 1:nrow(df)) {
  if (!is.na(df$year[i])) {
    if (df$year[i] > 5) {
      df$year[i] <- df$year[i] + 1900
    } else {
      df$year[i] <- df$year[i] + 2000
    }
  }
}

df$day<- as.numeric(df$day)
df$month<- as.numeric(df$month)
df$year<- as.numeric(df$year)
df$date <- as.Date(paste(df$year, df$month, df$day, sep = "-"), format = "%Y-%m-%d")

current_date <- Sys.Date()  # Get the current date
df$age <- as.integer(difftime(current_date, df$date, units = "days") / 365.25)

age_ranges <- c(0, 25, 50, 65, 75, Inf)  # Define the age ranges
age_labels <- c("0-25", "25-50", "50-65", "65-75", "75+")  # Define labels for the ranges

# Create a new column 'age_group' with the age ranges
df$range.age <- cut(df$age, breaks = age_ranges, labels = age_labels, include.lowest = TRUE)

#eliminate the columns no more useful
df <- df %>% select(-c(month,day,year,date))

df <- df %>%
  mutate(city = str_to_title(city))
df <- df %>%
  mutate(province = str_to_title(province))
df <- df %>%
  mutate(region = str_to_title(region))

#classify the regions and find the area
classify_region <- function(region_name) {
  northern_regions_west <- c("Lombardia", "Valle D'Aosta", "Liguria",
                        "Piemonte") # Add more northern regions as needed
  northern_regions_east<- c("Veneto", "Trentino Alto Adige", "Emilia Romagna", "Friuli Venezia Giulia")
  southern_regions <- c("Puglia", "Calabria", "Basilicata", "Molise", "Campania", "Abbruzzo") # Add more southern regions as needed
  central_regions <- c("Toscana", "Lazio", "Marche", "Umbria")
  islands <- c("Sardegna", "Sicilia")
  # Add more central regions as needed
  
  if (region_name %in% northern_regions_west) {
    return("North-West")
  } else if (region_name %in% northern_regions_east) {
    return("North-East")
  } else if (region_name %in% central_regions) {
    return("Center")
  } else if (region_name %in% southern_regions) {
    return("South")
  } else if (region_name %in% islands) {
    return("Islands")
  } else {
      return("Abroad")
    }
  }

df <- df %>%
  mutate(area = sapply(region, classify_region))

#if the region is not provided, set area as NA
df$area <- ifelse(df$region == "", NA, df$area)
#set all remaining empty cells as NA
df[df == ""] <- NA

#create the empty columns
vector_empty <- c("dummy.info", "solvency.pf", "income.pf", "status.pg", "date.cessasion", "flag.imputed")
df <- create_empty_columns(df, vector_empty)

check_type <- function(text) {
  if (type.subject == "Corporate") {
    if (grepl("societa' semplice|ss|s.s")) {
      return("ss")
    } else {
      if (grepl("srl|s.r.l|srls")) {
        return("srl")
      } else {
        if(grepl("spa|s.p.a")) {
          return("spa")
        } else {
          if (grepl("snc|s.n.c"))
        }
      }
    }
    
  }
}
#check if the entities are individuals or corporate
#check_pattern <- function(text) {
#if (grepl("societa' semplice|srl|spa|ss|snc|sas|sapa|saa|soccoop|s.r.l|s.p.a|s.s|s.n.c|s.a.s|s.a.p.a|s.a.a|s.o.c.c.o.o.p", text))
#  return("Corporate")
#  else
#    return("Individual")
#}

# Apply the function to create the new column
#Entities$type.subject <- sapply(Entities$name, check_pattern)


############################ REPORTS ##################################


Counterparties$n.entities <- as.numeric(Counterparties$n.entities)

GBV_total <- Loans %>% summarize(GBV_total = sum(gbv.original))

Loans_total <- Loans %>% summarize(Loans_total = sum(nrow(Loans)))

Borrowers_total <- Counterparties %>% filter(role == "Borrower") %>%
  summarize(Borrowers_total = n_distinct(name))

Totals <- cbind(Borrowers_total, Loans_total, GBV_total)






Borrower_guarantor_table <- merge(Counterparties_Link, Counterparties[,c('id.counterparties', 'role')], by= 'id.counterparties', all.x= TRUE)

Borrowers_filtered <- Borrower_guarantor_table %>% filter(role == "Borrower")
Guarantors_filtered <- Borrower_guarantor_table %>% filter(role == "Guarantor")



############################# CARICA LOANS TABLE #################################### 



#merge data in order to find the borrowers with or without guarantors
merged <- Loans %>% merge(Borrowers_filtered[,c('id.loan', 'id.counterparties', 'role')], 
                          by= 'id.loan', all.x= TRUE) %>%  merge(Guarantors_filtered[,c('id.loan', 'id.counterparties',
                                                                                       'role')], 
                                                                 by= 'id.loan', all.x= TRUE)



result <- merged %>% 
  group_by(role.y) %>%
  summarise(Borrower = n_distinct(id.counterparties.x))
result2 <- merged %>% 
  group_by(role.y) %>%
  summarise(GBV = sum(gbv.original))
results <- cbind(result, result2)

df <- merged
col<- "type"
pattern_replacement <- list("corrente" = "Bank Accounts", "mutuo ipotecario" = "Mortgages", 
                            "credito" = "Other", "mutuo chiro" = "Personal Loans", "fondiario" = "mortgages fondiario")
merged <- change_Value_If_Pattern_Found(df, col, pattern_replacement)


#tab 2
total_n_borrowers <- sum(merged %>% group_by(type) %>% summarize(`N Borrowers` = n_distinct(id.counterparties.x)) %>% pull(`N Borrowers`))

Loans_Report_2 <- merged %>% group_by(type) %>% 
  summarize("N Borrowers" = n_distinct(id.counterparties.x), 
            "Ratio" = round(`N Borrowers` / total_n_borrowers *100, 2), 
            "N Loans"= n(), "Total_GBV" = sum(gbv.original), "Mean_GBV" = sum(gbv.original)/total_n_borrowers)



gbv_ranges <- c(0, 60000, 300000, Inf)  # Define the age ranges
gbv_labels <- c("0-60k", "60k-300k", "300k +")  # Define labels for the ranges

# Create a new column 'age_group' with the age ranges
merged$range.gbv <- cut(merged$gbv.original, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)

#tab 3
total_n_borrowers <- sum(merged %>% group_by(range.gbv, status) %>% summarize(`N Borrowers` = n_distinct(id.counterparties.x)) %>% pull(`N Borrowers`))

Loans_Report_3 <- merged %>% group_by(range.gbv, status) %>% 
  summarize("N Borrowers" = n_distinct(id.counterparties.x), 
            "Ratio" = round(`N Borrowers` / total_n_borrowers *100, 2), 
            "N Loans"= n(), GBV = sum(gbv.original))

#tab 3
total_n_borrowers <- sum(merged %>% group_by(range.gbv, status) %>% summarize(`N Borrowers` = n_distinct(id.counterparties.x)) %>% pull(`N Borrowers`))

Loans_Report_3 <- merged %>% group_by(range.gbv, status) %>% 
  summarize("N Borrowers" = n_distinct(id.counterparties.x), 
            "Ratio" = round(`N Borrowers` / total_n_borrowers *100, 2), 
            "N Loans"= n(), GBV = sum(gbv.original))


#tab 4
total_n_borrowers <- sum(merged %>% group_by(range.gbv) %>% summarize(`N Borrowers` = n_distinct(id.counterparties.x)) %>% pull(`N Borrowers`))

Loans_Report_4 <- merged %>% group_by(range.gbv) %>% 
  summarize("N Borrowers" = n_distinct(id.counterparties.x), 
            "Ratio" = round(`N Borrowers` / total_n_borrowers *100, 2), 
            "N Loans"= n(), GBV = sum(gbv.original))


#tab 5
total_n_borrowers <- sum(merged %>% group_by(area) %>% summarize(`N Borrowers` = n_distinct(id.counterparties.x)) %>% pull(`N Borrowers`))

Loans_Report_5 <- merged %>% group_by(range.gbv) %>% 
  summarize("N Borrowers" = n_distinct(id.counterparties.x), 
            "Ratio" = round(`N Borrowers` / total_n_borrowers *100, 2), 
            "N Loans"= n(), GBV = sum(gbv.original))





