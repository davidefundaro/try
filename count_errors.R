###------------------------------------------###
#---      functions (maybe useless)        -----
###------------------------------------------###
vector <- c("utp", "bad")


########               ########
  # count errors function # 
########               ########
# Create a dataframe
values_to_check <- c("Utp", ".bad", "brr", "Autp", "bad", "lalala")
values_to_check <- data.frame(Value = values_to_check)
values_to_check$Value <- gsub("[[:punct:]]", "", values_to_check$Value)


count_errors <- function(table, Value, vector) {

# Initialize a count of values not found
count_not_found <- 0

# Loop through rows of the dataframe
for (row in 1:nrow(table)) {
  #set to lower
  table[[Value]][row] <- tolower(table[[Value]][row])
  # Check if the value in the row is in the vector
  if (!(table[[Value]][row] %in% vector)) {
    count_not_found <- count_not_found + 1
  }
}
return(count_not_found)
}

count_not_found <- count_errors(values_to_check, "Value", vector)

# Print the count of values not found
cat("nella colonna ci sono", count_not_found, "insoliti")



########               ########
# tables from matrix function # 
########               ########
data<- Loans_matrix_rounded[c(2:40),]
key <- "ndg" 

tables_from_matrix <- function(data, key, k) {
  num_rows <- nrow(Loans)  
  NDG_Dependent <- data.frame(matrix(NA, nrow = num_rows, ncol = 0))
  Loan_Dependent <- data.frame(matrix(NA, nrow = num_rows, ncol = 0))
  #i <- 1     
  for (j in 1:length(data[[key]])) {
    if (data[[key]][j] == 1) {
      new_column <- Loans[, j]
      col_name <- colnames(Loans)[j]
      NDG_Dependent[, col_name] <- new_column
      #i <- i + 1
    } else {
      new_column <- Loans[, j]
      col_name <- colnames(Loans)[j]
      Loan_Dependent[, col_name] <- new_column
    }
  }
  if(k == TRUE) {return(NDG_Dependent)}
  else {return(Loan_Dependent)}
}

tabella_1 <- tables_from_matrix(data, key, TRUE)
tabella_2 <- tables_from_matrix(data, key, FALSE)

