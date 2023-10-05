

###------------------------------------------###
#---               functions          -----
###------------------------------------------###


#given the name of new columns, add them as empty in the table
create_empty_columns <- function(table, vector) {
  x <- ncol(table)
  for(i in 1:length(vector)) {
    new_column <- NA
    colname <- vector[[i]]
    table[, colname] <- new_column
    i <- i+1
  }
  return(table)
}

#clean column names and turn into lower case
colname_function <- function(name) {
  name <- gsub('[/, ]', '.', name)
  name <- tolower(name)
  return(name)
}