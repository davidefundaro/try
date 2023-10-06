
###------------------------------------------###
#---            primary keys        -----
###------------------------------------------###

#function that finds possible keys of a table:
#check for the presence of duplicates and NAs and if it doesn't it is a possible key
detect_primary_keys <- function(table) {
  possible_keys <- c()
  for (col in names(table)) {
    if (length(unique(table[[col]])) == nrow(table) && !any(is.na(table[[col]]))) {
      possible_keys <- c(possible_keys, col)
    }
  }
  return(possible_keys)
}


#here we modify the function giving the number of NAs and percentage of uniqueness(num in 0:1) as parameters:
detect_primary_keys_NAs_perc <- function(table,number_NAs,percentage) {
  possible_keys_NAs_perc <- c()
  for (col in names(table)) {
    if (!any(is.na(table[[col]])) | sum(is.na(table[[col]]))==number_NAs) {
      if(length(unique(table[[col]]))/nrow(table) >= percentage){
        possible_keys_NAs_perc <- c(possible_keys_NAs_perc, col)
      }
    }
  }
  return(possible_keys_NAs_perc)
}




###------------------------------------------###
#---            formatting       -----
###------------------------------------------###

#function that capitalize the first letter and the letter after _ and the rest lowercase by splitting the string, modify the substring and unify:
capitalize_after_underscore <- function(x) {
  parts <- unlist(strsplit(x, "_"))
  formatted_parts <- sapply(parts, function(part) {
    if (nchar(part) > 0) {
      paste0(toupper(substring(part, 1, 1)), tolower(substring(part, 2)))
    } else {
      ""
    }
  })
  return(paste(formatted_parts, collapse = "_"))
}




###------------------------------------------###
#---            dependencies       -----
###------------------------------------------###

#check the functional dependencies of the columns against one (key) by grouping the data and counting the unique values:
check_dependencies<- function(table, key) {
  lista_num <- list()
  for (col in names(table)) {
    grouped_data <- split(table[[col]], table[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (all(dependency_check ==1 )) {
      lista_num[[col]] <- 1
    } else {
      lista_num[[col]] <- length(dependency_check) / sum(dependency_check)
    }
  }
  
  result <- lista_num
  return(result)
}


#this function works as the one before but before grouping and counting the unique values it deletes the NAs:
check_dependencies_NA<- function(table, key) {
  lista_num <- list()
  for (col in names(table)) {
    tabella_prova <- table[!is.na(table[[col]]), ]
    grouped_data <- split(tabella_prova[[col]], tabella_prova[[key]], drop = TRUE) 
    dependency_check <- sapply(grouped_data, function(group) length(unique(group)))
    
    if (all(dependency_check ==1 )) {
      lista_num[[col]] <- 1
    } else {
      lista_num[[col]] <- length(dependency_check) / sum(dependency_check)
    }
  }
  
  result <- lista_num
  return(result)
}


#unifying the two functions in one using the variable 'con_o_senza'(TRUE or FALSE) to decide which function to use:
check <- function(table, key,con_o_senza_na){
  if(con_o_senza_na){
    return(check_dependencies(table, key))
  } else {
    return(check_dependencies_NA(table, key))
  }
}



###------------------------------------------###
#---           printing       -----
###------------------------------------------###

#the check function return a list of two lists, this function print them in a readable way:
print_list <- function(list){
  for (col in names(list)){
    value <- as.numeric(list[[col]])*100
    formatted_ratio <- paste0(format(round(value, 2), nsmall = 2), "%")
    cat("Column Name: ", col, "  Ratio: ", formatted_ratio, "\n")
  }
  cat("\n")
}



#function that check the dependencies of all column vs all column and saves the results in a matrix:
#using the check_dependencies function defined above:

find_dependencies_matrix <- function(table) {
  num_cols <- ncol(table)
  dependency_matrix <- matrix(NA, nrow = num_cols, ncol = num_cols)
  colnames(dependency_matrix) <- colnames(table)
  rownames(dependency_matrix) <- colnames(table)
  
  for (col_2 in colnames(dependency_matrix)) {
    new_column <- check_dependencies(table, col_2) 
    new_column <- unlist(new_column)
    dependency_matrix[,col_2] <- new_column
  }
  return(dependency_matrix)
}

###------------------------------------------###
#---           see package version       -----
###------------------------------------------###
listInstalledPackages <- function() {
  installed_packages <- installed.packages()
  package_data <- data.frame(
    Package = rownames(installed_packages),
    Version = installed_packages[, "Version"],
    stringsAsFactors = FALSE
  )
  return(package_data)
}

# Call the function to list installed packages and their versions
installed_packages <- listInstalledPackages()

# Print the results
print(installed_packages)