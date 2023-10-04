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
