#' @title Validation of output csv parameter 
#' @description  Validation of the CSV argument to make sure it is of the correct format 
#' @param arg_name The name in error handling to describe the argument, 
#' the name will be used in the validation messages.
#' @param arg_value The input argument, a full csv path including file name and extension.
#' 
#'  
#' @export
#' @return validation or error messages
#' 
#' @examples
#' {
#'     CSVout = NULL
#'     result <- testCsvParameter("CSV", CSVout)
#'     }


testCsvParameter <- function(arg_name, arg_value) {
  caller <- sys.call(-1)
  tryCatch({
    
    if (is.null(arg_value)) {
      # message(paste("Warning in", as.character(caller[1]), ":", arg_name, "argument is NULL. No CSV file will be generated."))
      return()
    }
    
    if (!is.character(arg_value)) {
      stop(paste("Error in", as.character(caller[1]), ":", arg_name, "must be character or NULL "))
    }
    
    if (arg_value == FALSE) {
      stop(paste("Error in", as.character(caller[1]), ":", arg_name, "must be character or NULL "))
    }
    
    if (arg_value == TRUE) {
      stop(paste("Error in", as.character(caller[1]), ":", arg_name, "must be character or NULL"))
    }
    
    if (arg_value == "") {
      #message(paste("Warning in", as.character(caller[1]), ":", arg_name, "is empty. No CSV file will be generated."))
      invisible(NULL)
    }
    
    
    # Check if it's a directory path
    if (dir.exists(arg_value)) {
      stop(paste("File path for", arg_name, "is a directory path missing the file name and extension"))
    }
    
    
    if (!grepl("\\.csv$", arg_value)) {
      stop(paste("File", arg_name, "does not have the .csv extension."))
    }
    
    # if (!file.exists(arg_value)) {
    #   stop(paste("File path for", arg_name, "does not exist."))
    # }
 
  }, error = function(e) {
    stop(message(e$message))
  })
}
