testInputTable <- function(arg_name, arg_value) {
  errors <- c()  # Initialiser un vecteur pour collecter les messages d'erreur
  caller <- sys.call(-1) #define the caller function
  
  if (missing(arg_value) || length(arg_value) == 0) {
    stop(paste("Argument", arg_name, "is not defined or empty"))
  }
  tryCatch({
    if (is.character(arg_value)) {
      if (!file.exists(arg_value)) {
        errors <- c(errors, paste("Error in", as.character(caller[1]), ": The file path specified for", arg_name, "does not exist. Please provide a valid file path."))
      } else {
        #message(paste("Verification:", arg_name, "file path exists."))
        
        if (!grepl("\\.csv$", arg_value)) {
          errors <- c(errors, paste("Error in", as.character(caller[1]), ": The file specified for", arg_name, "is not a .csv file. Please ensure the file has a .csv extension."))
        # } else {
        #   message(paste("Verification:", arg_name, "file format is correct (CSV)."))
        }
      }
      
      if (length(errors) == 0) {  # Si aucune erreur, essayer de lire le fichier CSV
        data <- tryCatch({
          read.csv2(arg_value, header = TRUE, sep = ",")
        }, warning = function(w) {
          errors <- c(errors, paste("Warning in", as.character(caller[1]), ": Failed to import data from", arg_name, ". The file might be corrupted or improperly formatted."))
          return(invisible())
        })
        
        if (exists("data")) {
          #message(paste("Success:", arg_name, "has been successfully loaded as a dataframe."))
          return(data)
        }
      }
      
    } else if (is.data.frame(arg_value)) {
     # message(paste("Success:", arg_name, "is already a dataframe and is ready for immediate use.")) 
      return(arg_value)
      
    } else {
      errors <- c(errors, paste("Error in", as.character(caller[1]), ":", arg_name, "must be either a valid .csv file path or a dataframe."))
    }
    
  }, error = function(e) {
    errors <- c(errors, paste("Error in", as.character(caller[1]), ":", e$message))
  })
  
  if (length(errors) > 0) {
    cat("Errors encountered:\n", paste(errors, collapse = "\n"), "\n")
    stop("Errors were encountered. Execution halted.")
  }
  
  invisible(NULL)  # Use invisible() to suppress the NULL return from being printed
}
