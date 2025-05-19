check_n_columns <- function(df, source, num_columns) {
  caller <- sys.call(-1) #define the caller function
  tryCatch({
    if (ncol(df) < num_columns) {
      stop(paste("In",as.character(caller[1]), ", the data", source, " has less than the minimum required ", num_columns, "columns."))
    } else if (ncol(df) > num_columns) { 
      warning(paste("In", as.character(caller[1]),", the data", source, " has more than", num_columns, "columns."))
    } else {
      #print(paste("The data", source, " has exactly", num_columns, "columns."))
    }
  }, error = function(e) {
    print(paste("Error:", e$message))
  }, warning = function(e) {
    print(paste("Warning:", e$message))
  })
}