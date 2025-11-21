CsvFileSave <- function(CSVpath, OutputDF) {
  if (!is.null(CSVpath)) {
    
    # Check if the file exists and prompt for overwrite confirmation
    if (file.exists(CSVpath)) {
      cat("A CSV file with the same name already exists.\n")
      cat("Warning: This action will overwrite the existing file.\n")
      
      proceed <- ""
      while (!(proceed %in% c("y", "n"))) {
        proceed <- tolower(readline("Do you want to proceed? (y/n): "))
        if (!(proceed %in% c("y", "n"))) {
          cat("Invalid input. Please enter 'y' or 'n'.\n")
        }
      }
      
      if (proceed != "y") {
        cat("Operation aborted.\n")
        return(NULL)
      }
    }
    # Try to write the CSV file with error handling
    tryCatch({
      write.csv(OutputDF, CSVpath, row.names = FALSE)
      cat("The table was saved in", CSVpath, "\n")
    }, error = function(e) {
      cat("An error occurred while writing to the file:\n")
      cat(e$message, "\n")
    })
  }
}
