
#Bind Linelsts from an NMC export directory

bind_linelist<- function( directory) {
  # List all Excel files in the directory
  excel_files <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE)

  excel_files
  all_data<- NULL
  # Read and bind Excel files into a single data frame
  all_data <- lapply(excel_files, read_excel) %>%
    bind_rows()
  return(all_data)
}

# there are other functions which will assist you inidentifying the most recent file in the directory
