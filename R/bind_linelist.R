#' bind_linelist
#'
#' @param directory a string character denoting the directory where the linelist is stored
#'  This function will find all .xlsx files stored in a directory or folder. Note that the directory is the name of the folder where the.xlsx files are stored and NOT the name of the .xlsx file itself.
#'  The function converte the read in .xlsx file all to character to ensure all data can be bound together.
#'  type.convert is then implemented for appropriate type conversion however it is not perfect and may need to be adjusted.
#'
#'  Please note, the directory name is added as a column to the data.frame. This is useful for tracking the source of the data.
#'  We recommend that the output is then fed into stata2script()
#'
#' @return a tibble
#' @export
#'
#' @import readxl
#'
#' @examples
#' bind_linelist("database/linelist")
bind_linelist <- function(directory) {
  excel_files <- list.files(directory, pattern = "\\.xlsx$", full.names = TRUE)

  # Read all Excel files and handle type inconsistencies
  df_list <- lapply(excel_files, function(file) {
    df <- read_excel(file)
    df <- df %>%
      mutate(across(everything(), as.character)) %>%
      mutate( directory_names = directory)
    #mutate(across(where(is.numeric), as.character)) %>%
    #mutate(across(where(is.factor), as.character))
    return(df)
  })

  # Bind all data frames into one
  all_data <- bind_rows(df_list)%>%type.convert()

  return(all_data)
}
