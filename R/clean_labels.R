#' tbl_summary cleaning helper functions
#'
#' @param gt_summary_object a tbl_summary object
#'
#' @return a tbl_summary object with cleaned labels
#' @export
#'
#' @examples add it as a pip fucntion at the end of your tbl_summary pipeline
clean_labels <- function(gt_summary_object) {
  gt_summary_object[["table_body"]][["labels"]] <- gt_summary_object[["table_body"]][["labels"]] %>%
    gsub("_", " ", .) %>%
    str_to_title()
  return(gt_summary_object)
}


