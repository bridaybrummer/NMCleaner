#' Title
#'
#' @param gt_summary_object
#'
#' @return removes the first row of the table_body of a gt_summary object
#' @export
#'
#' @examples THis is useful when you may have removed the first label or row of a gtsummary object as it is blank
trim_tbl <- function(gt_summary_object) {
  length_table_body <- gt_summary_object$table_body %>% nrow()
  gt_summary_object$table_body <- gt_summary_object$table_body[2:length_table_body, ]
  return(gt_summary_object)
}
