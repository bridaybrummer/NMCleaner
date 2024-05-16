#' Shape files
#' @description
#' Running this code will bring up this helper page and also return the shape_file object which is stored in the environement.
#' You can choose whetehr you want provincial, district or sub-district shape files by navigating wiht the '$' operator
#'
#' @return a function that helps you navigate to the shape files
#' @export
#' @examples shape_files$provinces #returns the provinces shape file.
shapes<- function(){
  ?shapes()
  return(shape_files)
}
