##' Backward compatible wrapper for subdistrict standardisation
##'
Make_sub_district_name <- function(...) {
    .Deprecated("mutate_sub_district")
    mutate_sub_district(...)
}
##' @param ... Passed to `mutate_sub_district()`
