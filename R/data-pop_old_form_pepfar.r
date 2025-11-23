#' OLD District-level population estimates (Stats SA)
#'
#' Mid-year population estimates for South African districts (2002-2030)
#' compiled from Stats SA open workbooks and harmonised with the canonical
#' province/district dictionaries. Created via `data-raw/prepare_pop_files_StatSA.R`.
#'
#' @format A data frame/tibble with 51,272 rows and 12 columns:
#' \describe{
#'   \item{Name}{Original Stats SA label combining province acronym, district and code}
#'   \item{Sex}{"Female" or "Male"}
#'   \item{Age}{Stats SA age band}
#'   \item{Year}{Reference year (character, YYYY)}
#'   \item{Population}{Estimated population for the given slice}
#'   \item{Date}{Mid-year date (Date column, July 1st of `Year`)}
#'   \item{prov_from_Name}{Province acronym parsed from `Name`}
#'   \item{district_code}{District code parsed from `Name` (e.g., DC44)}
#'   \item{district_from_Name}{Free-text district label parsed from `Name`}
#'   \item{province_standard}{Canonical province name after `mutate_provinces()`}
#'   \item{prov}{Canonical province code}
#'   \item{district_standard}{Canonical district name after `mutate_district_name_v2()`}
#' }
#' @source \url{https://www.statssa.gov.za/publications/P0302/}
#' @seealso \code{data-raw/pop_old_from_pepfar.rda}
#' @examples
#' data(pop_old_from_pepfar)
#' dplyr::glimpse(pop_old_from_pepfar)
"pop_old_from_pepfar"
