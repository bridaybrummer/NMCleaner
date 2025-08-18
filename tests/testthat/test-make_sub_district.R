library(testthat)
library(tibble)

test_that("Make_sub_district_name maps exact normalized keys and keeps unmatched", {

    readr::read_rds("inst/extdata/sa_subdistrict_dictionary.rds")-> dict_long

    dict  <- dict_long %>%
        dplyr::group_by(province, district, subdistrict_code, standard_subdistrict) %>%
        dplyr::summarise(subdistrict_variant = list(unique(variant)), .groups = "drop")

  df <- tibble::tibble(subdistrict = c("dr pixley ka seme", "dr beyers naudé", "city of cape Town", "port saint johns"))
  res <- mutate_sub_district(df, subdistrict_variable = "subdistrict", dict = dict, use_fuzzy = FALSE, unmatched = "keep_raw")

  expect_equal(res$subdistrict_standard, c("Pixley ka Seme", "Dr Beyers Naudé", "City of Cape Town", "Port St Johns"))
})


test_that("unmatched = 'NA' sets unmatched values to NA", {
  dict_long <- tibble::tibble(
    standard_subdistrict = "STD1",
    variant = "Alpha",
    variant_norm = "alpha"
  )
  dict <- data.frame(dummy = 1)
  attr(dict, "long_version") <- dict_long

  df <- tibble::tibble(subdistrict = c("Alpha", "unknown"))
  res <- mutate_sub_district(df, subdistrict_variable = "subdistrict", dict = dict, use_fuzzy = FALSE, unmatched = "NA")

  expect_equal(res$subdistrict_standard, c("STD1", NA_character_))
})
