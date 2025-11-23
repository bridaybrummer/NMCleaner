library(testthat)
library(tibble)

test_that("mutate_district_name_v2 exact matches with long dictionary", {
  dict <- tibble::tibble(
    district_standard = c("Foo District", "Bar District"),
    district_code = c("D1", "D2"),
    variant = c("Foo", "Bar")
  )

  res <- mutate_district_name_v2(c("Foo", "Bar", "Unknown"), dictionary = dict)

  expect_equal(res$district_standard, c("Foo District", "Bar District", NA_character_))
  expect_equal(res$district_code, c("D1", "D2", NA_character_))
  expect_equal(res$match_method, c("exact", "exact", NA_character_))
})

test_that("mutate_district_name_v2 fuzzy matching works", {
  dict <- tibble::tibble(
    district_standard = c("Foo District"),
    district_code = c("D1"),
    variant = c("Foo")
  )

  # introduce a small typo that should be matched when fuzzy = TRUE
  res <- mutate_district_name_v2(c("F0o", "Foo"), dictionary = dict, fuzzy = TRUE, max_dist = 2L)

  # second entry should be exact
  expect_equal(res$district_standard[2], "Foo District")
  expect_equal(res$district_code[2], "D1")

  # first entry should at least not be NA for match_method when fuzzy is enabled
  expect_true(!is.na(res$match_method[1]) || res$match_method[1] %in% c("fuzzy", "exact"))
})

test_that("mutate_district_name_v2 accepts list-column dictionary", {
  dict_list <- tibble::tibble(
    district_standard = c("Baz District"),
    district_code = c("D3"),
    variants = list(c("Baz", "Bazz"))
  )

  res <- mutate_district_name_v2(c("Bazz", "Baz"), dictionary = dict_list)

  expect_equal(res$district_standard, c("Baz District", "Baz District"))
  expect_equal(res$district_code, c("D3", "D3"))
  expect_equal(res$match_method, c("exact", "exact"))
})

test_that("Handles tricky names with doctor, saint and other problematic prefixes", {
  res <- mutate_district_name_v2(data.frame(district = c("doctor kenneth kaunda", "sekhukhune", "doctor ruth segomotsi mompati")))

  expect_equal(res$district_standard, c("Dr Kenneth Kaunda", "Sekhukhune", "Dr Ruth Segomotsi Mompati"))
#  expect_equal(res$match_method, c("exact", "exact", "exact"))
})
