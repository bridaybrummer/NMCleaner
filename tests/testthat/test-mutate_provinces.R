library(testthat)

test_that("mutate_provinces maps common variants and abbreviations", {
  input <- c("EC", "Eastern Cape", "eastern cape", "gp", "Gauteng", "western cape", "WC", "LP", "limpopo")
  res <- mutate_provinces(data.frame(province = input))

  expect_equal(res$prov[1], "EC")
  expect_equal(res$province_standard[2], "Eastern Cape")
  expect_equal(res$prov[4], "GP")
  expect_equal(res$prov[6], "WC")
  expect_equal(res$prov[8], "LP")
})


## function ready for package use; examples are in the roxygen block above.
# example

# make dictionary
#provinces_db <- generate_canonical_provinces()

#provinces_db

#mutate_provinces(
#    data.frame(province = c("lim", "nothern cape", "nothern", "lipompo", "gp", "GT")),
#    fuzzy = TRUE,
#    dictionary = provinces_db$dict_long
#)
