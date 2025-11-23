library(testthat)
library(NMCleaner)

set.seed(42)

test_that("cusum_window_start detects a simulated outbreak", {
  # simulate 300 days of baseline Poisson noise with an outbreak spike
  days <- 300
  baseline <- rpois(days, lambda = 2)

  # inject an outbreak between day 160 and 180
  baseline[160:180] <- baseline[160:180] + rpois(21, lambda = 8)

  df <- tibble::tibble(
    date = seq.Date(as.Date('2020-01-01'), by = 1, length.out = days),
    n = baseline
  )
  
  res <- cusum_window_start(df, window = 10, date_index = "date", k = 0.5, cusum_threshold = 1)

  res
  
  # Expect at least one window to be marked as outbreak after the outbreak starts
  expect_true(any(res$latest_is_cusums_outbreak, na.rm = TRUE))
})
