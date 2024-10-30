test_that("normalise_ranks_01 works correctly", {
  test_vector <- 1:5
  result <- normalise_ranks_01(test_vector)

  expect_equal(length(result), 5)
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_true(is.numeric(result))

  # Should error on non-ranked data
  expect_error(normalise_ranks_01(c(1, 1, 2, 3, 3)))
})

test_that("normalise_mean works correctly", {
  test_vector <- 1:10
  result <- normalise_mean(test_vector)

  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)

  # Test with NA values
  test_na <- c(1:10, NA)
  result_na <- normalise_mean(test_na, remove_na = TRUE)
  expect_true(!is.na(mean(result_na, na.rm = TRUE)))
})

test_that("normalise_median works correctly", {
  test_vector <- 1:5
  result <- normalise_median(test_vector)

  expect_equal(length(result), 5)
  expect_true(is.numeric(result))

  # Test with NA values
  test_na <- c(1:5, NA)
  result_na <- normalise_median(test_na, remove_na = TRUE)
  expect_true(!is.na(median(result_na, na.rm = TRUE)))
})
