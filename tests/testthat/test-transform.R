test_that("transform_exp works correctly", {
  test_vector <- c(0, 0.5, 1)
  result <- transform_exp(test_vector)

  expect_equal(length(result), 3)
  expect_true(all(result >= 0))
  expect_true(is.numeric(result))
  
  expect_error(transform_exp(c(-1, 0.5, 2)))
}) 