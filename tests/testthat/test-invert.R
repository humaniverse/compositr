test_that("invert_rank works correctly", {
  test_vector <- 1:5
  result <- invert_rank(test_vector)
  
  expect_equal(length(result), 5)
  expect_equal(result[1], 5)  # First element should become last
  expect_equal(result[5], 1)  # Last element should become first
  
  # Test with NA values
  test_na <- c(1:5, NA)
  result_na <- invert_rank(test_na)
  expect_true(is.na(result_na[6]))
})

test_that("invert_this works correctly", {
  test_vector <- 1:5
  result <- invert_this(test_vector)
  
  expect_equal(length(result), 5)
  expect_equal(result[1], 5)  # First element should become last
  expect_equal(result[5], 1)  # Last element should become first
  
  # Test with NA values
  test_na <- c(1:5, NA)
  result_na <- invert_this(test_na)
  expect_true(is.na(result_na[6]))
}) 