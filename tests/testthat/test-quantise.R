test_that("quantise works correctly", {
    # Test basic quantisation
    test_vector <- 1:20
    result <- quantise(test_vector)

    expect_equal(length(result), 20) # Should maintain vector length
    expect_true(all(result >= 1 & result <= 10)) # Default n=10 quantiles
    expect_true(is.integer(result)) # Should return integers

    # Test with different n
    result_5 <- quantise(test_vector, n = 5)
    expect_true(all(result_5 >= 1 & result_5 <= 5)) # Should have 5 quantiles

    # Test inversion
    result_inv <- quantise(test_vector, invert = TRUE)
    expect_equal(result_inv[1], max(result)) # First element should be highest quantile
    expect_equal(result_inv[20], min(result)) # Last element should be lowest quantile

    # Test error on single value
    expect_error(
        quantise(rep(1, 10)),
        "The vector cannot be quantised as there is only one unique value."
    )

    # Test with NA values
    test_na <- c(1:10, NA)
    result_na <- quantise(test_na)
    expect_true(is.na(result_na[11])) # NA should be preserved

    # Test warning for unequal bins
    # Using a vector that will produce unequal bins
    expect_warning(
        quantise(1:11),
        "Quantiles are not in equal bins."
    )
})
