library(testthat)

test_that("OR_95CI returns correct odds ratio and confidence intervals", {

  # Set up known values
  coef <- c(0.5, -0.3)
  se <- c(0.1, 0.2)

  # Expected results based on manual calculation
  expected_result_1 <- "1.649 (1.355, 2.006)"
  expected_result_2 <- "0.741 (0.501, 1.096)"

  # Call the OR_95CI function and check if results match
  expect_equal(OR_95CI(coef[1], se[1], siglevel = 0.05, roundto = 3), expected_result_1)
  expect_equal(OR_95CI(coef[2], se[2], siglevel = 0.05, roundto = 3), expected_result_2)
})

