# write tests for the three brace balance files in the fixtures directory


test_that("check_brace_balance works", {
  # Define paths to test files
  balanced_file <- "fixtures/brace_checks/brace_check_good.R"
  unclosed_file <- "fixtures/brace_checks/brace_check_unclosed.R"
  overclosed_file <- "fixtures/brace_checks/brace_check_overclosed.R"

  # Test balanced braces
  expect_equal(check_brace_balance(balanced_file), "Balanced")
  expect_equal(check_brace_balance(unclosed_file), "Unclosed brackets")
  expect_equal(check_brace_balance(overclosed_file), "Too many closing brackets")
})
