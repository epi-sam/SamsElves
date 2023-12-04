test_that(
  "expect input error", 
  {
    expect_error(msg_prt(string = TRUE, output = "both"))
    expect_error(msg_prt(string = c("two", "strings"), output = "both"))
  }
)

test_that("output is a message", {
  expect_message(msg_prt(string = "My message", output = "message"), regexp = "My message")
})

test_that("output is std_out", {
  expect_output(msg_prt(string = "My std_out", output = "print"), regexp = "My std_out")
})

test_that("output is message and std_out", {
  # \n provides cleaner console output
  msg_prt(string = "\\nMy msg & print", output = "both") %>% 
  expect_message(regexp = "\\\\nMy msg & print") %>% 
  expect_output(regexp = "\\\\nMy msg & print")
})
