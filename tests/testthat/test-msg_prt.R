test_that(
  "expect input error", 
  {
    expect_error(msg_prt(string = TRUE, output = "both"))
    expect_error(msg_prt(string = c("two", "strings"), output = "both"))
  }
)

test_that("output is a message", {
  expect_message(msg_prt(string = "My message", output = "message"))
})

test_that("output is std_out", {
  expect_output(msg_prt(string = "My std_out", output = "print"))
})

test_that("output is message and std_out", {
  expect_output(msg_prt(string = "My msg & print", output = "both"))
  expect_message(msg_prt(string = "My msg & print", output = "both"))
})