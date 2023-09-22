test_that("input is a string", {
  string <- "55"
  expect_type(string, "character")
})

test_that("input is not a string", {
  string <- 55L
  expect_false(is.character(string))
  string <- 55
  expect_false(is.character(string))
  string <- TRUE
  expect_false(is.character(string))
  string <- NA
  expect_false(is.character(string))
  string <- NaN
  expect_false(is.character(string))
  string <- NULL
  expect_false(is.character(string))
  string <- Inf
  expect_false(is.character(string))
})

test_that("output is a message", {
  expect_message(msg_prt("My message", output = "message"))
})

test_that("output is std_out", {
  expect_output(msg_prt("My std_out", output = "print"))
})

test_that("output is message and std_out", {
  expect_output(msg_prt("My msg & print", output = "both"))
  expect_message(msg_prt("My msg & print", output = "both"))
})

