test_that("expect input error", {
  expect_error(msg_prt(TRUE, "both"))
  expect_error(msg_prt(55L, "both"))
  expect_error(msg_prt(NA, "both"))
  expect_error(msg_prt(NaN, "both"))
  expect_error(msg_prt(NULL, "both"))
  expect_error(msg_prt(Inf))
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