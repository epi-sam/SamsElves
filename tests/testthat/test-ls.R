# started: 2023 Oct 04 09:02:04
# dumb test to ensure you can access the file system

test_that(
  "ls works",
  {
  sys_return <- system("ls / | head -n1", intern = TRUE)
  expect_equal(sys_return, "bin")
  }
)
