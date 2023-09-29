test_that(
  "ls works",
  {
    expect_output(print(system("ls /")))
  }
)
