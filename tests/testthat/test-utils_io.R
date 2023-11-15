test_that("make_directory makes a directory and cleans up afterward", {
  withr::with_tempdir({
    dir1 <- 'temp_directory_1'
    make_directory(dir1)
    expect_true(dir.exists(dir1))
  })
})
