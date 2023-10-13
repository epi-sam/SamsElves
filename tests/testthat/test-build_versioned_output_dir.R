test_that("get_output_dir functionality works", {
  # create random root directory
  root <- system("mktemp -d", intern = TRUE)
  # run this cleanup code as teardown for the test
  teardown(unlink(root, recursive = TRUE))
  
  # expect bootstrap to work
  expect_equal(file.path(root, "1999_09_09.01"), get_output_dir(root = root, date = "1999_09_09"))
  expect_true(dir.exists(file.path(root, "1999_09_09.01")))
  
  # incrementing automatically happens
  expect_equal(file.path(root, "1999_09_09.02"), get_output_dir(root = root, date = "1999_09_09"))
  
  # handle convenience "today" value
  today.v1 <- format(Sys.Date(), "%Y_%m_%d.01")
  expect_equal(file.path(root, today.v1), get_output_dir(root = root, date = "today"))
})

test_that("get_latest_output_date_index returns 0 if no dirs exist", {
  # neither of these directories exist
  expect_equal(0, get_latest_output_date_index("/does/not/exist", date = "2001_01_01"))
  expect_equal(0, get_latest_output_date_index("fixtures/versioned-dirs/2000_01_01", date = "2001_01_01"))
})

test_that("get_latest_output_date_index returns correct value", {
  expect_equal(2, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09"))
})