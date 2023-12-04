
# Common test cruft

# directories
dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
root        <- file.path(dir_parent, dir_child)

test_that("get_output_dir functionality works", {
  
  withr::local_file(root)
  # dir.create(root)
  
  # expect bootstrap to work
  expect_equal(file.path(root, "1999_09_09.01"), make_versioned_dir(root = root, date = "1999_09_09"))
  expect_true(dir.exists(file.path(root, "1999_09_09.01")))
  
  # incrementing automatically happens
  expect_equal(file.path(root, "1999_09_09.02"), make_versioned_dir(root = root, date = "1999_09_09"))
  
  # handle convenience "today" value
  today.v1 <- format(Sys.Date(), "%Y_%m_%d.01")
  expect_equal(file.path(root, today.v1), make_versioned_dir(root = root, date = "today"))
  
  test_that("get_latest_output_date_index returns correct value", {
    expect_equal(2, get_latest_output_date_index(dir = root, date = "1999_09_09"))
  })
  
  
})

test_that("get_latest_output_date_index returns 0 if no dirs exist", {
  # neither of these directories exist
  expect_equal(0, get_latest_output_date_index("/does/not/exist", date = "2001_01_01"))
  expect_equal(0, get_latest_output_date_index("fixtures/versioned-dirs/2000_01_01", date = "2001_01_01"))
})

