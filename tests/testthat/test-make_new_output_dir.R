# Common test cruft

# directories
dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)


# cribbed from ihme.covid repo
test_that("get_latest_output_date_index returns 0 if no dirs exist", {
  # neither of these directories exist
  expect_equal(0, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "2001_01_01"))
})

.suffix <- "blargfoobar"
test_that("get_latest_output_date_index returns correct value", {
  expect_equal(2, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09"))
  expect_equal(15, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09", suffix = .suffix))
})

test_that("get_latest_output_dir works", {
  latest_dir     <- get_latest_output_dir(root = "fixtures/versioned-dirs/nested/1999_09_09")
  latest_dir_sfx <- get_latest_output_dir(root = "fixtures/versioned-dirs/nested/1999_09_09", suffix = .suffix)

  expect_equal(latest_dir, "fixtures/versioned-dirs/nested/1999_09_09/1999_09_09.02")
  expect_equal(latest_dir_sfx, "fixtures/versioned-dirs/nested/1999_09_09/1999_09_09.15blargfoobar")
})

test_that("get_latest_output_dir errors correctly", {
  expect_error(
    get_latest_output_dir(root = "fixtures/DOES-NOT-EXIST"),
    "Assertion on 'root' failed: Directory 'fixtures/DOES-NOT-EXIST' does not exist."
  )

  expect_error(
    get_latest_output_dir(root = "fixtures/versioned-dirs"),
    "No YYYY_MM_DD.VV<suffix> directories in fixtures/versioned-dirs"
  )
})


test_that("get_new_output_dir functionality works", {

  # create random root directory with self-teardown (`teardown()` is deprecated)
  withr::local_file(dir_full)
  dir.create(dir_full)

  # expect bootstrap to work
  expect_equal(file.path(dir_full, "1999_09_09.01"), get_new_output_dir(root = dir_full, date = "1999_09_09"))
  expect_equal(file.path(dir_full, "1999_09_09.01_sfx"), get_new_output_dir(root = dir_full, date = "1999_09_09", suffix = "_sfx"))
  expect_false(dir.exists(file.path(dir_full, "1999_09_09.01")))
})


test_that("make_new_output_dir functionality works", {

  # create random root directory with self-teardown (`teardown()` is deprecated)
  withr::local_file(dir_full)
  dir.create(dir_full)

  # expect bootstrap to work
  expect_equal(file.path(dir_full, "1999_09_09.01"), make_new_output_dir(root = dir_full, date = "1999_09_09"))
  expect_true(dir.exists(file.path(dir_full, "1999_09_09.01")))
  expect_equal(file.path(dir_full, "1999_09_09.01_sfx"), make_new_output_dir(root = dir_full, date = "1999_09_09", suffix = "_sfx"))
  expect_true(dir.exists(file.path(dir_full, "1999_09_09.01_sfx")))

  # incrementing automatically happens
  expect_equal(file.path(dir_full, "1999_09_09.02"), make_new_output_dir(root = dir_full, date = "1999_09_09"))
  expect_equal(file.path(dir_full, "1999_09_09.02_sfx"), make_new_output_dir(root = dir_full, date = "1999_09_09", suffix = "_sfx"))

  # handle convenience "today" value
  today.v1 <- format(Sys.Date(), "%Y_%m_%d.01")
  expect_equal(file.path(dir_full, today.v1), make_new_output_dir(root = dir_full, date = "today"))
})


# Last test
test_that("test cleanup works - tempdir (dir_parent) exists and dir_full does not",
          {
            expect_true(dir.exists(dir_parent))
            expect_false(dir.exists(dir_full))
          })
