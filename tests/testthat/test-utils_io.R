
# Common test cruft

# directories
dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)
# objects to read and write
save_object             <- list(a = 1, b = 2, c = 3)
fname_supported_ftype   <- 'save_object.rds'
fname_unsupported_ftype <- 'save_object.rdata'
fname_csv_ftype         <- 'save_object.csv'
fpath_supported_ftype   <- file.path(dir_full, fname_supported_ftype)
fpath_unsupported_ftype <- file.path(dir_full, fname_unsupported_ftype)
fpath_csv_ftype         <- file.path(dir_full, fname_csv_ftype)


# the path specified by tempdir() is user & session specific, and required for help files
# - form is Rtmp******
# - it must persist through and after tests
# - /tmp is node specific AND
# - /tmp within an Rstudio session is user-specific, part of the singularity container
# - it's complicated

# Interactive helpers
# print(system("ls -alt /tmp"))
# print(system(paste("ls -alt", dir_parent)))
# print(system(paste("ls -alt", dir_full))) # expect an error - ls: cannot access '/tmp/RtmpMHC8jJ/temp_directory_1': No such file or directory

test_that("make_directory makes a directory and cleans up afterward",
          {
            withr::local_file(dir_full)
            make_directory(dir_full)
            expect_true(dir.exists(dir_full))
          }
)

# Equivalent test Written with Kyle H. - retain for comparability
# test_that("make_directory makes a directory and cleans up afterward", {
#   withr::with_tempdir({
#     make_directory(dir_child)
#     expect_true(dir.exists(dir_child))
#   })
# })

test_that("tempdir (dir_parent) exists and dir_full does not",
          {
            expect_true(dir.exists(dir_parent))
            expect_false(dir.exists(dir_full))
          })

test_that("save_file writes a file for a correct extension",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)
            expect_true(file.exists(fpath_supported_ftype))
          })

test_that("save_file forbids overwrite",
          {

            withr::local_file(dir_full)
            dir.create(dir_full)

            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)

            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = paste("File already exists, not over-writing:", fpath_supported_ftype)

            )

          })

test_that("save_file errors correctly for nonexistent directory",
          {
            expect_error(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = "Parent directory does not exist, please create it first"

            )
          })

test_that("save_file prevents saving files with unsupported extension",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            expect_error(
              save_file(object = save_object
                        , f_path = fpath_unsupported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = "This function only supports .* file extensions \\(case-insensitive\\)"

            )
            expect_false(file.exists(fpath_unsupported_ftype))
          })

test_that("save_file produces correct messages",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)

            # first write
            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = TRUE)
              , regexp = "Saved file to disk"
            )
            # second write - allow overwrite
            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = FALSE
                        , verbose = TRUE)
              , regexp = "Overwriting file:"
            )

            # second write - forbid overwrite
            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = TRUE)
              , regexp = "File already exists, not over-writing:"
            )

          })

test_that("read_file reads a file",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            saveRDS(save_object, fpath_supported_ftype)
            expect_message(
              read_file(
                path_to_file = fpath_supported_ftype
                , verbose = TRUE)
              , regexp = paste("Reading file:", fpath_supported_ftype)
            )

          })

test_that("read_file errors correctly",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            save(save_object, file = fpath_unsupported_ftype)
            expect_error(
              read_file(
                path_to_file = fpath_unsupported_ftype
                , verbose = FALSE)
              , regexp = paste("This function only supports .* file extensions")
            )
          })

test_that(".csv option errors and works properly with alternate functions",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            utils::write.csv(save_object, file = fpath_csv_ftype)
            expect_error(
              read_file(
                path_to_file = fpath_csv_ftype
                , csv_opt = "read_csv"
              )
              , regexp = "csv_opt must be a namespaced function call e.g. data.table::fread - instead got read_csv"
            )

            expect_no_error(
              suppressMessages( # readr is noisy, don't need it for tests
                read_file(
                  path_to_file = fpath_csv_ftype
                  , csv_opt = "readr::read_csv"
                )
              )
            )

          })


test_that("... works to pass extra args to reader function",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            utils::write.csv(save_object, file = fpath_csv_ftype)

            expect_no_message(
              expect_no_error(
                read_file(
                  path_to_file     = fpath_csv_ftype
                  , csv_opt        = "readr::read_csv"
                  , show_col_types = FALSE
                  , name_repair    = "minimal"
                )
              )
            )

          })

# Last test
test_that("tempdir (dir_parent) exists and dir_full does not",
          {
            expect_true(dir.exists(dir_parent))
            expect_false(dir.exists(dir_full))
          })


test_that("get_latest_output_date_index returns 0 if no dirs exist", {
  # neither of these directories exist
  expect_equal(0, get_latest_output_date_index("/does/not/exist", date = "2001_01_01"))
  expect_equal(0, get_latest_output_date_index("fixtures/versioned-dirs/2000_01_01", date = "2001_01_01"))
})

test_that("get_latest_output_date_index returns correct value", {
  expect_equal(2, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09"))
})

test_that("get_latest_output_dir works", {
  latest_dir <- get_latest_output_dir(root = "fixtures/versioned-dirs/nested/1999_09_09")

  expect_equal(latest_dir, "fixtures/versioned-dirs/nested/1999_09_09/1999_09_09.02")
})

test_that("get_latest_output_dir errors correctly", {
  expect_error(
    get_latest_output_dir(root = "fixtures/DOES-NOT-EXIST"),
    "root fixtures/DOES-NOT-EXIST does not exist"
  )

  expect_error(
    get_latest_output_dir(root = "fixtures/versioned-dirs"),
    "No YYYY_MM_DD.VV directories in fixtures/versioned-dirs"
  )
})


test_that("make_new_output_dir functionality works", {
  # create random root directory
  root <- system("mktemp -d", intern = TRUE)
  # run this cleanup code as teardown for the test
  teardown(unlink(root, recursive = TRUE))

  # expect bootstrap to work
  expect_equal(file.path(root, "1999_09_09.01"), make_new_output_dir(root = root, date = "1999_09_09"))
  expect_true(dir.exists(file.path(root, "1999_09_09.01")))

  # incrementing automatically happens
  expect_equal(file.path(root, "1999_09_09.02"), make_new_output_dir(root = root, date = "1999_09_09"))

  # handle convenience "today" value
  today.v1 <- format(Sys.Date(), "%Y_%m_%d.01")
  expect_equal(file.path(root, today.v1), make_new_output_dir(root = root, date = "today"))
})
