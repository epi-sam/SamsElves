
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
                      , overwrite = FALSE
                      , verbose = FALSE)
            expect_true(file.exists(fpath_supported_ftype))
          })

test_that("save_file forbids overwrite",
          {

            withr::local_file(dir_full)
            dir.create(dir_full)

            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , overwrite = FALSE
                      , verbose = FALSE)

            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , overwrite = FALSE
                        , verbose = FALSE)
              , regexp = "File already exists, not over-writing:"
            )
          })

test_that("save_file overwrites with base behavior",
          {

            withr::local_file(dir_full)
            dir.create(dir_full)

            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , verbose = FALSE)

            expect_no_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , verbose = FALSE)
            )
          })

test_that("save_file errors correctly when parent directory does not exist",
          {
            expect_error(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = "Parent directory does not exist, please create it first"

            )
          })

test_that("save_file errors correctly for wrong csv_writer",
          {
            withr::local_file(dir_full)
            dir.create(dir_full)
            expect_error(
              save_file(object = save_object
                        , f_path = fname_csv_ftype
                        , csv_opt = "bad::package")
              , regexp = "csv_opt must be one of: readr::write_excel_csv, readr::write_excel_csv2, data.table::fwrite, utils::write.csv, utils::write.csv2"
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
              , regexp = "Reading file:"
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

test_that("read_file .csv option errors and works properly with alternate functions",
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


test_that("read_file ... works to pass extra args to reader function",
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
test_that("test cleanup works - tempdir (dir_parent) exists and dir_full does not",
          {
            expect_true(dir.exists(dir_parent))
            expect_false(dir.exists(dir_full))
          })
