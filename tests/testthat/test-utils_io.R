
# Common test cruft

# directories
dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)
# objects to read and write
save_object             <- list(a = 1, b = 2, c = 3)
fpath_supported_ftype   <- file.path(dir_parent, 'save_object.csv')
fpath_unsupported_ftype <- file.path(dir_parent, 'save_object.h5')

# first enforce the expected tempdir does not exist before tests
# - /tmp is node specific AND
# - /tmp within an Rstudio session is user-specific, part of the singularity container
# - it's complicated
system(paste("rm -rf /tmp/Rtmp*"))
print(system("ls /tmp"))

test_that("make_directory makes a directory and cleans up afterward", 
          {
            withr::local_file(dir_parent)
            make_directory(dir_full)
            expect_true(dir.exists(dir_full))
          }
)

test_that("directory is actually gone",
          {
            expect_false(dir.exists(dir_parent))
          }
)

# Using deprecated fuctions - shown for comparability - please retain
# test_that("make_directory makes a directory and cleans up afterward", {
#   withr::with_tempdir({
#     dir1 <- 'temp_directory_1'
#     make_directory(dir1)
#     expect_true(dir.exists(dir1))
#   })
# })

test_that("save_file writes a file for a correct extension",
          {
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)
            expect_true(file.exists(fpath_supported_ftype))
          }
)

test_that("save_file forbids overwrite",
          {
            
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            
            save_file(object = save_object
                      , f_path = fpath_supported_ftype
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)
            
            expect_message(
              save_file(object = save_object
                        , f_path = fpath_supported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = paste("File already exists, not over-writing :", fpath_supported_ftype)
              
            ) 
            
          }
)

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
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            expect_error(
              save_file(object = save_object
                        , f_path = fpath_unsupported_ftype
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = "This function only supports .* file extensions \\(case-insensitive\\)"
              
            )
            expect_false(file.exists(fpath_unsupported_ftype))
          }
)

test_that("save_file produces correct messages",
          {
            
          }
)


# Last test
test_that("directory is actually gone",
          {
            expect_false(dir.exists(dir_parent))
          }
)