
# Common test cruft

# directories
dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)
# objects to read and write
save_object             <- list(a = 1, b = 2, c = 3)
fname_supported_ftype   <- 'save_object.rds'
fname_unsupported_ftype <- 'save_object.rdata'
fpath_supported_ftype   <- file.path(dir_parent, fname_supported_ftype)
fpath_unsupported_ftype <- file.path(dir_parent, fname_unsupported_ftype)
# devtools::load_all()

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
              , regexp = paste("File already exists, not over-writing:", fpath_supported_ftype)
              
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
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            
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
            
          }
)

test_that("read_file reads a file",
          {
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            saveRDS(save_object, fpath_supported_ftype)
            expect_message(
              read_file(
                f_path = fpath_supported_ftype
                , verbose = TRUE)
              , regexp = paste("Reading file:", fpath_supported_ftype)
            )
            
          })

test_that("read_file errors correctly",
          {
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            save(save_object, file = fpath_unsupported_ftype)
            expect_error(
              read_file(
                f_path = fpath_unsupported_ftype
                , verbose = FALSE)
              , regexp = paste("This function only supports .* file extensions")
            )
          })

# FIXME SB - 2023 Nov 27 - this doesn't fix this persistent error, but works correctly
# Error in file() : cannot open the coError in file() : cannot open the connection
# In addition: Warning message:
# In file() :
#   cannot open file '/tmp/Rtmp8Lg0sx/Rfe703824c4fc55': No such file or directory
# Error in file(out, "wt") : cannot open the connectionn the connection

test_that("read_file errors correctly",
          {
            on.exit(unlink(dir_parent, recursive = TRUE), add = TRUE, after = FALSE)
            dir.create(dir_parent)
            save(save_object, file = fpath_unsupported_ftype)
            # expect_true(file.exists(fpath_unsupported_ftype)) # trying a reprex that doesn't use my functions
            expect_error(
              read_file(
                f_path = fpath_unsupported_ftype
                , verbose = FALSE)
              , regexp = paste("This function only supports .* file extensions")
            )
          })

# Last test
test_that("directory is actually gone",
          {
            expect_false(dir.exists(dir_parent))
          }
)
