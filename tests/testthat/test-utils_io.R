
# Common test cruft

dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)
save_object <- list(a = 1, b = 2, c = 3)
fpath_csv   <- file.path(dir_parent, 'save_object.csv')
fpath_h5    <- file.path(dir_parent, 'save_object.h5')

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
          })

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
                      , f_path = fpath_csv
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)
            expect_true(file.exists(fpath_csv))
          }
)

test_that("save_file forbids overwrite",
          {
            
            withr::local_file(dir_parent)
            dir.create(dir_parent)
            
            save_file(object = save_object
                      , f_path = fpath_csv
                      , forbid_overwrite = TRUE
                      , verbose = FALSE)
            
            expect_message(
              save_file(object = save_object
                        , f_path = fpath_csv
                        , forbid_overwrite = TRUE
                        , verbose = FALSE)
              , regexp = paste("File already exists, not over-writing :", fpath_csv)
              
            ) 
            
          }
)

test_that("directory is actually gone",
          {
            expect_false(dir.exists(dir_parent))
          })