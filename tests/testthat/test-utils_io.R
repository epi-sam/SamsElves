test_that("make_directory makes a directory and cleans up afterward", {
  
  dir_parent <- tempdir()
  dir_child <- 'temp_directory_1'
  dir_full <- file.path(dir_parent, dir_child)
  withr::local_dir(dir_parent)
  make_directory(dir_child)
  expect_true(dir.exists(dir_full))
})

# Using deprecated fuctions - shown for comparability - please retain
# test_that("make_directory makes a directory and cleans up afterward", {
#   withr::with_tempdir({
#     dir1 <- 'temp_directory_1'
#     make_directory(dir1)
#     expect_true(dir.exists(dir1))
#   })
# })

# test_that("save_file writes a file to disk", {
#   withr::with_tempfile()
# })