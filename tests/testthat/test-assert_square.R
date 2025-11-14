library(data.table)
# Test 1: Should PASS - Large square table
test_that("assert_square passes for large square table", {
  # Create a large square table: 100 x 100 x 50 = 500,000 rows
  dt_square <- data.table::CJ(
    id1 = 1:1000,
    id2 = 1:100,
    id3 = 1:50
  )
  dt_square[, value := rnorm(.N)]

  start_time <- Sys.time()
  expect_silent(
    assert_square(
      dt = dt_square,
      id_varnames = c("id1", "id2", "id3"),
      verbose = FALSE,
      hard_stop = TRUE
    )
  )
  end_time <- Sys.time()

  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(sprintf("Square table test completed in %.3f seconds", time_elapsed))

  # Should complete quickly (under 2 seconds for optimized version)
  expect_lt(time_elapsed, 2)
})

# Test 2: Should FAIL - Large table with duplicates
test_that("assert_square fails for large table with duplicates", {
  # Create a large table with duplicates: 100 x 100 x 50 = 500,000 rows + duplicates
  dt_duplicates <- data.table::CJ(
    id1 = 1:1000,
    id2 = 1:100,
    id3 = 1:50
  )
  dt_duplicates[, value := rnorm(.N)]

  # Add 1000 duplicate rows
  duplicates <- dt_duplicates[sample(.N, 1000), ]
  dt_duplicates <- data.table::rbindlist(list(dt_duplicates, duplicates))

  start_time <- Sys.time()
  expect_error(
    assert_square(
      dt = dt_duplicates,
      id_varnames = c("id1", "id2", "id3"),
      verbose = FALSE,
      hard_stop = TRUE
    ),
    regexp = "is not square"
  )
  end_time <- Sys.time()

  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(sprintf("Duplicate table test completed in %.3f seconds", time_elapsed))

  # Should complete quickly (under 1 second - duplicate check is fast)
  expect_lt(time_elapsed, 2)
})

# Test 3: Should FAIL - Large table with missing rows
test_that("assert_square fails for large table with missing rows", {
  # Create a large table with missing rows: 100 x 100 x 50 = 500,000 rows, remove 5000
  dt_missing <- data.table::CJ(
    id1 = 1:1000,
    id2 = 1:100,
    id3 = 1:50
  )
  dt_missing[, value := rnorm(.N)]

  # Remove 5000 random rows to create missing combinations
  rows_to_remove <- sample(nrow(dt_missing), 5000)
  dt_missing <- dt_missing[-rows_to_remove]

  start_time <- Sys.time()
  expect_error(
    assert_square(
      dt = dt_missing,
      id_varnames = c("id1", "id2", "id3"),
      verbose = FALSE,
      hard_stop = TRUE
    ),
    regexp = "is not square"
  )
  end_time <- Sys.time()

  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  message(sprintf("Missing rows test completed in %.3f seconds", time_elapsed))

  # Should complete reasonably fast (under 3 seconds with optimizations)
  expect_lt(time_elapsed, 2)
})
