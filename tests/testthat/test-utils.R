# vec_to_comma_string ----------------------------------------------------------
test_that("vec_to_comma_string works", {
  expect_equal(vec_to_comma_string(1:10), "1,2,3,4,5,6,7,8,9,10")
})

# comma_string_to_vec ----------------------------------------------------------
test_that("comma_string_to_vec works", {
  expect_equal(comma_string_to_vec("1,2,3,4,5,6,7,8,9,10"), as.character(1:10))
})


# is_sequential_int_vec --------------------------------------------------------
test_that("is_sequential_int_vec works", {
  expect_true(is_sequential_int_vec(1:10))
  expect_false(is_sequential_int_vec(c(1L, 3L, 2L)))
})

test_that("is_sequence_int_vec throws correct errors", {
  expect_error(is_sequential_int_vec(c(1L, 2L, NA, 4L)), "NA values are not allowed")
  expect_error(is_sequential_int_vec(c(1, 2, 3, 5)), 'is.vector\\(x, mode = "integer"\\) is not TRUE')
})
