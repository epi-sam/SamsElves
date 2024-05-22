# assert_no_null_arguments -----------------------------------------------------
test_fun <- function(arg_a){
  return(5*arg_a)
}

test_fun2 <- function(arg_b, arg_c){
  assert_no_null_arguments()
  return(arg_b * arg_c)
}

test_that(
  "assert_no_null_arguments() - test_fun() without assertion behaves as expected",
  {
    expect_error(
      test_fun(),
      regexp = 'argument "arg_a" is missing, with no default'
    )

    expect_equal(
      test_fun(NULL),
      as.numeric(NULL)
    )

    expect_equal(
      test_fun(4),
      20
    )
  }
)

test_that(
  "assert_no_null_arguments() produces expected errors",
  {
    expect_error(
      test_fun2(),
      regexp = "One or more of your arguments are still undefined symbols \\(likely missing an argument\\):\narg_b, arg_c"
    )

    expect_error(
      test_fun2(NULL),
      regexp = "One or more of your arguments are still undefined symbols \\(likely missing an argument\\):\narg_c"
    )

    expect_error(
      test_fun2(4),
      regexp = "One or more of your arguments are still undefined symbols \\(likely missing an argument\\):\narg_c"
    )

    expect_error(
      test_fun2(NULL, NULL),
      regexp = "One or more of your arguments is NULL - please recheck function inputs:\narg_b, arg_c"
    )

    expect_error(
      test_fun2("a", "b"),
      regexp = "non-numeric argument to binary operator"
    )
  }
)

test_that(
  "assert_no_null_argments does not throw an error",
  {
    expect_equal(
      test_fun2(4, 5),
      20
    )
  }
)

# assert_named_list ------------------------------------------------------------

test_that(
  "assert_named_list() throws the proper error",
  {
    expect_error(
      assert_named_list(list()),
      regexp = "must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )

    expect_error(
      assert_named_list(data.frame(a = 1, b = 2)),
      regexp = "must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )

    expect_error(
      assert_named_list(c(1, 2, 3)),
      regexp = "must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )

    expect_error(
      assert_named_list(list(1, b = 2)),
      regexp = "must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )
  }
)

test_that(
  "assert_named_list() does not throw an error",
  {
    expect_no_error(assert_named_list(list(a = 1, b = 2)))
    expect_no_error(assert_named_list(data.frame(a = 1, b = 2), allow_data_frame = TRUE))

  }
)

# assert_list_elements_and_types -----------------------------------------------

test_that(
  "assert_list_elements_and_types() throws the proper errors",
  {
    expect_error(
      assert_list_elements_and_types(data.frame(a = 1, b = 2), list(a = "numeric", b = "character")),
      regexp = "check_list must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )

    expect_error(
      assert_list_elements_and_types(list(a = 1, b = 2), list("numeric", b = "character")),
      regexp = "check_items must be a named list, not vector or data.frame \\(list names may not be whitespace\\).  If you want to allow a data.frame, set allow_data_frame = TRUE."
    )

    expect_message(
      expect_error(
        assert_list_elements_and_types(list(a = 1, b = 2), list(a = "numeric", b = "character")),
        regexp = "The following check_list data types did not match the check_items data types: a, b"
      )
    )

    expect_error(
      assert_list_elements_and_types(list(a = 1, b = 2), list(a = "double", b = "double", c = "double")),
      regexp = "The following check_items were not found in check_list: c"
    )

  }
)

test_that(
  "assert_list_elements_and_types() functions properly",
  {
    expect_no_error(
      assert_list_elements_and_types(list(a = 1, b = "2"), list(a = "double", b = "character"))
    )
    expect_no_error(
      assert_list_elements_and_types(data.frame(a = 1, b = "2"), list(a = "double", b = "character"), allow_data_frame = TRUE)
    )
    expect_no_error(
      assert_list_elements_and_types(data.frame(a = 1, b = "2"), list(a = NA, b = "character"), allow_data_frame = TRUE)
    )
  }
)
