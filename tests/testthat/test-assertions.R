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

