test_that("is_empty produces correct results and warnings", {

  expect_warning(
    expect_equal(
      is_empty(c('a', NA, 'c'))
      , c(FALSE, TRUE, FALSE)
    )
    , "x has NA"
  )

  expect_warning(
    expect_equal(
      is_empty(character())
      , c(TRUE)
    )
    , "x has length 0"
  )

  expect_warning(
    expect_equal(
      is_empty(c('a', '', 'c'))
      , c(FALSE, TRUE, FALSE)
    )
    , "x has an empty character string"
  )

  expect_warning(
    expect_equal(
      is_empty(c('a', '  ', 'c'))
      , c(FALSE, TRUE, FALSE)
    )
    , "x has a whitespace character string"
  )

  expect_warning(
    expect_equal(
      is_empty(NULL)
      , c(TRUE)
    )
    , "x is NULL"
  )

  expect_equal(
    is_empty(c('a', 'b', 'c'))
    , c(FALSE, FALSE, FALSE)
  )

  expect_equal(
    is_empty(c('a', '  b  ', 'c'))
    , c(FALSE, FALSE, FALSE)
  )

})

test_that("is_cli_null works", {
  expect_equal(
    sapply(list(a = "a", b = 1:5, c = NULL, d = c("", "    ")), is_cli_null),
    c('a' = FALSE, 'b' = FALSE, 'c' = TRUE, 'd' = TRUE)
  )
})
