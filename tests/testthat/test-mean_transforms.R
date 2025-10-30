test_that("means_year_compare works", {

  DT_test <- data.table::data.table(
    location_id = c(1,1,1,2,2,2)
    , year_id = c(2000,2001,2003,2000,2001,2003)
    , mean = c(10,12,14,20,22,24)
  )

  # 2 years
  DT_result <- means_year_compare(
    DT                = DT_test
    , yr_vec          = c(2000,2003)
    , central_varname = "mean"
    , id_varnams      = c("location_id", "year_id")
  )

  DT_expected <- data.table::data.table(
    location_id = c(1,2)
    , mean_2000 = c(10,20)
    , mean_2003 = c(14,24)
  )

  testthat::expect_equal(DT_result, DT_expected)

  # many years
  DT_result <- means_year_compare(
    DT                = DT_test
    , yr_vec          = 2000:2003
    , central_varname = "mean"
    , id_varnams      = c("location_id", "year_id")
  )

  DT_expected <- data.table::data.table(
    location_id = c(1,2)
    , mean_2000 = c(10,20)
    , mean_2001 = c(12,22)
    , mean_2003 = c(14,24)
  )

  testthat::expect_equal(DT_result, DT_expected)

})
