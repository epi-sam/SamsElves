if(FALSE){ # for debugging - tests use a different folder structure
  library(data.table)
  source("tests/testthat/fixtures/DT_draws.R")
}
source("/fixtures/DT_draws.R")

test_that("PERD_regex works",
          {
            expect_equal(
              "^draw_\\d{1,3}$|^point_estimate$",
              PERD_regex()
            )
            expect_equal(
              "^draw_\\d{1,3}$|^point_estimate$|^extra$",
              PERD_regex(additions = "^extra$")
            )
          })

test_that("order_draws works",
          {
            expect_equal(
              c(paste0("draw_", 0:10)),
              grep("draw_", (names(DT)), value = TRUE)
            )
          })

test_that("find_draws_varnames works", {
  expect_equal(
    c("point_estimate", paste0("draw_", 0:10)),
    find_draws_varnames(DT)
  )
})

test_that("find_draws_varnames fails well",
          {
            expect_error(
              find_draws_varnames(data.table::data.table(a = 1:5, b = 6:10)),
              "No draw/PE columns found"
            )
            expect_error(
              find_draws_varnames(data.frame(a = 1:5, b = 6:10)),
              "Assertion on 'DT' failed: Must be a data.table, not data.frame."
            )
          })

test_that("find_id_varnames works",
          {
            expect_equal(
              c("location_id", "year_id", "age_group_id", "sex_id", "metric_id", "run_id", "covariate_id"),
              find_id_varnames(DT, verbose = FALSE, removals = c("source", "measure_id"))
            )
          })

test_that("draws_wide_to_long works",
          {
            expect_no_error(
              DTL <- draws_wide_to_long(DT)
            )
            expect_equal(
              c("location_id", "year_id", "age_group_id", "sex_id", "metric_id", "source", "measure_id", "run_id", "covariate_id", "draw_id", "value"),
              names(DTL)
            )
          })

test_that("draws_wide_to_long back to wide works",
          {
            DTL <- draws_wide_to_long(DT)
            DT2 <- draws_long_to_wide(DTL)
            expect_equal(DT, DT2)
          })

test_that("draws_to_mean_ci works",
          {
            DT_pemlu <- draws_to_mean_ci(DT)
            DT_lu <- draws_to_mean_ci(DT, remove_point_estimate = T, remove_mean = T)
            expect_equal(
              c(0.769516373001214, 0.789962719334097, 0.809872699101758, 0.829846117961575, 0.849369551009887),
              DT_pemlu$mean
            )
            expect_contains(names(DT_pemlu), c("point_estimate", "mean", "lower", "upper"))
            expect_false(any(c("point_estimate", "mean") %in% names(DT_lu)))
          })


test_that("draws_year_diff works", {
  expect_equal(
    draws_year_diff(draws_wide_to_long(DT), yr_vec = c(1984, 1980)),
    data.table::data.table(
        location_id    = 8L,
        age_group_id   = 22L,
        sex_id         = 3L,
        metric_id      = 3L,
        source         = "ST-GPR",
        measure_id     = 18L,
        run_id         = "mv_20250811_foamy_livid_thyme",
        covariate_id   = 1980L,
        years          = "1980_1984",
        point_estimate = 0.08301997465775,
        mean           = 0.0798531780086733,
        lower          = 0.0109885252897033,
        upper          = 0.284009816015196
      )
  )
})


test_that("get_draw_pe_ui_difference works", {
  expect_equal(
    get_draw_pe_ui_difference(
      data.table::data.table(
        location_id    = 8L,
        age_group_id   = 22L,
        sex_id         = 3L,
        metric_id      = 3L,
        source         = "ST-GPR",
        measure_id     = 18L,
        draw_0 = 0.6,
        draw_1 = 0.4,
        draw_2 = 0.3,
        point_estimate = 0.42
      ),
      print_summary_stats = FALSE
    ),
    data.table::data.table(
      location_id    = 8L,
      age_group_id   = 22L,
      sex_id         = 3L,
      metric_id      = 3L,
      source         = "ST-GPR",
      measure_id     = 18L,
      point_estimate = 0.42,
      mean           = mean(c(0.6, 0.4, 0.3)),
      lower          = as.numeric(quantile(c(0.6, 0.4, 0.3), 0.025)),
      upper          = as.numeric(quantile(c(0.6, 0.4, 0.3), 0.975)),
      median         = median(c(0.6, 0.4, 0.3)),
      pe_percentile  = 2/3,
      point_estimate_in_ui = TRUE,
      pe_mean_difference   = 0.42 - mean(c(0.6, 0.4, 0.3)),
      pe_median_difference = 0.42 - median(c(0.6, 0.4, 0.3))
    )
  )
})
