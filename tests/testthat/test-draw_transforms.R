DT <- structure(
  list(
    location_id = c(8L, 8L, 8L, 8L, 8L),
    year_id = 1980:1984,
    age_group_id = c(22L, 22L, 22L, 22L, 22L),
    sex_id = c(3L, 3L, 3L, 3L, 3L),
    metric_id = c(3L, 3L, 3L, 3L, 3L),
    source = c("ST-GPR", "ST-GPR", "ST-GPR", "ST-GPR", "ST-GPR"),
    measure_id = c(18L, 18L, 18L, 18L, 18L),
    run_id = c(
      "mv_20250811_foamy_livid_thyme",
      "mv_20250811_foamy_livid_thyme",
      "mv_20250811_foamy_livid_thyme",
      "mv_20250811_foamy_livid_thyme",
      "mv_20250811_foamy_livid_thyme"
    ),
    covariate_id = c(1980L, 1980L, 1980L, 1980L, 1980L),
    point_estimate = c(
      0.76786172010929,
      0.789403093077112,
      0.810735329096779,
      0.83144085415834,
      0.85088169476704
    ),
    draw_0 = c(
      0.328883323736333,
      0.401848482861997,
      0.483091582041407,
      0.569295176151834,
      0.656706107760595
    ),
    draw_1 = c(
      0.962987354265359,
      0.968066133988126,
      0.972613402638404,
      0.976600679895695,
      0.98008200925015
    ),
    draw_2 = c(
      0.806061174358152,
      0.816762744551109,
      0.824271028922251,
      0.832419313184391,
      0.842779848514555
    ),
    draw_3 = c(
      0.900215948273571,
      0.915029814724308,
      0.926852982949778,
      0.937516702568494,
      0.94657841457804
    ),
    draw_4 = c(
      0.688902069784794,
      0.735562841574355,
      0.777494562669581,
      0.813330717614729,
      0.841472981772792
    ),
    draw_5 = c(
      0.848244146245703,
      0.876289392048594,
      0.902464034687275,
      0.924955525330043,
      0.943314336044124
    ),
    draw_6 = c(
      0.883143611740563,
      0.889882601191688,
      0.892256462660133,
      0.89301334369812,
      0.892096760465237
    ),
    draw_7 = c(
      0.850039214423413,
      0.865837631530599,
      0.879823118756255,
      0.893992180481257,
      0.906704000888152
    ),
    draw_8 = c(
      0.520473023800859,
      0.513518537521799,
      0.512346673506167,
      0.520438028146182,
      0.539176850279277
    ),
    draw_9 = c(
      0.836134476330173,
      0.860246578909709,
      0.883299162471652,
      0.90363736141777,
      0.920709999799099
    ),
    draw_10 = c(
      0.839595760054433,
      0.846545153772785,
      0.854086678816431,
      0.863108269088809,
      0.873443751756738
    )
  ),
  row.names = c(NA, -5L),
  class = c("data.table", "data.frame")
)

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
