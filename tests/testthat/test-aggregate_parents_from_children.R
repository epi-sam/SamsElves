if(FALSE){ # for debugging - tests use a different folder structure
  DT   <- fread("tests/testthat/fixtures/agg_data.csv")
  HIER <- fread("tests/testthat/fixtures/agg_hier.csv")
}

library(data.table)
DT   <- fread("fixtures/agg_data.csv")
HIER <- fread("fixtures/agg_hier.csv")

test_that("aggregate_from_children_to_parents does not double output", {

  expect_no_error(
    DT_agg <- aggregate_from_children_to_parents(
      DT                         = DT
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , varname_weights          = "wt_val"
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , stop_level               = 0
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
      , aa_hard_stop             = TRUE
      , tolerance_all_equal      = 1
    )
  )

  test_that("aggregated global values are sum of most detailed locations", {

    md_locs <- HIER[most_detailed == 1, location_id]
    expect_equal(
      DT[location_id %in% md_locs, sum(mean*wt_val)]
      , DT_agg[location_id==1, mean*wt_val]
    )
  })

})

test_that("aggregate_from_children_to_parents all-equal tolerance check stops correctly",{
  expect_error({
    expect_message({
      DT_agg <- aggregate_from_children_to_parents(
        DT = DT
        , varnames_to_aggregate    = c("mean", "lower", "upper")
        , varnames_to_aggregate_by = c("year_id")
        , varname_weights          = "wt_val"
        , hierarchy                = HIER
        , hierarchy_id             = "location_id"
        , stop_level               = 0
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = TRUE
        , aa_hard_stop             = TRUE
        , tolerance_all_equal      = 0.15
      )
    }, regexp = "Parent: 214 already exists and is not all.equal\\(\\) to aggregated children - Column 'lower': Mean relative difference: 0.1828952")
  }, regexp = "Parent is not all.equal\\(\\) to aggregated children")
})
