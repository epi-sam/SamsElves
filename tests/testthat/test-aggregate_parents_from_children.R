if(FALSE){ # for debugging - tests use a different folder structure
  library(data.table)
  DT   <- read_file("tests/testthat/fixtures/agg_data.csv")
  HIER <- read_file("tests/testthat/fixtures/agg_hier.csv")
}

library(data.table)
DT   <- read_file("fixtures/agg_data.csv")
HIER <- read_file("fixtures/agg_hier.csv")
ccroot <- Sys.getenv("CCROOT")
tryCatch(source(file.path(ccroot, "get_regional_scalars.R")), warning = function(cnd){
  warning("get_regional_scalars not sourced - check .Renviron for CCROOT - some tests will be skipped")
})

test_that("aggregate_from_children_to_parents start/stop levels function correctly", {

  stopifnot(max(HIER$level) == 5)

  expect_no_error(
    DT_regions <- aggregate_from_children_to_parents(
      DT                         = DT
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , varname_weights          = "wt_val"
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , start_level              = 5
      , stop_level               = 2
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
      , aa_hard_stop             = TRUE
      , tolerance_all_equal      = 1
    )
  )

  # In theory, apply regional scalars to level 2 locations here

  expect_no_error(
    DT_super_regions <- aggregate_from_children_to_parents(
      DT                         = DT_regions
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , varname_weights          = "wt_val"
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , start_level              = 2
      , stop_level               = 0
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
      , aa_hard_stop             = TRUE
      , tolerance_all_equal      = 1
    )
  )
  expect_no_error(
    DT_agg_all_levels <- aggregate_from_children_to_parents(
      DT                         = DT
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , varname_weights          = "wt_val"
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , start_level              = 5
      , stop_level               = 0
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
      , aa_hard_stop             = TRUE
      , tolerance_all_equal      = 1
    )
  )

  # Since we didn't apply regional scalars, these two should be equal.

  expect_equal(DT_super_regions, DT_agg_all_levels)

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

if(exists("get_regional_scalars", envir = .GlobalEnv)){

  # User must source get_regional_scalars from central functions for this to work
  # - the path to this function is proprietary

  test_that("apply_regional_scalars works", {

    expect_no_error(
      DT_agg_scaled <- aggregate_from_children_to_parents(
        DT                         = DT
        , varnames_to_aggregate    = c("mean", "lower", "upper")
        , varnames_to_aggregate_by = c("year_id")
        , hierarchy                = HIER
        , hierarchy_id             = "location_id"
        , stop_level               = 0
        , add_regional_scalars     = TRUE
        , varname_weights          = "wt_val"
        , release_id               = 34
        , location_set_id          = 35
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = FALSE
      )
    )

    expect_no_error(
      DT_agg_unscaled <- aggregate_from_children_to_parents(
        DT                         = DT
        , varnames_to_aggregate    = c("mean", "lower", "upper")
        , varnames_to_aggregate_by = c("year_id")
        , hierarchy                = HIER
        , hierarchy_id             = "location_id"
        , stop_level               = 0
        , add_regional_scalars     = FALSE
        , varname_weights          = "wt_val"
        , release_id               = 34
        , location_set_id          = 35
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = FALSE
      )
    )

    expect_equal(
      DT_agg_scaled[location_id %in% HIER[level>2, location_id]]
      , DT_agg_unscaled[location_id %in% HIER[level>2, location_id]]
    )

    expect_equal(
      all.equal(
        DT_agg_scaled[location_id %in% HIER[level<=2, location_id]]
        , DT_agg_unscaled[location_id %in% HIER[level<=2, location_id]]
      )
      , "Column 'mean': Mean relative difference: 0.009014079"
    )

  })
} else {
  test_that("apply_regional_scalars throws expected error", {

    expect_error(
      DT_agg_scaled <- aggregate_from_children_to_parents(
        DT                         = DT
        , varnames_to_aggregate    = c("mean", "lower", "upper")
        , varnames_to_aggregate_by = c("year_id")
        , hierarchy                = HIER
        , hierarchy_id             = "location_id"
        , stop_level               = 0
        , add_regional_scalars     = TRUE
        , varname_weights          = "wt_val"
        , release_id               = 34
        , location_set_id          = 35
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = FALSE
      )
      , regexp = "add_regional_scalars requires user to source get_regional_scalars\\(\\) from central functions"
    )

  })
}

test_that("most_detailed and missing location error systems work", {

  DT_MD <- merge(
    DT
    , HIER[most_detailed==1, .(location_id)]
    , by = "location_id"
    , all.y = TRUE
  )

  expect_no_error(
    DT_agg <- aggregate_from_children_to_parents(
      DT                         = DT_MD
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , stop_level               = 3
      , require_all_most_detailed = TRUE
      , verbose = FALSE
    )
  )

  expect_error({
    DT_agg <- aggregate_from_children_to_parents(
      DT                         = DT_MD[1:50] # random subset
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , stop_level               = 3
      , require_all_most_detailed = TRUE
      , verbose = FALSE
    )
  }, regexp = "required in hierarchy\\[most_detailed == 1, location_id\\] but absent in DT\\[\\[hierarchy_id\\]\\]"
  )

  # Naive attempt to roll up leaf locations, when some are most_detailed at
  # higher levels
  DT_agg <- aggregate_from_children_to_parents(
    DT                          = DT[location_id %in% HIER[level == 5, location_id]]
    , varnames_to_aggregate     = c("mean", "lower", "upper")
    , varnames_to_aggregate_by  = c("year_id")
    , hierarchy                 = HIER
    , hierarchy_id              = "location_id"
    , stop_level                = 3
    , require_all_most_detailed = FALSE
    , verbose                   = FALSE
  ) %>%
    expect_message(regexp = "level = 4; parent = 51; children = 53660, 53661") %>%
    expect_error(regexp = "dt_children has no rows")

  # allow user to override, but not by default
  DT_agg <- aggregate_from_children_to_parents(
    DT                          = DT[location_id %in% HIER[level == 5, location_id]]
    , varnames_to_aggregate     = c("mean", "lower", "upper")
    , varnames_to_aggregate_by  = c("year_id")
    , hierarchy                 = HIER
    , hierarchy_id              = "location_id"
    , stop_level                = 3
    , require_all_most_detailed = FALSE
    , require_rows              = FALSE # user override
    , verbose                   = FALSE
  )  %>% expect_no_error()


})
