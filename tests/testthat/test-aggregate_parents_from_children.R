if(FALSE){ # for debugging - tests use a different folder structure
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

# NOTE! - aggregating upper/lower is mathematically wrong - it is only done here
# to facilitate testing aggregation across multiple columns.
# NOTE! - all data are proportions, so aggregating by wt_val is required for
# tests.

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
  ) %>%
    expect_message(regexp = "Checking all.equal\\(\\) of parents to aggregated children at level = 4; parent = 214; children = 53660, 53661") %>%
    expect_error(regexp = "Parent \\(214\\) is not all.equal\\(\\) to aggregated children \\(25318, 25319")
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

  test_that("proportion data throws expected warnings", {

    result <- evaluate_promise(
      aggregate_from_children_to_parents(
        DT                         = DT
        , varnames_to_aggregate    = c("mean", "lower", "upper")
        , varnames_to_aggregate_by = c("year_id")
        , hierarchy                = HIER
        , hierarchy_id             = "location_id"
        , stop_level               = 2
        , add_regional_scalars     = TRUE
        , aggregate_proportions    = FALSE
        , release_id               = 34
        , location_set_id          = 35
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = FALSE
      )
    )
    expect_length(result$warnings, 2)
    expect_match(result$warnings[1], "It looks like you're aggregating proportions without weights!")
    expect_match(result$warnings[2], "It looks like you're aggregating proportions with regional scalars!")
  })

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
        , aggregate_proportions    = TRUE
        # , aggregate_proportions    = FALSE
        # , start_level              = 2
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
        , aggregate_proportions    = TRUE
        , release_id               = 34
        , location_set_id          = 35
        , require_square           = TRUE
        , verbose                  = FALSE
        , v_verbose                = FALSE
      )
    )

    # Debugging
    # DT_comp <- merge(
    #   DT_agg_scaled[,.(location_id, year_id, wt_val, mean)]
    #   , DT_agg_unscaled[,.(location_id, year_id, wt_val, mean)]
    #   , by = c("location_id", "year_id")
    #   , suffixes = c("_scaled", "_unscaled")
    #   , all.x = TRUE
    #   , all.y = TRUE
    # )
    # DT_comp[, mean_diff := mean_scaled - mean_unscaled]
    # DT_comp[, wt_diff := wt_val_scaled - wt_val_unscaled]
    # DT_comp[mean_diff != 0 | wt_diff != 0]


    expect_equal(
      DT_agg_scaled[location_id %in% HIER[level>2, location_id]]
      , DT_agg_unscaled[location_id %in% HIER[level>2, location_id]]
    )

    expect_equal(
      all.equal(
        DT_agg_scaled[location_id %in% HIER[level<=2, location_id]]
        , DT_agg_unscaled[location_id %in% HIER[level<=2, location_id]]
      )
      , "Column 'wt_val': Mean relative difference: 0.0006289781"
    )

    # Ensure internal and manual scalar application yield identical results
    regional_scalars <- get_regional_scalars(
      release_id        = 34
      , location_set_id = 35
      , year_id         = unique(DT$year_id)
    )

    DT_agg_manual_1 <- aggregate_from_children_to_parents(
      DT                         = DT
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , start_level              = 5
      , stop_level               = 2L
      , add_regional_scalars     = FALSE
      , varname_weights          = "wt_val"
      , aggregate_proportions    = TRUE
      , release_id               = 34
      , location_set_id          = 35
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
    )

    DT_agg_manual_1 <- merge(
      DT_agg_manual_1
      , regional_scalars[, .(location_id, year_id, scalar = mean)]
      , by = c("location_id", "year_id")
      , all.x = TRUE
    )
    DT_agg_manual_1[is.na(scalar), scalar := 1]
    measures <- c("wt_val")
    DT_agg_manual_1[, (measures) := lapply(.SD, function(x) x * scalar), .SDcols = measures]
    DT_agg_manual_1[, scalar := NULL]

    DT_agg_manual_2 <- aggregate_from_children_to_parents(
      DT                         = DT_agg_manual_1
      , varnames_to_aggregate    = c("mean", "lower", "upper")
      , varnames_to_aggregate_by = c("year_id")
      , hierarchy                = HIER
      , hierarchy_id             = "location_id"
      , start_level              = 2L
      , stop_level               = 0L
      , add_regional_scalars     = FALSE
      , varname_weights          = "wt_val"
      , aggregate_proportions    = TRUE
      , release_id               = 34
      , location_set_id          = 35
      , require_square           = TRUE
      , verbose                  = FALSE
      , v_verbose                = FALSE
    )

    # some weird index thing with data.table - check as data.frame
    expect_equal(
      as.data.frame(DT_agg_scaled)
      , as.data.frame(DT_agg_manual_2)
    )

    # Debugging
    # DT_comp_scalars <- merge(
    #   DT_agg_scaled[,.(location_id, year_id, wt_val, mean)]
    #   # DT_agg_manual_1[,.(location_id, year_id, wt_val, mean)]
    #   , DT_agg_manual_2[,.(location_id, year_id, wt_val, mean)]
    #   , by = c("location_id", "year_id")
    #   # , suffixes = c("_scaled", "_unscaled")
    #   , all.x = TRUE
    #   , all.y = TRUE
    # )
    # DT_comp_scalars[, mean_diff := mean.x - mean.y]
    # DT_comp_scalars[, wt_diff := wt_val.x - wt_val.y]
    # DT_comp_scalars[mean_diff != 0 | wt_diff != 0]
    # DT_comp[mean_diff != 0 | wt_diff != 0]

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
      , varname_weights          = "wt_val"
      , stop_level               = 3
      , aggregate_proportions = TRUE
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
      , varname_weights          = "wt_val"
      , stop_level               = 3
      , aggregate_proportions = TRUE
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
    expect_warning(regexp = "It looks like you're aggregating proportions without weights!") %>%
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
  )  %>%
    expect_warning(regexp = "It looks like you're aggregating proportions without weights!") %>%
    expect_message(regexp = "Aggregating from level 5 to 3") %>%
    expect_no_error()


})
