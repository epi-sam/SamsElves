library(data.table)
# If testing ever breaks, add data.table to imports for BOTH:
# - DESCRIPTION file
# - NAMESPACE file# Hierarchy with Washington, Arkansas, USA, Rajasthan, India, and Global
test_hier <- data.table::data.table(
  'location_id'        = c(570, 526, 102, 4868, 163, 1),
  'path_to_top_parent' = c('1,102,570',
                           '1,102,526',
                           '1,102',
                           '1,163,4868',
                           '1,163',
                           '1'),
  'level'              = c(3, 3, 2, 3, 2, 1)
)

fra_hier <- data.table::data.table(
  location_id = c(97896L, 60222L, 60245L, 60236L,
                  60250L, 60257L, 60260L, 60271L, 60277L, 60286L, 60292L, 97868L,
                  60319L, 60325L, 338L, 350L, 363L, 387L, 364L)
  , ihme_loc_id = c("FRA_97896", "FRA_60222", "FRA_60245", "FRA_60236", "FRA_60250", "FRA_60257",
                    "FRA_60260", "FRA_60271", "FRA_60277", "FRA_60286", "FRA_60292",
                    "FRA_97868", "FRA_60319", "FRA_60325", "FRA_338", "FRA_350",
                    "FRA_363", "FRA_387", "FRA_364")
  , level = c(3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 3L)
  , path_to_top_parent = c("1,64,73,97896", "1,64,73,97896,60222", "1,64,73,97896,60245", "1,64,73,97896,60236",
                           "1,64,73,97896,60250", "1,64,73,97896,60257", "1,64,73,97896,60260",
                           "1,64,73,97896,60271", "1,64,73,97896,60277", "1,64,73,97896,60286",
                           "1,64,73,97896,60292", "1,64,73,97896,97868", "1,64,73,97896,60319",
                           "1,64,73,97896,60325", "1,103,104,338", "1,103,104,350", "1,103,104,363",
                           "1,4,9,387", "1,166,174,364")
)

test_that("attach_location_id works",
          {
            # test_dt based on fra_hier - two rows with ihme_loc_id only
            test_dt <- data.table::data.table(
              'ihme_loc_id' = c('FRA_60222', 'FRA_350'),
              'some_data'   = c(1.5, 2.5)
            )
            test_dt_out <- attach_location_id(
              test_dt
              , hierarchy = fra_hier
              , loc_id_varname = 'location_id'
              , ihme_loc_varname = 'ihme_loc_id'
            )
            test_dt_out <- attach_location_id(
              as.data.frame(test_dt)
              , hierarchy = as.data.frame(fra_hier)
              , loc_id_varname = 'location_id'
              , ihme_loc_varname = 'ihme_loc_id'
            )
            expect_identical(
              c(350L, 60222L),
              test_dt_out$location_id
            )
            expect_identical(
              c('FRA_350', 'FRA_60222'),
              test_dt_out$ihme_loc_id
            )
            expect_identical(
              c(2.5, 1.5),
              test_dt_out$some_data
            )
          })

test_that("attach_location_id fails if ihme_loc_id is absent",
          {
            test_dt <- data.table::data.table(
              'location_id' = c(60222L, 350L),
              'some_data'   = c(1.5, 2.5)
            )
            expect_error(
              attach_location_id(
                test_dt
                , hierarchy = fra_hier
                , loc_id_varname = 'location_id'
                , ihme_loc_varname = 'ihme_loc_id'
              )
            )
          })

test_that("attach_ihme_loc_id works",
          {
            # same as above, but with ihme_loc_id
            test_dt <- data.table::data.table(
              'location_id' = c(60222L, 350L),
              'some_data'   = c(1.5, 2.5)
            )
            test_dt_out <- attach_ihme_loc_id(
              test_dt
              , hierarchy = fra_hier
              , loc_id_varname = 'location_id'
              , ihme_loc_varname = 'ihme_loc_id'
            )
            test_dt_out <- attach_ihme_loc_id(
              as.data.frame(test_dt)
              , hierarchy = as.data.frame(fra_hier)
              , loc_id_varname = 'location_id'
              , ihme_loc_varname = 'ihme_loc_id'
            )
            expect_identical(
              c(350L, 60222L),
              test_dt_out$location_id
            )
            expect_identical(
              c('FRA_350', 'FRA_60222'),
              test_dt_out$ihme_loc_id
            )
            expect_identical(
              c(2.5, 1.5),
              test_dt_out$some_data
            )
          })

test_that("attach_ihme_loc_id fails if location_id is absent",
          {
            test_dt <- data.table::data.table(
              'ihme_loc_id' = c('FRA_60222', 'FRA_350'),
              'some_data'   = c(1.5, 2.5)
            )
            expect_error(
              attach_ihme_loc_id(
                test_dt
                , hierarchy = fra_hier
                , loc_id_varname = 'location_id'
                , ihme_loc_varname = 'ihme_loc_id'
              )
            )
          })

test_that("attach_parent_location_id & attach_national_location_id work",
          {
            test_dt0 <- attach_parent_location_id(
              df = data.table::copy(test_hier)
              , hierarchy = test_hier
              , parent_level = 0
              , allow_self_as_parent = TRUE
            )
            test_dt1 <- attach_parent_location_id(
              df = data.table::copy(test_hier)
              , hierarchy = test_hier
              , parent_level = 1
              , allow_self_as_parent = TRUE
            )
            test_dt_nat <- attach_national_location_id(
              df = data.table::copy(fra_hier)
              , hierarchy = fra_hier
            )
            expect_identical(
              c(1L, 1L, 1L, 1L, 1L, 1L),
              test_dt0$parent_location_id
            )
            expect_identical(
              c(102L, 102L, 102L, 163L, 163L, NA),
              test_dt1$parent_location_id
            )
            expect_identical(
              c(97896L, 97896L, 97896L, 97896L, 97896L, 97896L, 97896L, 97896L,
                97896L, 97896L, 97896L, 97896L, 97896L, 97896L, 338L, 350L, 363L,
                387L, 364L),
              test_dt_nat$national_location_id
            )
          })

test_that("attach_parent_location_id fails correctly",
          {
            test_dt <- data.table::data.table(
              'location_id' = c(570, 526, 102),
              'some_data'   = c(1.5, 2.5, 3.5),
              'parent_location_id' = c(1, 1, 1)
            )
            expect_error(
              attach_parent_location_id(
                df = data.table::copy(test_dt)
                , hierarchy = test_hier
                , parent_level = 1
                , allow_self_as_parent = TRUE
                , new_varname = "parent_location_id"
              )
              , regexp = "forbidden in names\\(df\\) but present in new_varname: parent_location_id"
            )
          })

test_that("attach_national_ihme_loc_id works",
          {
            # two-column France table
            test_dt <- data.table::data.table(
              'location_id' = c(97896, 60222, 338),
              'some_data'   = c(1.5, 2.5, 3.5)
            )
            test_dt_out <- attach_national_ihme_loc_id(
              df = data.table::copy(test_dt)
              , hierarchy = fra_hier
              , allow_self_as_parent = TRUE
            )
            expect_identical(
              c( 'FRA_338', 'FRA_97896', 'FRA_97896'),
              test_dt_out$national_ihme_loc_id
            )
            test_dt_out <- attach_national_ihme_loc_id(
              df = as.data.frame(test_dt)
              , hierarchy = as.data.frame(fra_hier)
              , allow_self_as_parent = TRUE
            )
            expect_identical(
              c( 'FRA_338', 'FRA_97896', 'FRA_97896'),
              test_dt_out$national_ihme_loc_id
            )
          })

test_that("attach_national_ihme_loc_id fails when column exists",
          {
            test_dt <- data.table::data.table(
              'location_id'            = c(97896, 60222, 338),
              'some_data'              = c(1.5, 2.5, 3.5),
              'national_ihme_loc_id'   = c('FRA_97896', 'FRA_97896', 'FRA_338')
            )
            expect_error(
              attach_national_ihme_loc_id(
                dt = data.table::copy(test_dt)
                , hierarchy = fra_hier
                , allow_self_as_parent = TRUE
              )
            )
          })

