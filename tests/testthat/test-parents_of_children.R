# This allows testthat to function properly while not importing data.table formally
# - must also add `.datatable.aware=TRUE` to at least one R/* file
# If testing ever breaks, add data.table to imports for BOTH:
# - DESCRIPTION file
# - NAMESPACE file
library(data.table)
# Hierarchy with Washington, Arkansas, USA, Rajasthan, India, and Global
test_hier = data.table::data.table(
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

test_that(
  "parents_of_children works",
  {
    # A single child
    res <- parents_of_children(
      child_loc_ids = 102,
      hierarchy     = test_hier,
      parent_level  = 1
    )
    expect_equal(res, 1)

    # Multiple children of the same parent
    res <- parents_of_children(
      child_loc_ids = c(102, 570),
      hierarchy     = test_hier,
      parent_level  = 1
    )
    expect_equal(res, c(1))

    # Multiple children with different parents
    res <- parents_of_children(
      child_loc_ids = c(526, 4868),
      hierarchy     = test_hier,
      parent_level  = 2
    )
    expect_equal(res, c(102, 163))
  }
)

test_that(
  "parents_of_children throws expected errors",
  {
    expect_error(
      parents_of_children(
        child_loc_ids = c(102),
        hierarchy     = test_hier,
        parent_level  = -2
      ), regexp       = "Level is not available in hierarchy"
    )

    expect_error(
      parents_of_children(
        child_loc_ids = c(102),
        hierarchy     = test_hier,
        parent_level  = c(1, 2)
      ), regexp       = "Please specify a single parent level"
    )

    bad_hierarchy <- data.table::copy(test_hier)
    bad_hierarchy$path_to_top_parent <- NULL
    expect_error(
      parents_of_children(
        child_loc_ids = c(102),
        hierarchy     = bad_hierarchy,
        parent_level  = 1
      ), regexp       = "Was passed an invalid hierarchy"
    )

    expect_error(
      parents_of_children(
        child_loc_ids = c(777),
        hierarchy     = test_hier,
        parent_level  = 1
      ), regexp       = "Child location is not in hierarchy!"
    )

    expect_error(
      parents_of_children(
        child_loc_ids = 102,
        hierarchy     = test_hier,
        parent_level  = 3
      ), regexp       = "Parent level 3 is greater than or equal to child level 2."
    )
  }
)

test_that(
  "parents_of_children_vec works",
  {
    expect_identical(
      c(102L, 526L, NA, NA, NA, NA),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 1:6
      )
    )
    expect_identical(
      c(570L, 526L, NA, 4868L, NA, NA),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = rep(2, length(test_hier$location_id))
      )
    )

    expect_identical(
      c(102L, 102L, 102L, 163L, 163L, NA),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 1
      )
    )

  }
)

test_that(
  "parents_of_children_vec throws expected errors",
  {
    expect_error(
      parents_of_children_vec(
        child_loc_id_vec   = test_hier
        , hierarchy        = test_hier
        , parent_level_vec = 2
      )
      , regexp = "Must be of type 'integerish'"
    )
    expect_error(
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier$location_id
        , parent_level_vec = 2
      )
      , regexp = "Must be a data.table"
    )
    expect_error(
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = "apple"
      )
      , regexp = "Must be of type 'integerish'"
    )
    expect_error(
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = rep(2,2)
      )
      , regexp = "Assertion on 'length\\(child_loc_id_vec\\) == length\\(parent_level_vec\\)' failed: Must be TRUE."
    )
  }
)

test_that(
  "attach_parent_location_id works",
  {
    test_dt0 <- attach_parent_location_id(
      dt = copy(test_hier)
      , hierarchy = test_hier
      , parent_level = 0
    )
    test_dt1 <- attach_parent_location_id(
      dt = copy(test_hier)
      , hierarchy = test_hier
      , parent_level = 1
    )
    test_dt_nat <- attach_national_location_id(
      dt = copy(fra_hier)
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
      test_dt_nat$parent_location_id
    )
  }
)
