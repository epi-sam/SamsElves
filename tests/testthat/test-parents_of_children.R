# This allows testthat to function properly while not importing data.table formally
# - must also add `.datatable.aware=TRUE` to at least one R/* file
# If testing ever breaks, add data.table to imports for BOTH:
# - DESCRIPTION file
# - NAMESPACE file
library(data.table)
# Hierarchy with Washington, Arkansas, USA, Rajasthan, India, and Global
test_hier = data.table::data.table(
  'location_id'        = c(570, 526, 102, 4868, 163, 1),
  # 'path_to_top_parent' = c('570,102,1',
  #                          '526,102,1',
  #                          '102,1',
  #                          '4868,163,1',
  #                          '163,1',
  #                          '1'),
  'path_to_top_parent' = c('1,102,570',
                           '1,102,526',
                           '1,102',
                           '1,163,4868',
                           '1,163',
                           '1'),
  'level'              = c(3, 3, 2, 3, 2, 1)
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
      c(NA, NA, NA, NA, 570L, 163L),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 6:1
      )
    )
    expect_identical(
      c(NA, NA, NA, 526L, 570L, 4868L),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = rep(2, length(test_hier$location_id))
      )
    )
    expect_identical(
      c(NA, NA, NA, 526L, 570L, 4868L),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 2
      )
    )

    expect_identical(
      c(NA, 102L, 163L, 102L, 102L, 163L),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 1
      )
    )

    expect_identical(
      c(1L, 1L, 1L, 1L, 1L, 1L),
      parents_of_children_vec(
        child_loc_id_vec   = test_hier$location_id
        , hierarchy        = test_hier
        , parent_level_vec = 0
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

