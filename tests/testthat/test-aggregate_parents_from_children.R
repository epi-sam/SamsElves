# 2024 Nov 20 - old method used parent_id, new method uses path_to_top_parent - finish later

# library(data.table)
# test_that("aggregate_from_children_to_parents works as expected: simple case", {
#   # In this simple example, 98 and 99 are the children of parent 0.
#   # There are no NAs.
#
#   sample_data = data.table(
#     'value' = c(70, 80, 66, 55, 41),
#     'location_id' = c(98, 98, 99, 99, 0),
#     'parent_id' = c(0, 0, 0, 0, 42),
#     'date' = rep('2022-01-01', 5)
#   )
#
#   hierarchy = data.table(
#     'level' = c(4, 4, 3),
#     'location_id' = c(98, 99, 0),
#     'parent_id' = c(0, 0, 42),
#     'most_detailed' = c(1, 1, 0),
#     'region_name' = rep('somewhere', 3),
#     'location_name' = rep('nowhere', 3)
#   )
#
#   expected = data.table(
#     'value' = c(70, 80, 66, 55, 271),
#     'location_id' = c(98, 98, 99, 99, 0),
#     'parent_id' = c(0, 0, 0, 0, 42),
#     'date' = rep('2022-01-01', 5)
#   )
#
#   result =  aggregate_from_children_to_parents(
#     DT = sample_data
#     , hierarchy = hierarchy
#     , varnames_to_aggregate_by = c("date")
#     , varnames_to_aggregate = c('value')
#   )
#
#   # TODO: Do you want the function to subset to the original columns it was passed in?
#   result = result[, .(value, location_id, parent_id, date)]
#   expect_equal(result, expected)
# })
#
#
#
# test_that("aggregate_to_parents works as expected: harder case", {
#   # In this simple example, there are levels 3, 4, and 5 in the hierarchy.
#   # There are NAs in the data.
#
#   sample_data = data.table(
#     'value' = c(7, NA, 22, 70, 80, 66, NA, 41),
#     'location_id' = c(12, 12, 13, 98, 98, 99, 99, 0),
#     'parent_id' = c(99, 99, 99, 0, 0, 0, 0, 42),
#     'date' = rep('2022-01-01', 8)
#   )
#
#   hierarchy = data.table(
#     'level' = c(5, 5, 4, 4, 3),
#     'location_id' = c(12, 13, 98, 99, 0),
#     'parent_id' = c(99, 99, 0, 0, 42),
#     'most_detailed' = c(1, 1, 0, 0, 0),
#     'region_name' = rep('somewhere', 5),
#     'location_name' = rep('nowhere', 5)
#   )
#
#   # This is assuming NAs are treated like zeros.
#   # TODO: Would you expect one result for ID 98 here?
#   expected = data.table(
#     'value' = c(7, NA, 22, 150, 66, 216),
#     'location_id' = c(12, 12, 13, 98, 99, 0),
#     'parent_id' = c(99, 99, 99, 0, 0, 42),
#     'date' = rep('2022-01-01', 6)
#   )
#
#   result =  aggregate_from_children_to_parents(
#     DT = sample_data
#     , hierarchy = hierarchy
#     , varnames_to_aggregate_by = c("date")
#     , varnames_to_aggregate = c('value')
#   )
#
#   # TODO: Do you want the function to subset to the original columns it was passed in?
#   result = result[, .(value, location_id, parent_id, date)]
#   expect_equal(result, expected)
# })
