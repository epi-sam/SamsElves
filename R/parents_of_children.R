# This allows testthat to function properly
# If testing ever breaks, add data.table to imports for BOTH:
# - DESCRIPTION file
# - NAMESPACE file
.datatable.aware=TRUE

#' @title parents_of_children
#'
#' @description Given a vector of location IDs, returns the vector of their unique parent IDs at a given level.
#'
#' @param child_loc_ids [int] Vector of location IDs to pull parent ID for
#' @param hierarchy [data.table] Hierarchy. Must have columns location_id, path_to_top_parent, and level.
#' @param parent_level [int] Single level of the hierarchy - find all parent location_ids of child location_ids at this level
#' @export
parents_of_children <- function(
    child_loc_ids,
    hierarchy,
    parent_level
){
  return(unique(sapply(child_loc_ids, function(x) parent_of_child(x, hierarchy, parent_level))))
}

#' @title parent_of_child
#'
#' @description Given a location ID, returns its parent ID at a given level.
#'
#' @param child_location_id [int] Location ID to pull parent ID for
#' @param hierarchy [data.table] Hierarchy. Must have columns location_id, path_to_top_parent, and level.
#' @param parent_level [int] Single level of the hierarchy - find all parent location_ids of child location_ids at this level
#'
#' @return [int] location_id for a single parent of a single child location
parent_of_child <- function(
    child_location_id,
    hierarchy,
    parent_level
) {
  validate_parents_of_children_inputs(child_location_id, hierarchy, parent_level)

  all_parents = hierarchy[location_id == child_location_id, path_to_top_parent]
  all_parents = as.integer(unlist(strsplit(all_parents, ",")))

  for (parent in all_parents){
    # Loop through all parents of this child.
    # If the parent you're checking exists on parent_level, then return.
    if (hierarchy[location_id == parent, level] == parent_level){
      return(parent)
    }
  }
  # Ideally, this never triggers, but if it does you at least have a good error message.
  stop(sprintf("Oops, something went wrong inside parents_of_children! Revisit inputs:
       child_location_id %i \n
       hierarchy %s \n
       parent_level %i", child_location_id, head(hierarchy), parent_level))
}

#' Helper function to validate inputs to function
#'
#' @param child_location_id [int] ihme location_id
#' @param hierarchy [data.table] ihme location hierarchy
#' @param parent_level [int] ihme location level
#'
#' @return [none] stop on failure
validate_parents_of_children_inputs <- function(child_location_id, hierarchy, parent_level){
  # Check for valid parent_level
  if(length(parent_level) != 1){
    stop("Please specify a single parent level")
  }
  if (!parent_level %in% unique(hierarchy$level)){
    stop(sprintf("Level is not available in hierarchy. Available levels are %s",
                 paste0(unique(hierarchy$level), collapse = ",")))
  }

  # Check for valid hierarchy
  if(!"package:data.table" %in% search()) stop("data.table must be loaded for parents_of_children()")
  if(!isTRUE(data.table::is.data.table(hierarchy))) stop("hierarchy must be a data.table")
  if (!all(c('path_to_top_parent', 'location_id', 'level') %in% names(hierarchy))){
    stop("Was passed an invalid hierarchy. Must have columns path_to_top_parent, location_id, and level.")
  }

  # Check that child_location_id is valid
  child_hier <- hierarchy[location_id == child_location_id]
  if (nrow(child_hier) == 0){
    stop("Child location is not in hierarchy!")
  }

  # Check that child and parent are compatible
  child_level = child_hier$level[1]
  if (child_level <= parent_level){
    stop(sprintf("Parent level %i is greater than or equal to child level %i.", parent_level, child_level))
  }
}
