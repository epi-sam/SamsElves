# This allows testthat to function properly
# If testing ever breaks, add data.table to imports for BOTH:
# - DESCRIPTION file
# - NAMESPACE file
.datatable.aware=TRUE

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

#' @title parents_of_children
#'
#' @description Given a vector of location IDs, returns the vector of their unique parent IDs at a given level.
#'
#' @param child_loc_ids [int] Vector of location IDs to pull parent ID for
#' @param hierarchy [data.table] Hierarchy. Must have columns location_id, path_to_top_parent, and level.
#' @param parent_level [int] Single level of the hierarchy - find all parent location_ids of child location_ids at this level
#' @returns [int] vector of _unique_ parent location_ids
#' @export
parents_of_children <- function(
    child_loc_ids,
    hierarchy,
    parent_level
){
  return(unique(sapply(child_loc_ids, function(x) parent_of_child(x, hierarchy, parent_level))))
}


#' Return a vector of parent location_ids at level x based on path_to_top_parent
#'
#' Faster, vectorized version parents_of_children. Assumes `path_to_top_parent`
#' string is structured left-to-right as: path_to_top_parent = "1,10,30,70,5555"
#' levels             =  0, 1, 2, 3,   4
#'
#' @param child_loc_id_vec [int] vector of location_ids
#' @param hierarchy [data.frame] ihme location hierarchy (get_location_metadata)
#' @param parent_level_vec [int] vector of levels for the parent of each
#'   location to return.  must be length 1 (find parents of all child locations
#' @param allow_self_as_parent [lgl: defautl FALSE] if TRUE, allow location_id
#'   at parent_level at a single level) or length(child_loc_id_vec)
#' @returns [int] vector of parent location_ids, NA if none found at a given
#'   level
#' @export
parents_of_children_vec <- function(
    child_loc_id_vec,
    hierarchy,
    parent_level_vec,
    allow_self_as_parent = FALSE
){

  checkmate::assert_integerish(child_loc_id_vec)
  checkmate::assert_vector(child_loc_id_vec)
  checkmate::assert_data_frame(hierarchy)
  vars_hier_req <- c('path_to_top_parent', 'location_id', 'level')
  checkmate::assert_subset(vars_hier_req, choices = names(hierarchy))
  checkmate::assert_integerish(parent_level_vec)
  checkmate::assert_vector(parent_level_vec)
  checkmate::assert_logical(allow_self_as_parent, len = 1)
  if(length(parent_level_vec) == 1) parent_level_vec <- rep(parent_level_vec, length(child_loc_id_vec))
  checkmate::assert_true(length(child_loc_id_vec) == length(parent_level_vec))

  hier_sub <- subset(hierarchy, select = vars_hier_req) # safe for data.frame or data.table

  # Faster, vectorized paradigm. Using data.table's merge is ESSENTIAL to retain
  # original order so vectorized assignment of output is correct, even with
  # missing values.
  # Using base R merge() is NOT safe - it sorts NAs to the bottom by default
  # This is WRONG
  # grid <- merge(
  #   x                 = data.frame(location_id = child_loc_id_vec)
  #   , y               = as.data.frame(hier_sub)
  grid <- data.table::merge.data.table(
    x                 = data.table::data.table(location_id = child_loc_id_vec)
    , y               = data.table::as.data.table(hier_sub)
    , by              = "location_id"
    , all.x           = TRUE
    , allow.cartesian = FALSE
    , sort            = FALSE
  )

  pttp_deconstructed <- strsplit(grid$path_to_top_parent, ",", fixed = TRUE)
  parents_at_lvl <- lapply(seq_along(pttp_deconstructed), function(idx) {
    # return(pttp_deconstructed[[idx]] [parent_level_vec[[idx]] + 1])
    # +1 because global (location_id 1) is level 0 (level is zero-indexed)
    lvl_idx <- parent_level_vec[[idx]] + 1
    len_pttp <- length(pttp_deconstructed[[idx]])
    parent_id <- pttp_deconstructed[[idx]] [lvl_idx]
    if(allow_self_as_parent == FALSE & lvl_idx == len_pttp) parent_id <- NA_integer_
    return(parent_id)
  })
  parent_loc_id_vec <- as.integer(unlist(parents_at_lvl))

  checkmate::assert_vector(parent_loc_id_vec)
  checkmate::assert_true(length(child_loc_id_vec) == length(parent_loc_id_vec))

  return(parent_loc_id_vec)

}


