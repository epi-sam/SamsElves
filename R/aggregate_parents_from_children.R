#' Aggregate from children to parents using a hierarchy
#'
#' Aggregate iteratively from leaf nodes up through an (assumed MECE) hierarchy
#' to the top level. Retain all child and parent values, e.g. for a location
#' hierarchy, retain all location_ids, and aggregate values up to the top
#' specified parent level.  This function is designed to be used iteratively,
#' starting at the leaf nodes and working up to the top level.  It will
#' aggregate all children of a parent, then aggregate those parents up to the
#' next level, and so on. Aggregation will stop at each level if aggregates are
#' not square.  If a parent location already exists in the data, this will check
#' for all.equal() between the parent and the aggregated children, and message
#' if v_verbose = TRUE, and throw an error if aa_hard_stop = TRUE.
#'
#' Relies on the `children_of_parents()` function to find children of a parent
#' hierarchy_id e.g. location_id, then aggregates the selected columns for all
#' children of one parent.
#'
#' @param DT [data.table] e.g. some data table with hierarchy_id as a column
#' @param varnames_to_aggregate [chr] e.g. c("mean", "upper", "lower")
#' @param varnames_to_aggregate_by [chr] e.g c("year_id", "age_group_id")
#' @param varname_weights [chr] (default NULL) - if you want to weight the
#'   aggregation by a variable, e.g. population.  If NULL, do a simple
#'   children-to-parent sum the values in varnames_to_aggregate within each
#'   combination of varnames_to_aggregate_by.  If not NULL, calculate weights
#'   for all children of each parent before aggregation.  Weights sum to 1
#'   between all children, within each combination of varnames_to_aggregate_by.
#' @param hierarchy [data.table] e.g. a location hierarchy with required
#'   columns: `hierarchy_id`, path_to_top_parent, level, most_detailed
#' @param hierarchy_id [chr] What variable does your hierarchy define, e.g.
#'   "location_id" (2024-11-21 only supported option)
#' @param stop_level [x] (default 3L) Stops aggregation when the child level ==
#'   stop_level (e.g. 3L aggregate up to national for locations, but no further;
#'   regional scalars mean regions are larger than combined countries under them
#'   from e.g. small islands)
#' @param require_square [lgl] (default TRUE) If TRUE, will check inputs and
#'   outputs for square (i.e. all variables are present for all combinations of
#' @param verbose [lgl] message each parent and children being aggregated?
#' @param v_verbose [lgl] message each parent that is not all.equal() to its
#'   aggregated children (if parent already exists in the dataset)?
#' @param tolerance_all_equal [dbl] (Default NULL uses all.equal's defaults)
#'   Tolerance for all.equal mean relative differnce check between parent and
#'   aggregated children (if parent is already in DT).  A value of 1 means the
#'   aggregated children are double the value of the parent (you probably did
#'   something wrong).  Use large values for large allowance in differnces due
#'   to rounding, etc.  Adjust the tolerance to your operation's mathematical
#'   limitations.
#' @param aa_hard_stop [lgl] (default FALSE) If TRUE, will stop if a parent is
#'   not all.equal() to its aggregated children, within user-specified level of
#'   tolerance.
#'
#' @return [data.table] aggregated data.table
#' @export
#'
aggregate_from_children_to_parents <- function(
    DT
    , varnames_to_aggregate
    , varnames_to_aggregate_by
    , varname_weights    = NULL
    , hierarchy
    , hierarchy_id        = "location_id"
    , stop_level          = 3L
    , require_square      = TRUE
    , verbose             = TRUE
    , v_verbose           = FALSE
    , tolerance_all_equal = NULL
    , aa_hard_stop        = FALSE
){

  # Arg Validations
  assert_scalar(x = hierarchy_id)
  if(!hierarchy_id == "location_id") stop("Only 'location_id' is currently supported for `hierarchy_id`")
  assert_scalar(x = stop_level)
  stopifnot(is.character(varnames_to_aggregate))
  stopifnot(is.character(varnames_to_aggregate_by))
  stopifnot(data.table::is.data.table(DT))
  stopifnot(data.table::is.data.table(hierarchy))
  varnames_hier <- c("path_to_top_parent", "level", "most_detailed", hierarchy_id)
  assert_x_in_y(varnames_hier
                , colnames(hierarchy))
  assert_x_in_y(c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by)
                , colnames(DT))
  if(hierarchy_id %in% varnames_to_aggregate) stop("hierarchy_id cannot be aggregated")
  if(hierarchy_id %in% varnames_to_aggregate_by) stop("hierarchy_id cannot be aggregated by")

  # Set a flag for whether we're aggregating with weights
  agg_msg <- "Aggregating"
  agg_with_weights <- !is.null(varname_weights)
  if(agg_with_weights) {
    stopifnot(is.character(varname_weights))
    stopifnot(length(varname_weights) == 1 & is.atomic(varname_weights))
    assert_x_in_y(varname_weights, colnames(DT))
    agg_msg <- paste0(agg_msg, " with weights: ", varname_weights)
  }

  # aggregation will drop all undefined variables - warn the user
  keep_vars    <- unique(c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by, varname_weights))
  non_agg_vars <- setdiff(colnames(DT), keep_vars)
  if (length(non_agg_vars)){
    message(paste("The following variables will be dropped during aggregation:", toString(non_agg_vars)))
  }

  # Select only what's necessary
  DT <- DT[, ..keep_vars]
  hierarchy <- hierarchy[, ..varnames_hier]
  # Assert completeness
  DT_incomplete <- DT[!complete.cases(DT)]
  hierarchy_incomplete <- hierarchy[!complete.cases(hierarchy)]
  if(nrow(DT_incomplete)){
    stop("aggregate_from_children_to_parents requires complete.cases() and data are incomplete, example: \n", prt_multiline(DT_incomplete[1, ]))
  }
  if(nrow(hierarchy_incomplete)){
    stop("aggregate_from_children_to_parents requires complete.cases() and hierarchy is incomplete, example: \n", prt_multiline(hierarchy_incomplete[1, ]))
  }

  assert_square(
    dt            = DT
    , id_varnames = unique(c(hierarchy_id, varnames_to_aggregate_by))
    , hard_stop   = require_square
    , verbose     = FALSE
  )

  setkeyv(DT, varnames_to_aggregate_by)

  # Define levels
  levels_rev <- sort(unique(hierarchy$level), decreasing = TRUE) # reverse hierarchy levels for bottom-up aggregation

  message(agg_msg)

  for (level_i in levels_rev){

    if(level_i == stop_level) {message("\nDone aggregating at level = ", level_i); break}

    parent_level <- level_i - 1
    message("\nChild level ", level_i, " to parent level ", parent_level, " (", max(levels_rev), " total)")

    # Outer loop: for each hierarchy level, starting at leaf nodes and going up
    # to a pre-specified level (3, counties), aggregate all children to their
    # parent level, then repeat and roll those up to the next parent level
    parents_of_level <- hierarchy[level == level_i - 1 & most_detailed == 0, location_id]
    loc_ids_i        <- hierarchy[level == level_i]$location_id
    parents_of_level <- parents_of_children(
      child_loc_ids  = loc_ids_i
      , hierarchy    = hierarchy
      , parent_level = parent_level
    )

    for (parent_i in parents_of_level){

      # Inner loop: Find children, aggregate selected columns for all children
      # of one parent location, reset the location_id to the parent_id, then
      # bind back on the temp data.table.

      children <- children_of_parents(
        parent_loc_ids   = parent_i
        , hierarchy      = hierarchy[level %in% c(level_i, parent_level)]
        , output         = "loc_ids"
        , include_parent = FALSE
      )

      if(verbose) message("- Parent: ", parent_i, "\n   - Children: ", toString(children))

      if (length(children) > 0){
        dt_children <- DT[get(hierarchy_id) %in% children]

        # If children aren't square, aggregation will go wrong
        withCallingHandlers(
          {
            # set this up to trigger a warning if the square check fails
            square_catch <- assert_square(
              dt            = dt_children
              , id_varnames = unique(c(hierarchy_id, varnames_to_aggregate_by))
              , hard_stop   = FALSE
              , verbose     = FALSE
            )
          }
          , warning = function(assert_square_cnd){
            assert_square_cnd
            message("Square check failed. Inspect parent and its children.")
            message(paste("level:", level_i, "parent:", parent_i, "children:", toString(children)))
          }
        )

        # Prepare weights
        if(agg_with_weights){
          # agggregate weights so they're available at the next level
          dt_child_weights_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varname_weights]
          dt_children[, (varname_weights) := lapply(.SD, function(x) x / sum(x)), .SDcols = varname_weights, by = varnames_to_aggregate_by]
          dt_children[, (varnames_to_aggregate) := lapply(.SD, function(x) x * get(varname_weights)), .SDcols = varnames_to_aggregate]
        }

        # Wow, we finally get to aggregate something
        dt_parent_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varnames_to_aggregate]

        if(agg_with_weights){
          if(!nrow(dt_child_weights_agg) == nrow(dt_parent_agg)){
            stop("Weights aggregation failed\n  -- " , paste("nrow child weights:",  nrow(dt_child_weights_agg), "\n  -- nrow parents aggregated:", toString(nrow(dt_parent_agg))))
          }
          dt_parent_agg <- merge(dt_parent_agg, dt_child_weights_agg, by = varnames_to_aggregate_by, all.x = TRUE)
        }

        dt_parent_agg[[hierarchy_id]] <- parent_i
        setcolorder(dt_parent_agg, keep_vars)

        # If the parent was already in DT, signal if it's not all.equal() to our aggregation
        if(parent_i %in% DT[[hierarchy_id]]){
          setkeyv(DT, varnames_to_aggregate_by)
          setkeyv(dt_parent_agg, varnames_to_aggregate_by)
          aa_args           <- list(target = DT[get(hierarchy_id) == parent_i], current = dt_parent_agg)
          # allow user to fine-tune all.equal or use default (NULL)
          aa_args$tolerance <- tolerance_all_equal # NULL handles itself
          catch_aa          <- do.call(all.equal, aa_args)
          not_aa <- !all(catch_aa == TRUE)
          if(not_aa & v_verbose) {
            message("   - Parent: ", parent_i, " already exists and is not all.equal() to aggregated children", " - ", toString(catch_aa))
          }
          if(not_aa & aa_hard_stop) stop("Parent is not all.equal() to aggregated children")
        }

        DT <- rbind(DT[!location_id == parent_i], dt_parent_agg, fill = TRUE)

      }
    }
  }

  assert_square(
    dt            = DT
    , id_varnames = unique(c(hierarchy_id, varnames_to_aggregate_by))
    , hard_stop   = require_square
    , verbose     = FALSE
  )
  setcolorder(DT, keep_vars)
  setorderv(DT, c(hierarchy_id, varnames_to_aggregate_by))
  return(DT)

}
