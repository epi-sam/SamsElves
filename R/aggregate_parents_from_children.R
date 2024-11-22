#' Aggregate from children to parents using a hierarchy
#'
#' Aggregate from leaf nodes up through an (assumed MECE) hierarchy to the top
#' level.
#'
#' Relies on the `children_of_parents()` function to find children of a parent
#' hierarchy_id e.g. location_id, then aggregates the selected columns for all
#' children of one parent.
#'
#' @param DT [data.table] e.g. some data table with hierarchy_id as a column
#' @param varnames_to_aggregate [chr] e.g. c("cases", "deaths")
#' @param varnames_to_aggregate_by [chr] e.g c("year", "age_group")
#' @param hierarchy [data.table] e.g. a location hierarchy with required
#'   columns: `hierarchy_id`, path_to_top_parent, level
#' @param hierarchy_id [chr] What variable does your hierarchy define, e.g.
#'   "location_id" (2024-11-21 only supported option)
#' @param stop_level [x] (default 3L) Stops aggregation when the child level ==
#'   stop_level (e.g. 3L aggregate up to national for locations, but no further;
#'   regional scalars mean regions are larger than combined countries under them
#'   from e.g. small islands)
#' @param tolerance_all_equal [dbl] (Default NULL uses all.equal's defaults)
#'   Tolerance for all.equal mean relative differnce check between parent and
#'   aggregated children (if parent is already in DT).  A value of 1 means the
#'   aggregated children are double the value of the parent (you probably did
#'   something wrong).  Use large values for large allowance in differnces due
#'   to rounding, etc.  Adjust the tolerance to your operation's mathematical
#'   limitations.
#' @param require_square [lgl] (default TRUE) If TRUE, will check inputs and
#'   outputs for square (i.e. all variables are present for all combinations of
#'   `hierarchy_id` and `varnames_to_aggregate_by`, and no rows are duplicated)
#'
#' @return [data.table] aggregated data.table
#' @export
#'
aggregate_from_children_to_parents <- function(
    DT
    , varnames_to_aggregate
    , varnames_to_aggregate_by
    , hierarchy
    , hierarchy_id        = "location_id"
    , stop_level          = 3L
    , tolerance_all_equal = NULL
    , require_square      = TRUE
){

  # Arg Validations
  assert_scalar(x = hierarchy_id)
  if(!hierarchy_id == "location_id") stop("Only 'location_id' is currently supported for `hierarchy_id`")
  assert_scalar(x = stop_level)
  stopifnot(is.character(varnames_to_aggregate))
  stopifnot(is.character(varnames_to_aggregate_by))
  stopifnot(is.data.table(DT))
  stopifnot(is.data.table(hierarchy))
  assert_x_in_y(c("path_to_top_parent", "level", hierarchy_id)
                , colnames(hierarchy))
  assert_x_in_y(c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by)
                , colnames(DT))

  # aggregation will drop all undefined variables - warn the user
  non_agg_vars <- setdiff(colnames(DT), c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by))
  if (length(non_agg_vars)){
    message(paste("The following variables will be dropped during aggregation:", toString(non_agg_vars)))
  }
  DT[, (non_agg_vars) := NULL]
  col_order_og <- colnames(DT)

  assert_square(
    dt            = DT
    , id_varnames = c(hierarchy_id, varnames_to_aggregate_by)
    , hard_stop   = require_square
    , verbose     = FALSE
  )

  # Define levels
  levels_rev <- sort(unique(hierarchy$level), decreasing = TRUE) # reverse hierarchy levels for bottom-up aggregation

  message("Aggregating:")

  for (level_i in levels_rev){

    parent_level <- level_i - 1
    message("\nChild level ", level_i, " to parent level ", parent_level, " (", max(levels_rev), " total)")

    # Outer loop: for each hierarchy level, starting at leaf nodes and going up
    # to a pre-specified level (3, counties), aggregate all children to their
    # parent level, then repeat and roll those up to the next parent level
    if(level_i == stop_level) {message("Done aggregating. Child level = ", level_i); break}
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

      message("- Parent: ", parent_i, "\n   - Children: ", toString(children))

      if (length(children) > 0){
        dt_children <- DT[get(hierarchy_id) %in% children]

        # If children aren't square, aggregation will go wrong
        withCallingHandlers(
          {
            # set this up to trigger a warning if the square check fails
            square_catch <- assert_square(
              dt            = dt_children
              , id_varnames = c(hierarchy_id, varnames_to_aggregate_by)
              , hard_stop   = FALSE
              , verbose     = FALSE
            )
          }
          , warning = function(assert_square_cnd){
            assert_square_cnd
            message(paste("level:", level_i, "parent:", parent_i, "children:", toString(children)))
            message("Square check failed. Inspect parent and its children.")
          }
        )

        # Wow, we finally get to aggregate something
        dt_parent_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varnames_to_aggregate]

        dt_parent_agg[[hierarchy_id]] <- parent_i
        setcolorder(dt_parent_agg, col_order_og)

        # If the parent was already in DT, signal if it's not all.equal() to our aggregation
        if(parent_i %in% DT[[hierarchy_id]]){
          # allow user to fine-tune all.equal or use default (NULL)
          aa_args           <- list(target = DT[get(hierarchy_id) == parent_i], current = dt_parent_agg)
          aa_args$tolerance <- tolerance_all_equal # NULL handles itself
          catch_aa          <- do.call(all.equal, aa_args)
          if(!all(catch_aa == TRUE)) message("Parent: ", parent_i, " already exists and is not all.equal() to aggregated children", " - ", toString(catch_aa))
        }

        DT <- rbind(DT[!location_id == parent_i], dt_parent_agg, fill = TRUE)

      }
    }
  }

  assert_square(
    dt            = DT
    , id_varnames = c(hierarchy_id, varnames_to_aggregate_by)
    , hard_stop   = require_square
    , verbose     = FALSE
  )
  setcolorder(DT, c(hierarchy_id, varnames_to_aggregate_by, varnames_to_aggregate))
  setorderv(DT, c(hierarchy_id, varnames_to_aggregate_by))
  return(DT)

}
