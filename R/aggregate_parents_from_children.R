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
#' @param hierarchy [data.table] e.g. a location hierarchy with required columns: `hierarchy_id`, path_to_top_parent, level
#' @param stop_level [x] (default 0L) According to hierarchy levels, what is the top level?
#' @param hierarchy_id [chr] e.g. "location_id" (currently only supported option)
#' @param varnames_to_aggregate [chr] e.g. c("cases", "deaths")
#' @param varnames_to_aggregate_by [chr] e.g c("year", "age_group")
#'
#' @return [data.table] aggregated data.table
#' @export
#'
aggregate_from_children_to_parents <- function(
    DT
    , hierarchy
    , varnames_to_aggregate
    , varnames_to_aggregate_by
    , stop_level = 0L
    , hierarchy_id = "location_id"
){

  # Arg Validations
  assert_scalar(x = stop_level)
  assert_scalar(x = hierarchy_id)
  stopifnot(is.character(varnames_to_aggregate))
  stopifnot(is.character(varnames_to_aggregate_by))
  stopifnot(is.data.table(DT))
  stopifnot(is.data.table(hierarchy))
  assert_all_in(
    c("location_id")
    , hierarchy_id
  )
  assert_all_in(
    c("path_to_top_parent", "level", hierarchy_id)
    , colnames(hierarchy)
  )
  assert_all_in(
    c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by)
    , colnames(DT)
  )

  # aggregation will drop all undefined variables - warn the user
  non_agg_vars <- setdiff(colnames(DT), c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by))
  if (length(non_agg_vars)){
    message(paste("The following variables will be dropped during aggregation:", toString(non_agg_vars)))
  }
  DT[, (non_agg_vars) := NULL]
  col_order_og <- colnames(DT)

  # Define levels
  levels_rev <- sort(unique(hierarchy$level), decreasing = TRUE) # reverse hierarchy levels for bottom-up aggregation

  for (level_i in levels_rev){

    message("\nLevel: ", level_i, " of ", max(levels_rev))

    # Outer loop: for each hierarchy level, starting at leaf nodes and going up
    # to a pre-specified level (3, counties), aggregate all children to their
    # parent level, then repeat and roll those up to the next parent level
    if(level_i == stop_level) {message("Done aggregating."); break}
    parent_level     <- level_i - 1
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

      message("-- Parent: ", parent_i, " Children: ", toString(children))

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
            stop("Square check failed. Inspect parent and its children.")
          }
        )

        # Wow, we finally get to aggregate something
        dt_parent_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varnames_to_aggregate]
        dt_parent_agg[[hierarchy_id]] <- parent_i
        setcolorder(dt_parent_agg, col_order_og)

        # If the parent was already in DT, signal if it's not all.equal() to our aggregation
        if(parent_i %in% DT[[hierarchy_id]]){
          catch_aa <- all.equal(DT[get(hierarchy_id) == parent_i], dt_parent_agg)
          if(!all(catch_aa == TRUE)) message("Parent: ", parent_i, " already exists and is not all.equal() to aggregated children", " - ", toString(catch_aa))
        }

        DT <- rbind(DT[!location_id == parent_i], dt_parent_agg, fill = TRUE)

      }
    }
  }

  assert_square(
    dt            = DT
    , id_varnames = c(hierarchy_id, varnames_to_aggregate_by)
    , hard_stop   = FALSE
    , verbose     = FALSE
  )
  setcolorder(DT, c(hierarchy_id, varnames_to_aggregate_by, varnames_to_aggregate))
  setorderv(DT, c(hierarchy_id, varnames_to_aggregate_by))
  return(DT)

}
