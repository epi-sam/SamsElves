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
#' @param varname_weights [chr: default NULL] - if you want to weight the
#'   aggregation by a variable, e.g. population.  If NULL, do a simple
#'   children-to-parent sum the values in varnames_to_aggregate within each
#'   combination of varnames_to_aggregate_by.  If not NULL, calculate weights
#'   for all children of each parent before aggregation.  Weights sum to 1
#'   between all children, within each combination of varnames_to_aggregate_by.
#' @param hierarchy [data.table: default "location_id"] e.g. a location
#'   hierarchy with required columns: `hierarchy_id`, path_to_top_parent, level,
#'   most_detailed
#' @param hierarchy_id [chr] What variable does your hierarchy define, e.g.
#'   "location_id" (2024-11-21 only supported option)
#' @param start_level [int: default max(hierarchy$level)] What level to start
#'   aggregating
#' @param stop_level [int: default 3] Stops aggregation when the child level ==
#'   stop_level (e.g. 3L aggregate up to national for locations, but no further;
#'   regional scalars mean regions are larger than combined countries under them
#'   from e.g. small islands)
#' @param add_regional_scalars [lgl: default FALSE] If TRUE, will add regional
#'   scalars (e.g. GBD Regions) to the hierarchy, and aggregate them
#'   automatically.  This assumes your hierarchy is a location hierarchy with
#'   location_id, path_to_top_parent, level, most_detailed columns.
#' @param release_id [int: default NULL] required only if add_regional_scalars =
#'   TRUE
#' @param location_set_id [int: default 35] required only if
#'   add_regional_scalars = TRUE
#' @param require_square [lgl: default TRUE] If TRUE, will check inputs and
#'   outputs for square (i.e. all variables are present for all combinations of
#'   hierarchy_id and varnames_to_aggregate_by).  If FALSE, will warn if not
#'   square.
#' @param require_rows [lgl: default TRUE] If TRUE, assert_square checks data
#'   has > 0 rows
#' @param verbose [lgl: default TRUE] message each parent and children being
#'   aggregated?
#' @param v_verbose [lgl: default FALSE] message each parent that is not
#'   all.equal() to its aggregated children (if parent already exists in the
#'   dataset)?
#' @param tolerance_all_equal [dbl: default NULL] If NULL, use all.equal's
#'   default tolerance for all.equal mean relative differnce check between
#'   parent and aggregated children (if parent is already in DT).  A value of 1
#'   means the aggregated children are double the value of the parent (you
#'   probably did something wrong).  Use large values for large allowance in
#'   differnces due to rounding, etc.  Adjust the tolerance to your operation's
#'   mathematical limitations.
#' @param aa_hard_stop [lgl: default FALSE] If TRUE, stop if a parent is not
#'   `all.equal()` to its aggregated children, within user-specified level of
#'   tolerance.
#' @param aggregate_proportions [lgl: default FALSE] If TRUE, only apply
#'   regional scalars to varname_weights
#' @param require_all_most_detailed [lgl: default TRUE] If TRUE, require that
#'   all most_detailed locations in the hierarchy are present in DT
#'
#' @return [data.table] aggregated data.table
#' @export
#'
aggregate_from_children_to_parents <- function(
    DT
    , varnames_to_aggregate
    , varnames_to_aggregate_by
    , hierarchy
    , hierarchy_id              = "location_id"
    , start_level               = max(hierarchy$level)
    , stop_level                = 3L
    , varname_weights           = NULL
    , aggregate_proportions     = FALSE
    , add_regional_scalars      = FALSE
    , release_id                = NULL
    , location_set_id           = 35
    , require_square            = TRUE
    , require_rows              = TRUE
    , require_all_most_detailed = TRUE
    , verbose                   = TRUE
    , v_verbose                 = FALSE
    , tolerance_all_equal       = NULL
    , aa_hard_stop              = FALSE
 ){

  # Arg Validations
  checkmate::assert_character(hierarchy_id, len = 1)
  if(!hierarchy_id == "location_id") stop("Only 'location_id' is currently supported for `hierarchy_id`")
  checkmate::assert_integerish(start_level, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(stop_level, len = 1, any.missing = FALSE)
  checkmate::assert_character(varnames_to_aggregate, any.missing = FALSE, min.len = 1)
  checkmate::assert_character(varnames_to_aggregate_by, any.missing = FALSE, min.len = 1)
  checkmate::assert_data_table(DT)
  checkmate::assert_data_table(hierarchy)
  if(add_regional_scalars == TRUE ){
    if(hierarchy_id != "location_id")
      stop("add_regional_scalars only designed for location hierarchies")
    if(!stop_level %in% 0L:2L)
      stop("add_regional_scalars requires aggregation to level 2 (GBD Region) or above")
    if(!exists("get_regional_scalars", envir = .GlobalEnv))
      stop("add_regional_scalars requires user to source get_regional_scalars() from central functions")
  }

  varnames_hier <- c("path_to_top_parent", "level", "most_detailed", hierarchy_id)
  assert_x_in_y(varnames_hier, colnames(hierarchy))
  assert_x_in_y(
    c(hierarchy_id, varnames_to_aggregate, varnames_to_aggregate_by)
    , colnames(DT)
  )
  if(hierarchy_id %in% varnames_to_aggregate) stop(sprintf("%s cannot be an aggregated variable", hierarchy_id))
  if(hierarchy_id %in% varnames_to_aggregate_by) stop(sprintf("cannot aggregate by %s", hierarchy_id))
  if(start_level > max(hierarchy$level)) stop("start_level cannot be greater than max(hierarchy$level)")

  if(require_all_most_detailed == TRUE){
    assert_x_in_y(hierarchy[most_detailed == 1, location_id], DT[[hierarchy_id]])
  }

  measure_range             <- range(unlist(DT[, ..varnames_to_aggregate]))
  measure_likely_proportion <- all(min(measure_range) >= -1L & max(measure_range) <= 1L)
  if(measure_likely_proportion && is.null(varname_weights)){
    warning("
It looks like you're aggregating proportions without weights!

- If you want to aggregate proportions, set `varname_weights`,
  - e.g. to a population variable
")
  }

  # Set start message depending on if we're aggregating with weights
  agg_msg <- ""
  agg_with_weights <- !is.null(varname_weights)
  if(agg_with_weights) {
    checkmate::assert_character(varname_weights, len = 1, any.missing = FALSE)
    assert_x_in_y(varname_weights, colnames(DT))
    agg_msg <- sprintf("\n - with weights: %s", varname_weights)
  }

  # aggregation will drop all undefined variables - warn the user
  keep_vars    <- unique(c(hierarchy_id, varnames_to_aggregate_by, varname_weights, varnames_to_aggregate))
  non_agg_vars <- setdiff(colnames(DT), keep_vars)
  if (length(non_agg_vars) & verbose){
    message("\nAggregation keeps only hierarchy_id, varnames_to_aggregate_by, varname_weights, varnames_to_aggregate")
    message(sprintf("\nThe following variables will be dropped during aggregation: %s", toString(non_agg_vars)))
  }

  # Select only what's necessary
  DT <- DT[, ..keep_vars]
  hierarchy <- hierarchy[, ..varnames_hier]
  # Assert completeness
  DT_incomplete <- DT[!complete.cases(DT)]
  hierarchy_incomplete <- hierarchy[!complete.cases(hierarchy)]
  if(nrow(DT_incomplete) > 0){
    stop("aggregate_from_children_to_parents requires complete.cases() and data are incomplete, example: \n", prt_multiline(DT_incomplete[1, ]))
  }
  if(nrow(hierarchy_incomplete) > 0){
    stop("aggregate_from_children_to_parents requires complete.cases() and hierarchy is incomplete, example: \n", prt_multiline(hierarchy_incomplete[1, ]))
  }

  assert_square(
    dt              = DT
    , id_varnames   = unique(c(hierarchy_id, varnames_to_aggregate_by))
    , hard_stop     = require_square
    , stop_if_empty = require_rows
    , verbose       = FALSE
  )

  data.table::setkeyv(DT, varnames_to_aggregate_by)

  # reverse hierarchy levels for bottom-up aggregation
  levels_rev <- sort(start_level:stop_level, decreasing = TRUE)

  message("\nAggregating from level ", levels_rev[1], " to ", levels_rev[length(levels_rev)], agg_msg)

  # Outer Loop - Levels --------------------------------------------------------
  for (level_i in levels_rev){

    # Regional Scalars -----------------------------------------------------
    if(level_i == 2 && add_regional_scalars == TRUE){
      message("\n --- Applying regional scalars at level 2--- \n")

      vars_to_multiply <- c(varnames_to_aggregate, varname_weights)
      # only multiply weights if we're aggregating proportions
      if(aggregate_proportions == TRUE) vars_to_multiply <- varname_weights

      # quality check
      if(aggregate_proportions == FALSE && measure_likely_proportion == TRUE){
        warning("
It looks like you're aggregating proportions with regional scalars!

 - YOU HAVE NOT set `aggregate_proportions = TRUE` and probably should!

 - If you want to aggregate proportions with scalars:
   - set `aggregate_proportions = TRUE`
   - only the weights will be multiplied by regional scalars")
      }

      DT <- apply_regional_scalars(
        DT               = DT,
        vars_to_multiply = vars_to_multiply,
        release_id       = release_id,
        location_set_id  = location_set_id
      )
    }

    if(level_i == stop_level) {message("\nDone aggregating at level = ", level_i); break}

    parent_level <- level_i - 1
    message("\nChild level ", level_i, " to parent level ", parent_level, " (", max(levels_rev), " levels total)")

    # Outer loop: for each hierarchy level, starting at leaf nodes and going up
    # to a pre-specified level (e.g. 3 = counties), aggregate all children to
    # their parent level, then repeat and roll those up to the next parent
    # level.

    parents_of_level <- parents_of_children(
      # child_loc_ids  = loc_ids_i
      child_loc_ids  = hierarchy[level == level_i]$location_id
      , hierarchy    = hierarchy
      , parent_level = parent_level
    )

    # Temporary data.table to hold all parents at this level before binding
    DT_level <- data.table::data.table()

    # Inner Loop - Locations ---------------------------------------------------
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

      if (length(children) > 0) {
        dt_children <- DT[get(hierarchy_id) %in% children]

        # If children aren't square, aggregation will go wrong
        withCallingHandlers(
          {
            # set this up to trigger a helpful warning if the square check fails
            square_catch <- assert_square(
              dt              = dt_children
              , id_varnames   = unique(c(hierarchy_id, varnames_to_aggregate_by))
              , hard_stop     = FALSE
              , stop_if_empty = require_rows
              , verbose       = FALSE
            )
          }
          , warning = function(assert_square_cnd){
            assert_square_cnd
            message("\nSquare check warning. Inspect parent and its children.")
            message(sprintf("level = %s; parent = %s; children = %s \n", level_i, parent_i, toString(children)))
          }
          , error = function(assert_square_cnd){
            assert_square_cnd
            message("\nSquare check failed. Inspect parent and its children.")
            message(sprintf("level = %s; parent = %s; children = %s \n", level_i, parent_i, toString(children)))
          }
        )

        # Weights --------------------------------------------------------------
        if(agg_with_weights){
          # agggregate weights so they're available at the next level
          dt_child_weights_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varname_weights]
          dt_children[, (varname_weights) := lapply(.SD, function(x) x / sum(x)), .SDcols = varname_weights, by = varnames_to_aggregate_by]
          dt_children[, (varnames_to_aggregate) := lapply(.SD, function(x) x * get(varname_weights)), .SDcols = varnames_to_aggregate]
        }

        # Aggregate ------------------------------------------------------------
        dt_parent_agg <- dt_children[, lapply(.SD, function(x) sum(x)), by = varnames_to_aggregate_by, .SDcols = varnames_to_aggregate]

        if(agg_with_weights){
          if(!nrow(dt_child_weights_agg) == nrow(dt_parent_agg)){
            stop("Weights aggregation failed\n  -- " , paste("nrow child weights:",  nrow(dt_child_weights_agg), "\n  -- nrow parents aggregated:", toString(nrow(dt_parent_agg))))
          }
          dt_parent_agg <- merge(dt_parent_agg, dt_child_weights_agg, by = varnames_to_aggregate_by, all.x = TRUE)
        }

        # some parents/children may not have any rows - use set()
        data.table::set(dt_parent_agg, j = hierarchy_id, value = parent_i)
        data.table::setcolorder(dt_parent_agg, keep_vars)

        # all.equal() Check ----------------------------------------------------
        # If the parent was already in DT, signal if it's not all.equal() to our aggregation
        if(parent_i %in% DT[[hierarchy_id]]){
          data.table::setkeyv(DT, varnames_to_aggregate_by)
          data.table::setkeyv(dt_parent_agg, varnames_to_aggregate_by)
          aa_args           <- list(target = DT[get(hierarchy_id) == parent_i], current = dt_parent_agg)
          # allow user to fine-tune all.equal or use default (NULL)
          aa_args$tolerance <- tolerance_all_equal # NULL handles itself
          catch_aa          <- do.call(all.equal, aa_args)
          not_aa            <- !all(catch_aa == TRUE)

          if(not_aa & v_verbose) {
            message("   - Parent: ", parent_i, " already exists and is not all.equal() to aggregated children", " - ", toString(catch_aa))
          }
          if(not_aa & aa_hard_stop) {
            stop(sprintf("Parent (%s) is not all.equal() to aggregated children (%s)\n - %s"
                         , parent_i, toString(children), toString(catch_aa)))
          }
        }

        DT_level <- data.table::rbindlist(list(DT_level, dt_parent_agg), use.names = TRUE, fill = TRUE)
        # DT <- rbind(DT[!location_id == parent_i], dt_parent_agg, fill = TRUE)

      } # End If Children Exist

    } # End Inner Loop - Locations

    # Bind aggregated parents at this level back onto DT
    DT <- data.table::rbindlist(
      list(DT[!location_id %in% parents_of_level], DT_level)
      , use.names = TRUE
      , fill = TRUE
    )

  } # End Outer Loop - Levels


  # Assert & Return ------------------------------------------------------------
  assert_square(
    dt              = DT
    , id_varnames   = unique(c(hierarchy_id, varnames_to_aggregate_by))
    , hard_stop     = require_square
    , stop_if_empty = require_rows
    , verbose       = FALSE
  )
  data.table::setcolorder(DT, keep_vars)
  data.table::setorderv(DT, c(hierarchy_id, varnames_to_aggregate_by))
  return(DT)

}
