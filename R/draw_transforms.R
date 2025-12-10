#' Regex to identify draw columns + point estimate + anything else the user deems helpful
#'
#' @param include_PE [lgl] include point_estimate in regex?
#' @param additions [chr] additional regex to include
#'
#' @returns [chr] regex to identify draw columns + point estimate
#' @export
PERD_regex <- function(include_PE = TRUE, additions = NULL) {
  checkmate::assert_logical(include_PE, len = 1)
  checkmate::assert_character(additions, len = 1, null.ok = TRUE)
  if(include_PE == TRUE) additions <- c("^point_estimate$", additions)
  return(paste(c("^draw_\\d{1,3}$", additions), collapse = "|"))
}


#' Order vector of draw names in order of their numeric indices.  PERD
#' compliant.
#'
#' e.g. "draw_1", "draw_2", ..., "draw_10", "draw_11", ...
#'
#' @param draw_varnames [chr] vector of variable names
#' @param pe_varname [chr: default {"point_estimate"}] name of point estimate variable, if present
#'
#' @returns [chr] ordered vector of variable names
#' @export
order_draws <- function(draw_varnames, pe_varname = "point_estimate"){
  checkmate::assert_character(draw_varnames, any.missing = FALSE, min.len = 1)
  pe_present      <- pe_varname %in% draw_varnames
  vars_draws_only <- grep(PERD_regex(include_PE = FALSE, additions = NULL), draw_varnames, value = TRUE)
  vars_other      <- setdiff(draw_varnames, c(vars_draws_only, pe_varname))
  draws_present   <- length(vars_draws_only) > 0
  ret_vars        <- vars_other
  if (draws_present == TRUE) {
    vars_draws_only <- vars_draws_only[order(as.integer(sub("^draw_", "", vars_draws_only)))]
    ret_vars        <- c(ret_vars, vars_draws_only)
  }
  if(pe_present == TRUE){
    ret_vars <- c(pe_varname, ret_vars)
  }
  return(ret_vars)

}


#' Find draw variable names in a data.table
#'
#' Stops if no draw/PE columns found
#'
#' @param DT [data.table]
#' @param include_PE [lgl: default TRUE] include point_estimate in search? Passed to PERD_regex()
#' @param additions [chr: default NULL] additional regex to include. Passed to PERD_regex()
#'
#' @returns [chr] vector of variable names
#' @export
find_draws_varnames <- function(DT, include_PE = TRUE, additions = NULL) {
  checkmate::assert_data_table(DT)
  draws_rgx     <- PERD_regex(include_PE = include_PE, additions = additions)
  perd_varnames <- grep(draws_rgx, colnames(DT), value = TRUE)
  if(!length(perd_varnames) >= 1) stop("No draw/PE columns found")
  # sort the PE up front for visibility
  return(order_draws(perd_varnames))
}

#' Util to find a potential set of id_varnames for a data.table full of draws.
#'
#' Finds non-value variable names by excluding draw/point_estimate variable
#' names
#'
#' Interactive helper
#'
#' @param DT [data.table]
#' @param removals [chr: default {"value"}] vector of variable names to exclude
#'   from the final search - passed to `setdiff()`
#' @param include_PE [lgl: default FALSE] include "point_estimate" among
#'   id_varnames? Passed to `PERD_regex()`
#' @param additions [chr: default NULL] additional regex to include. Passed to
#'   `PERD_regex()`
#' @param verbose [lgl: default TRUE] print found colnames to console?
#'
#' @returns [chr] vector of variable names
#' @export
find_id_varnames <- function(
    DT
    , removals   = c("value")
    , include_PE = FALSE
    , additions  = NULL
    , verbose    = TRUE
){
  checkmate::assert_data_table(DT)
  # cannot take setdiff of `find_draws_varnames()`, since it errors if none are found
  varnames_ids <- grep(
    pattern       = PERD_regex(
      include_PE  = !include_PE # invert since grep logic is inverted
      , additions = additions
    )
    , x      = colnames(DT)
    , value  = TRUE
    , invert = TRUE
  )
  varnames_ids <- setdiff(varnames_ids, removals)
  if(verbose == TRUE) dput(varnames_ids) # print in a way that can be copy/pasted
  return(varnames_ids) # for programmatic use

}


#' Transform draws from wide to long format creating a `draw_id` column
#'
#' @param DT [data.table] input draws
#' @param id_varnames [character] columns to keep as is
#' @param verbose [lgl] print debug messages?
#'
#' @returns [data.frame] draws in long format
#' @export
draws_wide_to_long <- function(
    DT
    , id_varnames = find_id_varnames(DT, verbose = FALSE)
    , verbose     = FALSE
){
  checkmate::assert_data_table(DT)
  checkmate::assert_character(id_varnames, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(verbose, len = 1)
  # checkmate::assert_subset(id_varnames, choices = colnames(DT))
  assert_x_in_y(id_varnames, colnames(DT))

  assert_square(DT, id_varnames = id_varnames)

  vars_draws = find_draws_varnames(DT)

  if(verbose == TRUE) {
    message("wide to long - draw_id columns are e.g. : ", toString(vars_draws[1:5]))
    msg_tic()
  }

  # faster than melt()
  DT <- tidyr::pivot_longer(data = DT, cols = all_of(vars_draws), names_to = "draw_id", values_to = "value") %>%
    # dplyr::mutate(draw = as.integer(sub("^draw_", "", draw))) %>% turns point-estimates to NA
    dplyr::mutate(draw_id = sub("^draw_", "", draw_id)) %>%
    data.table::as.data.table()

  data.table::setorderv(DT, id_varnames)

  if(verbose) msg_toc()

  return(DT)
}

#' Transform draws from long to wide format by "draw_id"
#'
#' Draw columns will be ordered as c("point_estimate", "draw_0", "draw_1", "draw_2", ..., "draw_n")
#'
#' @param DT [data.table] input draws with columns `draw_id` and value_varname
#' @param id_varnames [character] columns to keep as is
#' @param verbose [lgl] print debug messages?
#' @param value_varname [chr: default "value"] name of value variable in DT
#'
#' @returns [data.frame] draws in wide format
#' @export
draws_long_to_wide <- function(
    DT
    , value_varname = "value"
    , id_varnames   = find_id_varnames(DT, removals = c(value_varname), verbose = FALSE)
    , verbose       = FALSE
){

  checkmate::assert_data_table(DT)
  assert_x_in_y(c(value_varname, id_varnames), colnames(DT))
  checkmate::assert_logical(verbose, len = 1)

  assert_square(DT, id_varnames = id_varnames)

  names_prefix <- "draw_"

  if(verbose == TRUE) {
    # draw_id may be integer (draws only) or character (draws + point_estimate)
    ex_vars <- order_draws(as.character(unique(DT$draw_id)))[1:5]
    ex_vars <- c(paste0(names_prefix, ex_vars))
    ex_vars <- gsub("draw_point_estimate", "point_estimate", ex_vars) # handle potential PE
    ex_vars <- order_draws(ex_vars)
    message("id_varnames: ", toString(id_varnames))
    message("long to wide - creating columns from draw_id e.g. : ", toString(ex_vars))
    msg_tic()
  }

  # faster than dcast
  DTW <- data.table::as.data.table(
    tidyr::pivot_wider(
      data           = DT
      , names_from   = "draw_id"
      , values_from  = value_varname
      , names_prefix = names_prefix
    )
  )


  data.table::setnames(DTW, "draw_point_estimate", "point_estimate", skip_absent = TRUE)

  data.table::setorderv(DTW, find_id_varnames(DTW, verbose = FALSE))

  if(verbose) msg_toc()

  return(DTW)
}


#' Pivot long draws wide by any arbitrary variable name
#'
#' @param DT [data.table] input draws
#' @param var_vec [chr] vector of variable levels to pivot wide
#' @param value_varname [chr] name of value variable in DT
#' @param varname [chr] variable name to pivot wide
#'
#' @returns [data.table] draws in wide format
#' @export
draws_var_to_wide <- function(
    DT
    , varname
    , var_vec       = NULL
    , value_varname = "value"
){
  checkmate::assert_data_table(DT)
  assert_x_in_y(c("draw_id", value_varname, varname), colnames(DT))
  if(!is.null(var_vec)) {
    assert_x_in_y(var_vec, unique(DT[[varname]]))
  }
  DT %>%
    # allow all levels of the variable to pivot
    { if(!is.null(var_vec)) dplyr::filter(., get(varname) %in% var_vec) else . } %>%
    tidyr::pivot_wider(., names_from = varname, values_from = value_varname, names_prefix = sprintf("%s_", value_varname)) %>%
    data.table::as.data.table()
}


#' Transform wide draws to mean and 95 percent CI
#'
#' For now, retaining point estimate and mean by default since much processing
#' code depends on mean column
#'
#' @param DT [data.table] input draws in wide format
#' @param id_varnames [character] columns to keep as is
#' @param vars_draws_pe [chr] vector of variable names in `DT` that are draws + point_estimate
#' @param remove_vars_draws [lgl] remove draw columns?
#' @param remove_point_estimate [lgl] remove point_estimate column?
#' @param remove_mean [lgl] remove mean column?
#' @param fix_mean_zero [lgl] Some sets of draws have only a single value,
#'   leading to a non-zero mean, and zeros in the UI.  This will set the mean to
#'   zero if mean is > 0 and the upper is 0, or if mean < 0 and lower is 0.
#' @param verbose [lgl] print debug messages?
#'
#' @returns [data.table] mean and 95 percent CI of draws (columns: mean, lower, upper),
#'   with or without draw columns, depending on `remove_vars_draws`
#' @export
draws_to_mean_ci <- function(
    DT
    , id_varnames           = find_id_varnames(DT, verbose = FALSE)
    , vars_draws_pe         = find_draws_varnames(DT)
    , remove_vars_draws     = TRUE
    , remove_point_estimate = FALSE
    , remove_mean           = FALSE
    , fix_mean_zero         = FALSE
    , verbose               = FALSE
){

  checkmate::assert_data_table(DT)
  checkmate::assert_logical(remove_vars_draws, len = 1)
  checkmate::assert_logical(fix_mean_zero, len = 1)
  checkmate::assert_logical(verbose, len = 1)
  # checkmate::assert_subset(c(id_varnames, vars_draws_pe), colnames(DT))
  assert_x_in_y(c(id_varnames, vars_draws_pe), colnames(DT))

  vars_draws <- setdiff(vars_draws_pe, "point_estimate")

  if(verbose == TRUE) {
    message("draws to mean/95%CI - draw columns, e.g. : ", toString(vars_draws[1:5]))
    msg_tic()
  }

  DT <- data.table::as.data.table(DT)
  DT[, mean := base::rowMeans(.SD), .SDcols = vars_draws]
  DT[, lower := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = vars_draws]
  DT[, upper := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = vars_draws]

  # post process
  # some sets of draws have only a single value, leading to a non-zero mean, and zeros in the UI
  if(fix_mean_zero == TRUE){
    DT[mean > 0 & upper == 0, mean := 0]
    DT[mean > 0 & lower == 0, lower := 0]
  }
  if(remove_vars_draws == TRUE) DT[, c(vars_draws) := NULL]
  if(remove_point_estimate == TRUE) DT[, point_estimate := NULL]
  if(remove_mean == TRUE) DT[, mean := NULL]

  data.table::setorderv(DT, id_varnames)

  if(verbose) msg_toc()

  return(DT[])
}


#' Pivot long draws wide by years
#'
#' Convenience wrapper for `draws_var_to_wide()`
#'
#' @param DT [data.table] input draws
#' @param yr_vec [int] vector of years to pivot wide
#' @param value_varname [chr] name of value variable in DT
#'
#' @returns [data.table] draws in wide format
#' @export
draws_years_to_wide <- function(DT, yr_vec = NULL, value_varname = "value"){
  return(draws_var_to_wide(DT, varname = "year_id", var_vec = yr_vec, value_varname = value_varname))
}

# draws_years_to_wide <- function(DT, yr_vec = NULL){
#   checkmate::assert_data_table(DT)
#   assert_x_in_y(c("year_id", "draw_id", "value"), colnames(DT))
#   checkmate::assert_integerish(unique(DT$year_id), min.len = 2)
#   if(!is.null(yr_vec)) {
#     assert_x_in_y(yr_vec, unique(DT$year_id))
#   }
#
#   DT %>%
#     # allow all years to pivot
#     { if(!is.null(yr_vec)) dplyr::filter(., year_id %in% yr_vec) else . } %>%
#     tidyr::pivot_wider(., names_from = "year_id", values_from = "value", names_prefix = "value_") %>%
#     data.table::as.data.table()
# }



#' Compare two-years diffs at the draw level
#'
#' Calculate year 2 minus year 1.  Helper function that wraps other draw
#' transformations,  useful for publications.
#'
#' @param DT [data.table] a table of (long) draws
#' @param yr_vec [int] 2 years to compare
#' @param id_varnames [character] columns to keep as id columns
#'
#' @returns [data.table] a table of mean and 95% CI for the difference between
#'   the two years, by id_varnames
#' @export
draws_year_diff <- function(DT, yr_vec, id_varnames = find_id_varnames(DT, verbose = FALSE)){
  checkmate::assert_data_table(DT)
  # checkmate::assert_subset(c("year_id", id_varnames), colnames(DT))
  assert_x_in_y(c("year_id", id_varnames), colnames(DT))
  checkmate::assert_integerish(unique(DT$year_id), min.len = 2)
  # checkmate::assert_subset(yr_vec, unique(DT$year_id))
  assert_x_in_y(yr_vec, unique(DT$year_id))
  checkmate::assert_set_equal(length(yr_vec), 2)

  yr_vec    <- sort(yr_vec)
  keep_vars <- c(setdiff(id_varnames, "year_id"), "value")

  DTW <- draws_years_to_wide(DT, yr_vec = yr_vec)
  DTW[, value := get(paste0("value_", yr_vec[2])) - get(paste0("value_", yr_vec[1]))]

  DTW <- DTW[, ..keep_vars]
  DTW[, years := paste0(yr_vec[1], "_", yr_vec[2])]
  DTW <- draws_long_to_wide(DTW)
  return(draws_to_mean_ci(DTW))
}


# ---- Probabilities -----------------------------------------------------------------

#' Calculate probability that draws of `comp_var` for `comp_vec[1]` are <,
#' <=, >, or >= draws of comp_var for `comp_vec[2]`.
#'
#' The comparison is done at the draw level after pivoting wide by comp_var.
#'
#' The user's order for `comp_vec` matters: the first element is compared
#' against the second (or all others, depending on `comparison_type`), using
#' the specified `operator`.
#'
#' @param DT [data.table] a table of (long) draws with columns: `draw_id`,
#'  `comp_var`, `value`, plus any `by_vars`
#' @param comp_var [chr] variable name to compare - e.g. "year_id"
#' @param comp_vec [chr] vector of 2 variable levels to compare - e.g. c("1990",
#'   "2020")
#' @param operator [chr: {"lt", "lte", "gt", "gte", "eq", "neq"}] operator for comparison -
#'   translates to <, <=, >, >=, == or != internally.  Note: == and != should only be applied
#'   in very select situations (probably only low numbers of counts or binary data)
#'   where exact equality is possible. Buyer beware.
#' @param by_vars [chr] variable names to group by when calculating probability
#' - e.g. location_id
#' @param comparison_type [chr: {"pairwise", "joint"}] type of comparison to
#'   perform: "pairwise": compare comp_vec[1] against comp_vec[2] only if
#'   length(comp_vec) == 2 (original behavior); if length(comp_vec) > 2, compare
#'   comp_vec[1] against each of comp_vec[2:n] separately (new behavior).
#'   "joint": compare comp_vec[1] against ALL other values in comp_vec (new
#'   behavior).
#' @param return_type [chr: {"probs", "binary"}] type of return object: "probs" (original
#'  behavior) returns a table of probabilities by `by_vars`; "binary" returns
#'  a table with binary indicators {1,0} at the draw level by `by_vars`.
#' @param value_varname [chr: default "value"] name of value variable in DT
#' @param verbose [lgl: default TRUE] print debug messages?
#'
#' @returns [data.table] a table of probabilities by `by_vars`
#' @export
#'
#' @examples
#' DT_demo <- data.table::as.data.table(
#' tibble::tribble(
#'    ~adm2_code, ~year_id, ~draw_id, ~me_name, ~value,
#'    "ADM2_1",     2024,       1,    "dpt1",    0.80,
#'    "ADM2_1",     2024,       1,    "bcg1",    0.70,
#'    "ADM2_1",     2024,       1,    "mcv1",    0.90,
#'    "ADM2_1",     2024,       2,    "dpt1",    0.60,
#'    "ADM2_1",     2024,       2,    "bcg1",    0.65,
#'    "ADM2_1",     2024,       2,    "mcv1",    0.20,
#'    "ADM2_2",     2024,       1,    "dpt1",    0.50,
#'    "ADM2_2",     2024,       1,    "bcg1",    0.55,
#'    "ADM2_2",     2024,       1,    "mcv1",    0.55,
#'    "ADM2_2",     2024,       2,    "dpt1",    0.40,
#'    "ADM2_2",     2024,       2,    "bcg1",    0.35,
#'    "ADM2_2",     2024,       2,    "mcv1",    0.20,
#'    )
#' )
#'
#' # Probability (0-1) at the draw level that mcv1 > dpt1 & bcg1 jointly, by
#' # adm2_code.  Typically the output of interest for publication.
#' draws_inequal_prob(
#' DT_demo
#' , comp_var        = "me_name"
#' , comp_vec        = c("mcv1", "dpt1", "bcg1")
#' , operator        = "gt"
#' , by_vars         = c("adm2_code")
#' , return_type     = "probs"
#' , comparison_type = 'joint'
#' , verbose         = TRUE
#' )
#'
#' # Binary indicators {0,1} at the draw level that mcv1 > dpt1 & mcv1 > bcg1,
#' # separately, by adm2_code - useful to interrogate the `probs` results above
#' draws_inequal_prob(
#' DT_demo
#' , comp_var        = "me_name"
#' , comp_vec        = c("mcv1", "dpt1", "bcg1")
#' , operator        = "gt"
#' , by_vars         = c("adm2_code")
#' , return_type     = "binary"
#' , comparison_type = "pairwise"
#' , verbose         = TRUE
#' )
draws_inequal_prob <- function(
    DT
    , comp_var
    , comp_vec
    , operator
    , by_vars
    , value_varname   = "value"
    , comparison_type = "pairwise"
    , return_type     = "probs"
    , verbose         = TRUE
){
  checkmate::assert_data_table(DT)
  checkmate::assert_choice(operator, choices = c("lt", "lte", "gt", "gte", "eq", "neq"))
  checkmate::assert_vector(comp_vec, min.len = 2)
  checkmate::assert_choice(comparison_type, choices = c("pairwise", "joint"))
  checkmate::assert_choice(return_type, choices = c("probs", "binary"))
  assert_x_in_y(c("draw_id", comp_var, by_vars), names(DT))
  assert_x_in_y(comp_vec, DT[[comp_var]])
  assert_x_not_in_y("draw_id", by_vars)
  assert_x_not_in_y("prop", names(DT))

  operator_fn <- switch(
    operator
    , "lt"  = `<`
    , "lte" = `<=`
    , "gt"  = `>`
    , "gte" = `>=`
    , "eq"  = `==`
    , "neq"  = `!=`
  )

  if(opertaor %in% c("eq", "neq") & isTRUE(verbose)){
    warning("Using 'eq' or 'neq' operator may lead to unintuitive results unless comparing discrete count or binary data.  Please ensure this is your intention.")
  }

  # Convert to wide format
  DTW <- draws_var_to_wide(DT, varname = comp_var, var_vec = comp_vec, value_varname = value_varname)


  # Safety net
  first_dupe <- anyDuplicated(DTW, by = c(by_vars, "draw_id"))
  if(first_dupe > 0){
    if(verbose){
      message("Wide draws rows 1-5:")
      msg_multiline(DTW[1:5, ])
    }
    stop(sprintf(
      "After pivoting wide by %s, found duplicate rows for the same %s & draw_id combination.
      First duplicated row: %s.
      Please ensure that input DT has unique rows for each combination of %s + draw_id + %s.",
      comp_var, toString(by_vars)
      , first_dupe
      , toString(by_vars), comp_var
    ))
  }

  value_var_names <- sprintf("%s_%s", value_varname, comp_vec)
  by_vars_no_draw <- setdiff(by_vars, "draw_id")

  # big tables are slow - nice to have a timer
  if(verbose) msg_tic()

  if (comparison_type == "pairwise" && length(comp_vec) == 2) {


    # Original behavior: compare first vs second
    op_var    <- sprintf("%s_%s_%s", comp_vec[1], operator, comp_vec[2])
    prob_var  <- sprintf("prob_%s", op_var)

    DTW[, (op_var) := as.integer(operator_fn(get(value_var_names[1]), get(value_var_names[2])))]
    DTP <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE)), by = by_vars_no_draw]
    data.table::setnames(DTP, "prob", prob_var)

  } else if (comparison_type == "joint") {
    # New behavior: compare first against ALL others
    # Probability that comp_vec[1] satisfies operator vs ALL other values
    op_var   <- sprintf("%s_%s_all_%s", comp_vec[1], operator, paste(comp_vec[-1], collapse = "_"))
    prob_var <- sprintf("prob_%s", op_var)

    # For each draw, check if first value satisfies operator against all others
    comparison_cols <- value_var_names[-1]

    DTW[, (op_var) := {
      first_val <- get(value_var_names[1])
      # Check if condition holds for ALL comparisons
      all_conditions <- do.call(cbind, lapply(comparison_cols, function(col) {
        operator_fn(first_val, get(col))
      }))
      as.integer(apply(all_conditions, 1, all, na.rm = TRUE))
    }]

    DTP <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE)), by = by_vars_no_draw]
    data.table::setnames(DTP, "prob", prob_var)

  } else {
    # Pairwise comparisons for multiple values
    # Compare comp_vec[1] against each of comp_vec[2:n]
    result_list <- list()

    for (i in 2:length(comp_vec)) {
      op_var   <- sprintf("%s_%s_%s", comp_vec[1], operator, comp_vec[i])
      prob_var <- sprintf("prob_%s", op_var)

      DTW[, (op_var) := as.integer(operator_fn(get(value_var_names[1]), get(value_var_names[i])))]

      DTP_temp <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE)), by = by_vars_no_draw]
      data.table::setnames(DTP_temp, "prob", prob_var)

      result_list[[i-1]] <- DTP_temp
    }

    # Merge all results
    DTP <- Reduce(function(x, y) merge(x, y, by = by_vars_no_draw, all = TRUE), result_list)
  }

  if(verbose) msg_toc()

  ret_obj <- if(return_type == "probs"){
    DTP
  } else if (return_type == "binary"){
    DTW
  }

  return(ret_obj)

}


#' Calculate probability that draws in year 1 are <, <=, >, or >=
#' draws in year 2.
#'
#' Convenience wrapper for draws_inequal_prob()
#'
#' @param DT [data.table] a table of (long) draws
#' @param yr_vec [int] 2 years to compare
#' @param operator [chr: {"lt", "lte", "gt", "gte"}] operator for comparison -
#'   translates to <, <=, >, >= internally
#' @param by_vars [chr] variable names to group by when calculating probability
#'   - e.g. location_id
#'
#' @returns [data.table] a table of probabilities by `by_vars`
#' @export
#'
#' @examples
#'  DT_demo <- data.table::as.data.table(
#' tibble::tribble(
#'    ~adm2_code, ~year_id, ~draw_id, ~me_name, ~value,
#'    "ADM2_1",     2020,       1,    "dpt1",    0.80,
#'    "ADM2_1",     2020,       1,    "bcg1",    0.70,
#'    "ADM2_1",     2020,       1,    "mcv1",    0.90,
#'    "ADM2_1",     2020,       2,    "dpt1",    0.60,
#'    "ADM2_1",     2020,       2,    "bcg1",    0.65,
#'    "ADM2_1",     2020,       2,    "mcv1",    0.20,
#'    "ADM2_1",     2024,       1,    "dpt1",    0.50,
#'    "ADM2_1",     2024,       1,    "bcg1",    0.55,
#'    "ADM2_1",     2024,       1,    "mcv1",    0.55,
#'    "ADM2_1",     2024,       2,    "dpt1",    0.40,
#'    "ADM2_1",     2024,       2,    "bcg1",    0.35,
#'    "ADM2_1",     2024,       2,    "mcv1",    0.20,
#'    )
#' )
#'
#' # Probability (0-1) at the draw level that 2024 > 2020, by adm2_code
#' draws_year_prob(
#'   DT_demo
#'   , yr_vec        = c(2020, 2024)
#'   , operator      = "gt"
#'   , by_vars       = c("adm2_code", "me_name") # me_name is necessary, or will aggregate across me_name
#' )
draws_year_prob <- function(
    DT
    , yr_vec
    , operator
    , by_vars
    , comparison_type = "pairwise"
    , return_type     = "probs"
    , verbose         = TRUE
){

  checkmate::assert_integerish(yr_vec, len = 2)

  # special case of draws_inequal_prob
  return(
    draws_inequal_prob(
      DT                = DT
      , comp_var        = "year_id"
      , comp_vec        = yr_vec
      , operator        = operator
      , by_vars         = by_vars
      , comparison_type = comparison_type
      , return_type     = return_type
      , verbose         = verbose
    )
  )
}



# ---- Graveyard -----------------------------------------------------------------

# v1
# draws_inequal_prob <- function(
    #     DT
#     , comp_var
#     , comp_vec
#     , operator
#     , by_vars
# ){
#
#   checkmate::assert_data_table(DT)
#   checkmate::assert_choice(operator, choices = c("lt", "lte", "gt", "gte"))
#   checkmate::assert_vector(comp_vec, len = 2)
#   assert_x_in_y(c("draw_id", comp_var, by_vars), names(DT))
#   assert_x_in_y(comp_vec, DT[[comp_var]])
#   assert_x_not_in_y("draw_id", by_vars)
#   assert_x_not_in_y("prop", names(DT))
#
#   operator_fn <- switch(
#     operator
#     , "lt"  = `<`
#     , "lte" = `<=`
#     , "gt"  = `>`
#     , "gte" = `>=`
#   )
#
#   op_var    <- sprintf("%s_%s_%s", comp_vec[1], operator, comp_vec[2])
#   prob_var  <- sprintf("prob_%s", op_var)
#   var_names <- sprintf("value_%s", comp_vec)
#
#   DTW <- draws_var_to_wide(DT, varname = comp_var, var_vec = comp_vec)
#
#   # new binary column based on operator
#   DTW[, (op_var) := as.integer(operator_fn(get(var_names[1]), get(var_names[2])))]
#   by_vars <- setdiff(by_vars, "draw_id")
#
#   # probability calc
#   DTP <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE) ), by = by_vars ]
#   data.table::setnames(DTP, "prob", prob_var)
#
#   return(DTP)
# }
