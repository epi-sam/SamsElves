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


#' Transform draws from wide to long format
#'
#' @param DT [data.table] input draws
#' @param id_varnames [character] columns to keep as is
#' @param verbose [lgl] print debug messages?
#'
#' @returns [data.frame] draws in long format
#' @export
draws_wide_to_long <- function(DT, id_varnames = find_id_varnames(DT, verbose = FALSE), verbose = FALSE){
  checkmate::assert_data_table(DT)
  checkmate::assert_character(id_varnames, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(verbose, len = 1)
  # checkmate::assert_subset(id_varnames, choices = colnames(DT))
  assert_x_in_y(id_varnames, colnames(DT))

  assert_square(DT, id_varnames = id_varnames)

  vars_draws = find_draws_varnames(DT)

  if(verbose == TRUE) message("wide to long - draw_id columns, e.g. : ", toString(vars_draws[1:5]))

  # faster than melt()
  DT <- tidyr::pivot_longer(data = DT, cols = all_of(vars_draws), names_to = "draw_id", values_to = "value") %>%
    # dplyr::mutate(draw = as.integer(sub("^draw_", "", draw))) %>% turns point-estimates to NA
    dplyr::mutate(draw_id = sub("^draw_", "", draw_id)) %>%
    data.table::as.data.table()

  data.table::setorderv(DT, id_varnames)

  return(DT)
}

#' Transform draws from long to wide format
#'
#' Draw columns will be ordered as c("point_estimate", "draw_0", "draw_1", "draw_2", ..., "draw_n")
#'
#' @param DT [data.table] input draws
#' @param id_varnames [character] columns to keep as is
#' @param verbose [lgl] print debug messages?
#'
#' @returns [data.frame] draws in wide format
#' @export
draws_long_to_wide <- function(DT, id_varnames = find_id_varnames(DT, removals = c("value"), verbose = FALSE), verbose = FALSE){

  checkmate::assert_data_table(DT)
  # checkmate::assert_subset(c("value", id_varnames), colnames(DT))
  assert_x_in_y(c("value", id_varnames), colnames(DT))
  checkmate::assert_logical(verbose, len = 1)

  assert_square(DT, id_varnames = id_varnames)

  names_prefix <- "draw_"

  ex_vars <- order_draws(unique(DT$draw_id))[1:5]
  ex_vars <- c(paste0(names_prefix, ex_vars))
  ex_vars <- gsub("draw_point_estimate", "point_estimate", ex_vars) # handle potential PE

  if(verbose == TRUE) {
    message("id_varnames: ", toString(id_varnames))
    message("long to wide - draw_id columns to e.g. : ", toString(ex_vars))
  }

  # faster than dcast
  DTW <- data.table::as.data.table(
    tidyr::pivot_wider(
      data           = DT
      , names_from   = "draw_id"
      , values_from  = "value"
      , names_prefix = names_prefix
    )
  )


  data.table::setnames(DTW, "draw_point_estimate", "point_estimate", skip_absent = TRUE)

  data.table::setorderv(DTW, find_id_varnames(DTW, verbose = FALSE))

  return(DTW)
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

  if(verbose == TRUE) message("draws to mean/95%CI - draw columns, e.g. : ", toString(vars_draws[1:5]))

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

  return(DT[])
}

#' Pivot long draws wide by some variable
#'
#' @param DT [data.table] input draws
#' @param var_vec [chr] vector of variable levels to pivot wide
#'
#' @returns [data.table] draws in wide format
#' @export
draws_var_to_wide <- function(DT, varname, var_vec = NULL){
  checkmate::assert_data_table(DT)
  assert_x_in_y(c("draw_id", "value", varname), colnames(DT))
  if(!is.null(var_vec)) {
    assert_x_in_y(var_vec, unique(DT[[varname]]))
  }
  DT %>%
    # allow all levels of the variable to pivot
    { if(!is.null(var_vec)) dplyr::filter(., get(varname) %in% var_vec) else . } %>%
    tidyr::pivot_wider(., names_from = varname, values_from = "value", names_prefix = paste0("value_")) %>%
    data.table::as.data.table()
}


#' Pivot long draws wide by years
#'
#' Convenience wrapper for draws_var_to_wide()
#'
#' @param DT [data.table] input draws
#' @param yr_vec [int] vector of years to pivot wide
#'
#' @returns [data.table] draws in wide format
#' @export
draws_years_to_wide <- function(DT, yr_vec = NULL){
  return(draws_var_to_wide(DT, varname = "year_id", var_vec = yr_vec))
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

#' Calculate probability that draws of comp_var for comp_vec[1] are <, <=, >, or >=
#' draws of comp_var for comp_vec[2].
#'
#' The comparison is done at the draw level after pivoting wide by comp_var.
#'
#' @param DT [data.table] a table of (long) draws
#' @param comp_var [chr] variable name to compare - e.g. "year_id"
#' @param comp_vec [chr] vector of 2 variable levels to compare - e.g. c("1990", "2020")
#' @param operator [chr: {"lt", "lte", "gt", "gte"}] operator for comparison -
#'  translates to <, <=, >, >= internally
#' @param by_vars [chr] variable names to group by when calculating probability
#' - e.g. location_id
#'
#' @returns [data.table] a table of probabilities by `by_vars`
#' @export
draws_inequal_prob <- function(
    DT
    , comp_var
    , comp_vec
    , operator
    , by_vars
){

  checkmate::assert_data_table(DT)
  checkmate::assert_choice(operator, choices = c("lt", "lte", "gt", "gte"))
  checkmate::assert_vector(comp_vec, len = 2)
  assert_x_in_y(c("draw_id", comp_vars, by_vars), names(DT))
  assert_x_in_y(comp_vec, DT[[comp_var]])
  se$assert_x_not_in_y("draw_id", by_vars)
  se$assert_x_not_in_y("prop", names(DT))

  operator_fn <- switch(
    operator
    , "lt"  = `<`
    , "lte" = `<=`
    , "gt"  = `>`
    , "gte" = `>=`
  )

  op_var    <- sprintf("%s_%s_%s", comp_vec[1], operator, comp_vec[2])
  prob_var  <- sprintf("prob_%s", op_var)
  var_names <- sprintf("value_%s", comp_vec)

  DTW <- draws_var_to_wide(DT, varname = comp_var, var_vec = comp_vec)

  # new binary column based on operator
  DTW[, (op_var) := as.integer(operator_fn(get(var_names[1]), get(var_names[2])))]
  by_vars <- setdiff(by_vars, "draw_id")

  # probability calc
  DTP <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE) ), by = by_vars ]
  setnames(DTP, "prob", prob_var)

  return(DTP)
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
draws_year_prob <- function(DT, yr_vec, operator, by_vars){

  # checkmate::assert_data_table(DT)
  # checkmate::assert_integerish(yr_vec, len = 2)
  # checkmate::assert_choice(operator, choices = c("lt", "lte", "gt", "gte"))
  # assert_x_in_y(by_vars, names(DT))
  # assert_x_not_in_y("draw_id", by_vars)
  # assert_x_not_in_y("prop", names(DT))
  #
  # operator_fn <- switch(
  #   operator
  #   , "lt"  = `<`
  #   , "lte" = `<=`
  #   , "gt"  = `>`
  #   , "gte" = `>=`
  # )
  #
  # op_var   <- sprintf("%s_%s_%s", yr_vec[1], operator, yr_vec[2])
  # prob_var <- sprintf("prob_%s", op_var)
  #
  # DTW <- draws_years_to_wide(DT, yr_vec)
  # yr_names <- sort(grep("_\\d{4}$", names(DTW), value = TRUE))
  #
  # # new binary column based on operator
  # DTW[, (op_var) := as.integer(operator_fn(get(yr_names[1]), get(yr_names[2])))]
  # by_vars <- setdiff(by_vars, "draw_id")
  #
  # # probability calc
  # DTP <- DTW[, .(prob = mean(get(op_var), na.rm = TRUE) ), by = by_vars ]
  # setnames(DTP, "prob", prob_var)
  #
  # return(DTP)

  # make this a special case of draws_inequal_prob
  return(
    draws_inequal_prob(
      DT        = DT
      , comp_var = "year_id"
      , comp_vec = yr_vec
      , operator = operator
      , by_vars  = by_vars
    )
  )
}

