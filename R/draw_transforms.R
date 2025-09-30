# started: 2025 Jan 13 19:22:05
# purpose: suite of function to transform draws from wide to long format and
# vice versa, and compute mean and 95% CI of draws
# updated: 2025 Aug 27 - adding point_estimates


#' Regex to identify draw columns + point estimate + anything else the user deems helpful
#'
#' @returns [chr] regex to identify draw columns + point estimate
PERD_regex <- function(include_PE = TRUE, additions = NULL) {
  checkmate::assert_logical(include_PE, len = 1)
  checkmate::assert_character(additions, len = 1, null.ok = TRUE)
  if(include_PE == TRUE) additions <- c("^point_estimate$", additions)
  return(paste(c("^draw_\\d{1,3}$", additions), collapse = "|"))
}


#' Order character draw names in order of their numeric indices
#'
#' e.g. "draw_1", "draw_2", ..., "draw_10", "draw_11", ...
#'
#' @param draw_varnames
#'
#' @returns [chr] ordered vector of variable names
#' @export
order_draws <- function(draw_varnames, pe_varname = "point_estimate"){

  checkmate::assert_character(draw_varnames, any.missing = FALSE, min.len = 1)
  pe_mask <- draw_varnames == pe_varname
  draws_only <- draw_varnames[!pe_mask]
  if(length(draws_only) == 0) return(draw_varnames)
  draws_ordered <- draws_only[order(as.integer(sub("^draw_", "", draws_only)))]
  if(any(pe_mask)) draws_ordered <- c(pe_varname, draws_ordered)
  return(draws_ordered)

}


#' #' Find draw variable names in a data.table
#' #'
#' #' @param DT [data.table]
#' #' @param draws_rgx [chr] regex to identify draw columns + point estimate
#' #'
#' #' @returns [chr] vector of variable names
#' #' @export
#' find_draws_varnames <- function(DT, draws_rgx = PERD_regex()) {
#'   checkmate::assert_data_table(DT)
#'   checkmate::assert_character(draws_rgx, len = 1)
#'   perd_varnames <- grep(draws_rgx, colnames(DT), value = TRUE)
#'   # sort the PE up front for visibility
#'   # checkmate::assert_integerish(length(perd_varnames), lower = 1)
#'   if(!length(perd_varnames) >= 1) stop("No draw/PE columns found")
#'   return(order_draws(perd_varnames))
#' }

#' Find draw variable names in a data.table
#'
#' @param DT [data.table]
#' @param include_PE [lgl] include point_estimate in search? Passed to PERD_regex()
#' @param additions [chr: default NULL] additional regex to include. Passed to PERD_regex()
#'
#' @returns [chr] vector of variable names
#' @export
find_draws_varnames <- function(DT, include_PE = TRUE, additions = NULL) {
  checkmate::assert_data_table(DT)
  draws_rgx <- PERD_regex(include_PE = include_PE, additions = additions)
  perd_varnames <- grep(draws_rgx, colnames(DT), value = TRUE)
  if(!length(perd_varnames) >= 1) stop("No draw/PE columns found")
  # sort the PE up front for visibility
  return(order_draws(perd_varnames))
}

#' Util to find a potential set of id_varnames for a data.table full of draws
#'
#' Interactive helper
#'
#' @param DT [data.frame]
#'
#' @return [chr] vector of variable names
#' @export
find_id_varnames <- function(
    DT
    , additions = NULL
    , removals  = "value"
    , verbose   = TRUE
){
  checkmate::assert_data_table(DT)
  ret_vec <- grep(PERD_regex(additions = additions), colnames(DT), value = TRUE, invert = TRUE)
  ret_vec <- setdiff(ret_vec, removals)
  if(verbose == TRUE) dput(ret_vec) # print in a way that can be copy/pasted
  return(ret_vec) # for programmatic use
}


#' Transform draws from wide to long format
#'
#' @param DT [data.table] input draws
#' @param id_varnames [character] columns to keep as is
#' @param verbose [lgl] print debug messages?
#'
#' @return [data.frame] draws in long format
#' @export
draws_wide_to_long <- function(DT, id_varnames = find_id_varnames(DT, verbose = FALSE), verbose = FALSE){
  checkmate::assert_data_table(DT)
  checkmate::assert_character(id_varnames, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(verbose, len = 1)
  checkmate::assert_subset(id_varnames, choices = colnames(DT))

  assert_square(DT, id_varnames = id_varnames)

  vars_draws_pe = find_draws_varnames(DT, include_PE = TRUE)

  if(verbose == TRUE) message("wide to long - draw_id columns, e.g. : ", toString(vars_draws_pe[1:5]))

  # faster than melt()
  DT <- tidyr::pivot_longer(data = DT, cols = all_of(vars_draws_pe), names_to = "draw_id", values_to = "value") %>%
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
#' @return [data.frame] draws in wide format
#' @export
draws_long_to_wide <- function(DT, id_varnames = find_id_varnames(DT, removals = c("value"), verbose = FALSE), verbose = FALSE){

  checkmate::assert_data_table(DT)
  checkmate::assert_subset(c("value", id_varnames), colnames(DT))
  checkmate::assert_logical(verbose, len = 1)

  assert_square(DT, id_varnames = id_varnames)

  names_prefix <- "draw_"

  ex_vars <- order_draws(unique(DT$draw_id))[1:5]
  ex_vars <- c(ex_vars[1], paste0(names_prefix, ex_vars[2:5]))

  if(verbose == TRUE) {
    message("id_varnames: ", toString(id_varnames))
    message("long to wide - draw_id columns to e.g. : ", toString(ex_vars))
  }

  # faster than dcast
  DTW <- tidyr::pivot_wider(data = DT, names_from = "draw_id", values_from = "value", names_prefix = names_prefix)  %>%
    data.table::as.data.table()

  data.table::setnames(DTW, "draw_point_estimate", "point_estimate")

  data.table::setorderv(DTW, find_id_varnames(DTW, verbose = FALSE))

  return(DTW)
}

#' Transform wide draws to mean and 95\% CI
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
#' @returns [data.table] mean and 95\% CI of draws (columns: mean, lower, upper),
#'   with or without draw columns, depending on `remove_vars_draws`
#' @export
draws_to_mean_ci <- function(
    DT
    , id_varnames           = find_id_varnames(DT, verbose = FALSE)
    , vars_draws            = find_draws_varnames(DT, include_PE = FALSE)
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
  checkmate::assert_subset(c(id_varnames, vars_draws), colnames(DT))

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

#' Summarize draws by getting the mean, median, upper, lower of draws. This
#' function also keeps the `point_estimate` if present in draws.
#'
#' @param DT [data.table] input draws in wide format
#' @param remove_draws [lgl] should draws be removed? (defaults to `TRUE`)
#'
#' @return [data.table] Summarized draws, where draw columns are removed.
#' @export
draws_pe_summarize <- function(
    DT
    , remove_draws = TRUE
){
  checkmate::assert_data_table(DT)
  checkmate::assert_logical(remove_draws, len = 1)

  vars_draws <- find_draws_varnames(DT, include_PE = FALSE)

  DT[, mean          := base::rowMeans(.SD), .SDcols = vars_draws]
  DT[, lower         := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.025), .SDcols = vars_draws]
  DT[, upper         := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.975), .SDcols = vars_draws]
  DT[, median        := matrixStats::rowQuantiles(as.matrix(.SD), probs = 0.5), .SDcols = vars_draws]
  DT[, pe_percentile := base::rowMeans(point_estimate >= as.matrix(.SD)), .SDcols = vars_draws]
  if (remove_draws) DT[, c(vars_draws) := NULL]

  return(DT[])
}


#' Pivot long draws wide by years
#'
#' @param DT [data.table] input draws
#'
#' @return [data.table] draws in wide format
#' @export
draws_years_to_wide <- function(DT, yr_vec = NULL){
  checkmate::assert_data_table(DT)
  checkmate::assert_subset(c("year_id", "draw_id", "value"), colnames(DT))
  checkmate::assert_integerish(unique(DT$year_id), min.len = 2)
  if(!is.null(yr_vec)) {
    checkmate::assert_subset(yr_vec, unique(DT$year_id))
  }

  DT %>%
    { if(!is.null(yr_vec)) dplyr::filter(., year_id %in% yr_vec) else . } %>%
    tidyr::pivot_wider(., names_from = "year_id", values_from = "value", names_prefix = "value_") %>%
    data.table::as.data.table()
}



#' Compare two-years diffs at the draw level
#'
#' Calculate year 2 minus year 1.  Helper function that wraps other draw
#' transformations,  useful for publications.
#'
#' @param DT [data.table] a table of (long) draws
#' @param yr_vec [int] 2 years to compare
#' @param id_varnames [character] columns to keep as id columns
#'
#' @return [data.table] a table of mean and 95% CI for the difference between
#'   the two years, by id_varnames
#' @export
draws_year_diff <- function(DT, yr_vec, id_varnames = find_id_varnames(DT, verbose = FALSE)){
  checkmate::assert_data_table(DT)
  checkmate::assert_subset(c("year_id", id_varnames), colnames(DT))
  checkmate::assert_integerish(unique(DT$year_id), min.len = 2)
  checkmate::assert_subset(yr_vec, unique(DT$year_id))
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


#' If draws are available, calculate the difference between:
#'
#'  1) the PE and the UI of the draws (to ensure PE is within UI)
#'  2) the PE and mean of draws
#'  3) the PE and median of draws
#'
#' @param DT [data.table] input draws (wide format)
#' @param print_summary_stats [lgl] print information regarding metrics 1-3 above?
#'
#' @return [data.table] a data.table with the columns:
#'
#'    - `point_estimate_in_ui`: 1 = point_estimate is within UI of draws, 0 = if not
#'    - `pe_mean_difference`: difference between `point_estimate` and `mean` of draws
#'    - `pe_median_difference`: difference between `point_estimate` and `median` of draws
#'
#' @export
get_draw_pe_ui_difference <- function(DT, print_summary_stats = TRUE) {

  checkmate::assert_data_table(DT)
  if (!any(grepl('^draw_', colnames(DT)))) stop('No draws in `DT`')
  checkmate::assert_subset(x = 'point_estimate', choices = colnames(DT))
  checkmate::assert_logical(x = print_summary_stats, len = 1)

  DT <- draws_pe_summarize(
    DT             = DT
    , remove_draws = TRUE
  )

  DT[,
    `:=` (
      point_estimate_in_ui = data.table::fifelse(
        point_estimate >= lower & point_estimate <= upper, TRUE, FALSE
      ),
      pe_mean_difference   = point_estimate - mean,
      pe_median_difference = point_estimate - median
    )
  ]

  if(print_summary_stats) {
    cat("\n")

    message('Summary of the distribution of the point_estimate:')
    print(summary(DT$point_estimate))

    cat("\n\n")

    message('Summary of the distribution of the mean of the draws:')
    print(summary(DT$mean))

    cat("\n\n")

    message('Summary of the distribution of the median of the draws:')
    print(summary(DT$median))

    cat("\n\n")

    if (any(DT$point_estimate_in_ui == 0)) {
      num_zero_rows <- length(which(DT$point_estimate_in_ui == 0))
      pct_out_of_ui <- round(num_zero_rows / nrow(DT) * 100, digits = 2)
      warning(
        pct_out_of_ui, "% (n = ", num_zero_rows,
        ") of rows have point_estimate outside of UI from draws. ",
        "You can subset rows that have PE outside of you by subsetting",
        " the returned data.table to point_estimate_in_ui == 0."
      )
      cat("\n\n")
    }

    message('Summary of difference between point_estimate minus mean of draws:')
    print(summary(DT$pe_mean_difference))

    cat("\n\n")
    message('Summary of difference between point_estimate minus median of draws:')
    print(summary(DT$pe_median_difference))

    cat("\n\n")
    message('Summary of percentile of point_estimate relative to draws:')
    print(summary(DT$pe_percentile))
  }
  return(DT[])
}
