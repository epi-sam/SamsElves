#' Assert a data.table is square by id_varnames
#'
#' Checks:
#' - duplicated rows by id_var combination
#' - missing rows by id_var combination
#'
#' @param dt [data.table] table of data
#' @param id_varnames [chr] vector of id variable names that, in combination (e.g. expand.grid) will uniquely ID all rows of data
#' @param verbose [lgl: default FALSE] print success message?
#' @param hard_stop [lgl: default TRUE] - if non-square, stop if TRUE warn if FALSE
#' @param no_na_varnames [chr: default NULL] optional vector of variable names that must not be NA to be considered 'square'
#' @param stop_if_empty [lgl: default TRUE] - if `dt` has no rows, stop if TRUE warn if FALSE
#'
#' @return [list] 2 data.tables - duplicated rows and missing rows
#' @export
#'
#' @examples
#' # fail
#' .chk_list <- assert_square(dt = data.table(num = 1:3, let = letters[1:3]), id_varnames = c("num", "let"), verbose = TRUE)
#' # pass
#' .chk_list <- assert_square(dt = data.table::CJ(num = 1:3, let = letters[1:3]), id_varnames = c("num", "let"), verbose = TRUE)
assert_square <- function(
    dt
    , id_varnames
    , no_na_varnames = NULL
    , verbose = FALSE
    , hard_stop = TRUE
    , stop_if_empty = TRUE
){

  # assert inputs
  # assert no duplicated column names
  vars_dt <- colnames(dt)
  vars_dt_uniq <- unique(vars_dt)
  if(length(vars_dt) > length(vars_dt_uniq)) stop("dt has duplicated column names: ", toString(vars_dt[duplicated(vars_dt)]))
  checkmate::assert_data_table(dt)
  checkmate::assert_character(id_varnames, any.missing = FALSE, min.len = 1)
  checkmate::assert_logical(verbose, len = 1)
  checkmate::assert_subset(id_varnames, choices = colnames(dt))

  # Check for NA values
  if(!is.null(no_na_varnames)){
    # enforce stop behavior by not returning on.exit values
    hard_stop <- TRUE
    assert_no_na(dt, no_na_varnames, verbose = verbose)
  }

  dt_name <- deparse(substitute(dt))

  if(nrow(dt) == 0) {
    cnd_msg <- sprintf("%s has no rows.", dt_name)
    if (stop_if_empty == TRUE) {
      stop(cnd_msg)
    } else {
      if (verbose == TRUE) warning(cnd_msg)
    }
  }

  # Build a square of id_vars
  id_vars               <- lapply(id_varnames, function(x) unique(dt[[x]]))
  id_vars_square        <- do.call(data.table::CJ, id_vars)
  names(id_vars_square) <- id_varnames

  # Identify missing rows
  missing_rows <- data.table::fsetdiff(id_vars_square, dt[, ..id_varnames], all = FALSE)
  if(nrow(missing_rows) > 0 & verbose == TRUE) {
    message(paste0("Missing rows in the data.table, example: ", toString(paste(names(missing_rows), missing_rows[1, ], sep = ":") )))
  }

  # Identify duplicated rows
  duplicated_rows <- dt[duplicated(dt, by = id_varnames), ]
  if(nrow(duplicated_rows) > 0 & verbose == TRUE) {
    message(paste0("Duplicated rows in the data.table, example: ", toString(paste(names(duplicated_rows), duplicated_rows[1, ], sep = ":") )))
  }

  # used in production if hard_stop = FALSE, but on.exit bypasses the stop
  # functions, so must be nested in a condition >.<
  non_square_list <- list(
    duplicated_rows = duplicated_rows,
    missing_rows    = missing_rows
  )
  if (hard_stop == FALSE) {
    on.exit(return(invisible(non_square_list)))
  }

  if(any(unlist(lapply(non_square_list, nrow))) > 0 ){

    cnd_msg <- sprintf("%s is not square.\n   - see returned list (invisible, must assign to an object) for duplicated / missing rows.", dt_name)
    if(hard_stop == TRUE) {
      stop(cnd_msg)
    } else {
      if (verbose == TRUE) warning(cnd_msg)
    }

  }

  if (verbose == TRUE) message(dt_name, " is square by: ", toString(id_varnames))

}
