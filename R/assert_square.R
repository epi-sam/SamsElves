#' Assert a data.table is square by id_varnames
#'
#' Checks:
#' - duplicated rows by id_var combination
#' - missing rows by id_var combination
#'
#' @param dt [data.table] table of data
#' @param id_varnames [chr] vector of id variable names that, in combination (e.g. expand.grid) will uniquely ID all rows of data
#' @param verbose [lgl] print success message?
#' @param hard_stop [lgl] default TRUE - stop if non-square, warn if FALSE
#' @param no_na_varnames [chr] optional (defualt NULL) vector of variable names that must not be NA to be considered 'square'
#'
#' @return [list] 2 data.tables - duplicated rows and missing rows
#' @export
#'
assert_square <- function(
    dt
    , id_varnames
    , no_na_varnames = NULL
    , verbose = TRUE
    , hard_stop = TRUE
    , stop_if_empty = TRUE
){

  # Validate inputs
  if(!data.table::is.data.table(dt)) stop("dt must be a data.table")
  if(!is.character(id_varnames)) stop("id_varnames must be a character vector")
  if(!is.logical(verbose)) stop("verbose must be a logical")
  varnames_missing <- setdiff(id_varnames, names(dt))
  if(length(varnames_missing)) stop("Not all id_varnames are present in the data.table: ", toString(varnames_missing))
  dt_name <- deparse(substitute(dt))

  if(nrow(dt) == 0) {
    cnd_msg <- sprintf("%s has no rows.", dt_name)
    if(stop_if_empty) stop(cnd_msg) else warning(cnd_msg)
  }

  # Build a square of id_vars
  id_vars               <- lapply(id_varnames, function(x) unique(dt[[x]]))
  id_vars_square        <- do.call(data.table::CJ, id_vars)
  names(id_vars_square) <- id_varnames

  # Identify missing rows
  missing_rows <- data.table::fsetdiff(id_vars_square, dt[, ..id_varnames], all = FALSE)
  if(nrow(missing_rows) > 0) {
    message(paste0("Missing rows in the data.table, example: ", toString(paste(names(missing_rows), missing_rows[1, ], sep = ":") )))
  }

  # Identify duplicated rows
  duplicated_rows <- dt[duplicated(dt, by = id_varnames), ]
  if(nrow(duplicated_rows) > 0) {
    message(paste0("Duplicated rows in the data.table, example: ", toString(paste(names(duplicated_rows), duplicated_rows[1, ], sep = ":") )))
  }

  non_square_list <- list(
    duplicated_rows = duplicated_rows,
    missing_rows    = missing_rows
  )

  if(!is.null(no_na_varnames)){
    assert_no_na(dt, no_na_varnames, verbose = verbose)
  }

  if(any(unlist(lapply(non_square_list, nrow))) > 0 ){
    assign("NON_SQUARE_LIST", non_square_list, envir = .GlobalEnv)
    cnd_msg <- sprintf("%s is not square - see NON_SQUARE_LIST for duplicated and / or missing rows.", dt_name)
    if(hard_stop) stop(cnd_msg) else warning(cnd_msg)
  }

  if (verbose) message(dt_name, " is square by: ", toString(id_varnames))

  return(invisible())
}

