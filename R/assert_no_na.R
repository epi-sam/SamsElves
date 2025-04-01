#' Assert no NA values in select columns
#'
#' @param dt [data.table]
#' @param varnames [chr] vector of variable names to check for NA values
#' @param verbose [lgl] print success message?
#'
#' @return [none] stop if NA values found in `varnames`
#' @export
#'
assert_no_na <- function(dt, varnames, verbose = TRUE){
  if(!is.data.table(dt)) stop("dt must be a data.table")
  if(!is.character(varnames)) stop("varnames must be a character vector")
  if(!all(varnames %in% names(dt))) stop("Not all varnames are present in the data.table")

  # Rowwise checks are slow - only perform full checks if any NA are found
  if (anyNA(dt[, ..varnames])) {
    na_rows <- dt[dt[, .I[apply(.SD, 1, anyNA)], .SDcols = varnames]]

    if(nrow(na_rows) > 0) {
      stop("NA values found - example row: ", toString(paste(names(na_rows), na_rows[1, ], sep = ":") ))
    }
  }

  if(verbose) message("No NA values found in: ", toString(varnames))
}
