#' SDG core function - Convenience `dir.create` wrapper with desired defaults
#' 
#' `dir.create()` natively prevents overwrites
#'
#' @param path [path] directory to create
#' @param RECURSIVE [lgl] dir.create arg (default = TRUE)
#'
#' @return [lgl] did directory creation succeed or fail?
#' @export
make_directory <- function(path, RECURSIVE = TRUE) {
  dir.create(path, recursive = RECURSIVE, showWarnings = FALSE)
}

#' Current Date-Time Stamp
#' 
#' Wrapper for `Sys.time()`.  Results are formatted as YYYY_MM_DD_hhmmssTZONE.
#' Allows user to print to stderr, stdout, and invisible returns the stamp.
#'
#' @param std_out [sdt_out] using `print()` 
#' @param std_err [std_err] message class
#'
#' @return [invisible] date-time stamp
#' @export
#'
datetime_stamp <- function(std_out = TRUE, std_err = FALSE, dt_format = "%Y_%m_%d_%H%M%S%Z"){
  dt_stamp <- format(Sys.time(), format = dt_format)
  if(std_out) print(dt_stamp)
  if(std_err) message(dt_stamp)
  invisible(dt_stamp)
}
