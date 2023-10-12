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