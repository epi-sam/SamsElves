#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#' @export
#'
#' @examples
#' assert_x_in_y(c("a", "b"), c("a", "b", "c"))
assert_x_in_y <- function(x, y){
  checkmate::assert_vector(x)
  checkmate::assert_vector(y)
  absent <- setdiff(x, y)
  if (length(absent) > 0) {
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    stop(sprintf("required in %s but absent in %s: %s", x_name, y_name, toString(absent)))
  }
}

#' Assert no elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @returns [none] stop if any elements of x are in y
#' @export
#'
#' @examples
#' assert_x_not_in_y(c("a", "b"), c("c", "d", "e"))
assert_x_not_in_y <- function(x, y){
  checkmate::assert_vector(x, null.ok = TRUE)
  checkmate::assert_vector(y)
  x_name  <- deparse(substitute(x))
  y_name  <- deparse(substitute(y))
  present <- intersect(x, y)
  if (length(present) > 0) {
    stop(sprintf("forbidden in %s but present in %s: %s", x_name, y_name, toString(present)))
  }
}
