#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#' @export
#'
assert_x_in_y <- function(x, y){
  absent <- setdiff(x, y)
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  if (length(absent) > 0) {
    stop(sprintf("required in %s but absent in %s: %s", x_name, y_name, toString(absent)))
  }
}
