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
  if (length(absent) > 0) {
    stop(paste0("required in x but absent in y: ", toString(absent)))
  }
}
