#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#' @export
#'
assert_x_in_y <- function(x, y){
  if (!all(x %in% y)){
    absent <- x[!x %in% y]
    stop(paste0("x required but absent in y: ", toString(absent)))
  }
}
