#' Assert all elements of x are in required
#'
#' @param x [vector] some vector
#' @param required [vector] some vector
#'
#' @return [none] stop if any elements of x are not in required
#' @export
#'
assert_all_in <- function(x, required){
  if (!all(x %in% required)){
    absent <- x[!x %in% required]
    stop(paste0("Required but absent: ", toString(absent)))
  }
}
