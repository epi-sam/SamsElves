#' Ensure an element is atomic and length 1
#'
#' @param x [any] some element
#'
#' @return [std_err] stop if not length 1
#' @export
#'
assert_scalar = function(x){
  if(!(is.atomic(x) && length(x) == 1L)){
    stop("x must be atomic and length 1L")
  }
}
