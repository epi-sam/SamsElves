#' Check that a vector/element is not empty
#'
#' Check for is.null & length 0. Vectorized check for is.na, empty string (""),
#' optional check for whitespace only string.
#'
#' @param x [vec] some scalar element
#' @param check_whitespace [lgl] check for all-whitespace strings?
#'
#' @return [lgl]
#' @export
is_empty <- function(x, check_whitespace = TRUE){
  
  if (is.null(x)) {
    warning("x is NULL")
    return (is.null(x))
  }
  
  if (length(x) == 0) {
    warning("x has length 0")
    return(length(x) == 0)
  }
  
  if (any(is.na(x))) {
    warning ("x has NA")
    return (is.na(x))
  }
  
  if (is.character(x)) {
    
    if(any(x == "")) {
      warning("x has an empty character string")
      return (x == "")
    }
    
    if(any(grepl("^[[:space:]]*$", x) & check_whitespace)){
      warning("x has a whitespace character string")
      return (grepl("^[[:space:]]*$", x))
    }
  }
  
  return(rep.int(FALSE, length(x)))
}