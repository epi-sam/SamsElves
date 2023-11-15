#' Check that a vector/element is not empty
#'
#' Checks for length == 1, is.null, is.na, empty string ("")
#'
#' @param x [vec] some scalar element
#'
#' @return [lgl]
#' @export
#'
#' @examples
is_empty <- function(x, check_whitespace = TRUE){
  
  if (is.null(x)) {
    warning("x is NULL")
    return (is.null(x))
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