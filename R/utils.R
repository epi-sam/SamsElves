#' Convert a vector to a comma-separated string
#'
#' Useful for passing R vectors to a CLI arg-parser
#'
#' @param vec [vector] some R vector
#'
#' @return [chr] a comma-separated string
#' @export
#'
#' @examples
vec_to_comma_string <- function(vec){
  return(paste(vec, collapse = ","))
}

#' Convert a comma-separated string to a vector
#'
#' Useful for passing comma-separated CLI args to an R script.
#' - WARNING: returned vector is character type - user is responsible for type conversions
#'
#' @param string [chr] a comma-separated string, e.g. 'arg1,arg2,arg3'
#'
#' @return [chr] a CHARACTER vector of the comma-separated string.
#' @export
#'
#' @examples
comma_string_to_vec <- function(string){
  return(strsplit(string, ",")[[1]])
}

#' Are all elements of a vector contiguous integers?
#'
#' Useful for checking if a vector is a valid array task list.
#' - uses user-submitted order
#'
#' @param x [int] an integer vector
#'
#' @return [lgl] TRUE if all elements are contiguous integers
#' @export
#'
#' @examples
#' is_contiguous_int_vec(1L:10L) # TRUE
#' is_contiguous_int_vec(c(1L, 3L, 2L)) # FALSE
is_sequential_int_vec <- function(x){
  stopifnot(is.vector(x, mode = "integer"))
  if(anyNA(x)) stop("NA values are not allowed")
  return(all(diff(x, lag = 1, differences = 1) == 1))
}
