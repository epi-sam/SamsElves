#' Get YYYY_MM_DD.VV version from an output path
#' 
#' Really just retuns the last element of a string split on "/".
#' This function currently has no validation or safety rails
#'
#' @param x [file path] versioned output folder
#'
#' @return [char] "YYYY_MM_DD.VV" folder name
#'
get_version_from_path <- function(x) {
  
  x <- unlist(strsplit(x, '/')) 
  return(x[length(x)])
  
}