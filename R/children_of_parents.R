#' Get child locations given parent IDs from IHME hierarchies
#'
#' \code{children_of_parents()} finds children from IHME Hierarchies, given a
#' vector of location_ids.v From a hierarchy, which children are have a given
#' parent ID in the path_to_top_parent column?  Returns logical T/F vector.
#'
#' @param parent_loc_ids Numeric vector of parent location_ids
#' @param path_to_parent_string Vector of strings, formatted "1,2,3". What is
#'   the path_to_top_parent string vector?
#' @param include_parent Logical T/F Do you want parents included with children?
#'   If FALSE, be careful if the parent location_ids are nested inside each
#'   other.
#'
#' @return Logical T/F vector
#' @export
#'
#' @examples
#'
#' paths <- c("1,31,32", "1,31,32,33", "1,4,5,6") # Central Asia, Armenia, China
#' parents <- c(6, 32) # China, Central Asia
#'
#' children_of_parents(parent_loc_ids = parents, path_to_parent_string = paths, include_parent = T)
#' # [1] TRUE  TRUE  TRUE
#'
#' children_of_parents(parent_loc_ids = parents, path_to_parent_string = paths, include_parent = F)
#' # [1] FALSE  TRUE FALSE
#'
children_of_parents <- function(
    parent_loc_ids, # vector of parent location_ids
    path_to_parent_string, # which column is path_to_top_parent?
    include_parent = TRUE # include parent with children, or only children?
){

  child_TF <- c()

  for(i in 1:length(path_to_parent_string)){
    X <- as.numeric(unlist(strsplit(path_to_parent_string[i], ",")))

    if(include_parent){

      child_TF[i] <- any(parent_loc_ids %in% X)

    } else if (!include_parent) {

      if(any(parent_loc_ids == X[length(X)])) {

        child_TF[i] <- FALSE

      } else {

        child_TF[i] <- any(parent_loc_ids %in% X)

      }
    }
  }

  return(child_TF)

}
