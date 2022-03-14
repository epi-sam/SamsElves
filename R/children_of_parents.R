#' children_of_parents
#'
#' For use with IHME Hierarchies.
#'
#' From a hierarchy, which children are have a given parent ID in the
#' path_to_top_parent column?  Returns logical T/F vector.
#'
#' Accepts a vector of parents, and requires user to define a vector name of
#' path_to_top_parent strings, formatted "1,2,3".
#'
#' Allows the user to exclude the parent (FALSE) from the output vector.
#' Default is to include parents, since exclusion could falsely exclude
#' locations if input parent location_ids are nested (e.g. 32 is nested in 31).
#'
#' @param parent_loc_ids Numeric vector of parent location_ids
#' @param path_to_parent_string Vector of strings, formatted "1,2,3".
#' What is the path_to_top_parent string vector?
#' @param include_parent Logical T/F
#' Do you want parents included with children?
#' If FALSE, be careful if the parent location_ids are nested inside each other.
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
#' [1] TRUE  TRUE  TRUE
#'
#' children_of_parents(parent_loc_ids = parents, path_to_parent_string = paths, include_parent = F)
#' [1] FALSE  TRUE FALSE
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

# testing
children_of_parents(parent_loc_ids = parents,
                    path_to_parent_string = test_col,
                    include_parent = F)

dfa <- hier %>%
  mutate(test = children_of_parents(parent_loc_ids = c(32,31),
                                        path_to_parent_string = path_to_top_parent,
                                    include_parent = T)) %>%
  filter(test)

tmp <- hier[, test := children_of_parents(parent_loc_ids = c(6, 31), path_to_parent_string = path_to_top_parent)]
tmp <- tmp[which(test),]

# development
any(parents %in% as.numeric(unlist(strsplit(hier$path_to_top_parent[1:20],","))))

test_col <- hier$path_to_top_parent[1:20]

any(parents %in% as.numeric(unlist(strsplit(test_col[20], ","))))

parents <- c(32, 42)

tmp <- c()

for(i in 1:length(test_col)){
  tmp[i] <-
    any(parents %in% as.numeric(unlist(strsplit(test_col[i], ","))))

}
