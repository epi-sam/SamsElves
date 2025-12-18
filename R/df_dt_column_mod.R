# started: 2025 Sep 08 11:48:43
# purpose: Suite of functions to handle columns from data.frames or
# data.tables, preserving class and modifying in place if data.table.

#' Add/overwrite a column in a data.frame or data.table, preserving class and
#' modifying in place if data.table.
#'
#' @param x [data.frame or data.table]
#' @param new_var [chr] new column name
#' @param vec [any] vector of values to add as new column
#'
#' @returns [data.frame or data.table] input `x` with new column added
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' df <- add_column(df, "c", c(TRUE, FALSE, TRUE))
#' print(df)
#' class(df)
#' dt <- data.table::data.table(a = 1:3, b = letters[1:3])
#' add_column(dt, "c", c(TRUE, FALSE, TRUE)) # modified in place
#' print(dt)
#' class(dt)
add_column <- function(x, varname, vec){
  checkmate::assert_data_frame(x)
  if(inherits(x, "data.table")){
    x <- data.table::copy(x) # avoid modifying by reference outside function scope
    x[, (varname) := vec]
  } else {
    x[[varname]] <- vec
  }
  return(x)
}

#' Drop a column from a data.frame or data.table, preserving class and
#' modifying in place if data.table.
#'
#' @param x [data.frame or data.table]
#' @param varname [chr] column name to drop
#'
#' @returns [data.frame or data.table] input `x` with column dropped
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' df <- drop_column(df, "c")
#' print(df)
#' class(df)
#' dt <- data.table::data.table(a = 1:3, b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' drop_column(dt, "c") # modified in place
#' print(dt)
#' class(dt)
drop_column <- function(x, varname){
  checkmate::assert_data_frame(x)
  if(inherits(x, "data.table")){
    x <- data.table::copy(x) # avoid modifying by reference outside function scope
    x[, (varname) := NULL]
  } else {
    x[[varname]] <- NULL
  }
  return(x)
}
