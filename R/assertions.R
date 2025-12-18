#' Assert function arguments are not null.
#'
#' Sometimes a NULL argument allows a the function to proceed without error, but
#' produces unhelpful, NULL results. This allows the user to reject functions
#' called with NULL arguments if they'll be problematic.
#'
#' @param as_list_env [list] as_list_env = as.list(environment()) - default
#'   calling environment one level up the call stack
#'
#' @return [std_err] console output of missing arguments
#' @export
#' @examples
#' # These cannot be run directly
#' \dontrun{
#' test_fun <- function(arg_a){return(5 *arg_a)}
#' test_fun(NULL) # produces NULL, but maybe you prefer an error
#' test_fun2 <- function(arg_b){assert_no_null_arguments(); return(5 * arg_b)}
#' test_fun2(NULL) # produces an error rather than NULL
#' test_fun2() # informs user of _all_ missing arguments rather than only the first missing arg found (base R behavior)
#' }
assert_no_null_arguments <- function(as_list_env = NULL) {

  # default to environment one level up the call stack
  if(is.null(as_list_env)) as_list_env <- as.list(rlang::caller_env(n = 1))

  arg_symbols <- unlist(lapply(as_list_env, is.symbol))
  if(any(arg_symbols)) {
    symbol_names <- paste(names(arg_symbols[arg_symbols]), collapse = ", ")
    stop(glue::glue("One or more of your arguments are still undefined symbols (likely missing an argument):
                    {symbol_names}"))
  }

  arg_nulls <- unlist(lapply(as_list_env, is.null))
  if(any(arg_nulls)) {
    null_names <- paste(names(arg_nulls[arg_nulls]), collapse = ", ")
    stop(glue::glue("One or more of your arguments is NULL - please recheck function inputs:
                    {null_names}"))
  }

}

#' Assert that an object is a list, and that all top level items have names
#'
#' @param x [list] object to be checked
#'
#' @return none
#' @export
#'
#' @examples
#' assert_named_list(list(a = 1, b = 2))
assert_named_list = function(x, allow_data_frame = FALSE){
  if(!is.null(x)){
    x_name <- deparse(substitute(x))
    err_msg <- paste(x_name, "must be a named list, not vector or data.frame (list names may not be whitespace).  If you want to allow a data.frame, set allow_data_frame = TRUE.")
    if(!allow_data_frame && is.data.frame(x)) stop(err_msg)
    if(!is.list(x))                           stop(err_msg)
    if(is.null(names(x)))                     stop(err_msg)
    if(any(is.na(names(x))))                  stop(err_msg)
    if(any(nchar(trimws(names(x))) == 0))     stop(err_msg)
  }
}

#' Assert that a list contains all the element from a _named_ check list.
#'
#' Each `truth_list` list element name is an element that should exist in
#' `check_list.` The value of each `truth_list` list element is the data type
#' that the corresponding `check_list` element should be. If you don't wish to
#' check data type, save as `NA` e.g. `list(arg1 = NA)`.
#'
#' @param check_list [list] named list of items to check for presence and data
#'   type.
#' @param truth_list [list] named list of pre-determined items with data types.
#'   If you don't wish to check data type, save as `NA` e.g. `list(arg1 = NA)`
#' @param allow_data_frame [lgl] TRUE if `check_list` is a data.frame (allows
#'   checking column presence and type).
#'
#' @return none
#' @export
#'
#' @examples
#' assert_list_elements_and_types(list(a = 1, b = "2"), list(a = "double", b = "character"))
#' assert_list_elements_and_types(mtcars, list(mpg = "double", cyl = "double"), allow_data_frame = TRUE)
assert_list_elements_and_types <- function(check_list, truth_list, allow_data_frame = FALSE){
  assert_named_list(check_list, allow_data_frame = allow_data_frame)
  assert_named_list(truth_list)
  found_mask    <- names(truth_list) %in% names(check_list)
  found_items   <- names(truth_list)[found_mask]
  unfound_items <- names(truth_list)[!found_mask]
  if(length(unfound_items) > 0){
    stop("Some required list elements not found: ", toString(unfound_items))
  }

  type_check <- unlist(
    lapply(seq_along(truth_list), function(idx){
      name_i <- names(truth_list)[idx]
      type_i <- truth_list[[idx]]
      if(is.na(type_i)) return(TRUE)
      return(typeof(check_list[[name_i]]) == type_i)
    })
  )
  names(type_check) <- names(truth_list)
  type_fail_names   <- names(type_check)[!type_check]
  type_fail_df      <- data.frame(
    item            = type_fail_names
    , type_supplied = unlist(lapply(type_fail_names, function(x) typeof(check_list[[x]])))
    , type_required = unlist(lapply(type_fail_names, function(x) truth_list[[x]]))
  )

  if(!all(type_check)){
    message(prt_multiline(type_fail_df))
    stop("The following check_list data types did not match the truth_list data types: ", toString(type_fail_names))
  }

}

#' Assert Greater Than or Equal To
#'
#' @param x [num]eric vector
#' @param y [num]eric vector
#'
#' @returns [none] stop if any elements of x are greater than y
#' @export
#'
#' @examples
#' lower = c(1, 2, 3)
#' mean = c(3, 3, 3)
#' assert_x_gte_y(mean, lower)
#' mean = c(1, 2, 4)
#' # assert_x_gte_y(lower, mean) # stop
assert_x_gte_y <- function(x, y){
  checkmate::assert_numeric(x, any.missing = FALSE)
  checkmate::assert_numeric(y, any.missing = FALSE)
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  if(any(x < y)){
    bad_idx   <- which(x < y)
    offenders <- paste0("(", x[bad_idx], " < ", y[bad_idx], ")")
    stop(sprintf("%s is less than/equal to %s at index: %s : %s", x_name, y_name, paste0(bad_idx, collapse = ", "), toString(offenders)))
  }
}
