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
#' test_fun <- function(arg_a){return(5 *arg_a)}
#' test_fun(NULL) # produces NULL, but maybe you prefer an error
#' test_fun2 <- function(arg_b){assert_no_null_arguments(); return(5 * arg_b)}
#' test_fun2(NULL) # produces an error rather than NULL 
#' test_fun2() # informs user of _all_ missing arguments rather than only the first missing arg found (base R behavior)
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

