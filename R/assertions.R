#' Assert function arguments are not null
#'
#' @param as_list_env [list] as_list_env = as.list(environment()) - default
#'   calling environment one level up the call stack
#'
#' @return [std_err] console output of missing arguments
#' @export
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

