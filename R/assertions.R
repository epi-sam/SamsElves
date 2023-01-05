#' Assert function arguments are not null
#'
#' @param as_list_env [list] Require: user must provide as_list_env =
#'   as.list(environment()) - cannot rely on default argument
#'
#' @return [none] console output of missing arguments
#' @export
assert_no_null_arguments <- function(as_list_env = as.list(environment())) {
  
  arg_list <- as_list_env # assume function argument is empty except for formals
  
  arg_symbols <- unlist(lapply(arg_list, is.symbol))
  if(any(arg_symbols)) {
    print(names(arg_symbols[arg_symbols]))
    stop("Your arguments are still undefined symbols")
  }
  arg_nulls <- unlist(lapply(arg_list, is.null))
  if(any(arg_nulls)) {
    print(names(arg_nulls[arg_nulls]))
    stop("One or more of your arguments is NULL - please recheck function inputs")
  }
  
}
