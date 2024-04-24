#' Parse command-line arguments to global environment
#'
#' Add an arbitrary number of CLI args to a parser (default trailing args only),
#' then assign to a chosen environment (default `globalenv()`).  By default,
#' automatically assign logical and integer data types when they are detected
#' among CLI arguments, e.g. 'true' is assigned as `TRUE` and 5 is assigned as
#' `5L`.
#'
#' This does not take advantage of argument defaults or types.  See
#' https://github.com/trevorld/r-argparse for documentation.
#'
#' @param trailingOnly [lgl] if TRUE, only the trailing arguments are used -
#'   passed to `commandArgs()`
#' @param assignment_env [env] which environment to assign CLI arguments to as
#'   named variables
#' @param assign_logical [lgl] if TRUE, assign true/false in parsed args to
#'   `TRUE`/`FALSE`
#' @param assign_integer [lgl] if TRUE, assign solely numeric args to integer
#'   e.g. 5 to `5L`
#'
#' @return [named list] named list of CLI arguments
#' @export
#'
#' @examples
parse_all_named_cli_args <- function(
    trailingOnly   = TRUE,
    assign_logical = TRUE,
    assign_integer = TRUE,
    assignment_env = globalenv()
) {
  # Validate inputs
  if (!is.logical(trailingOnly) | length(trailingOnly) != 1) {
    stop("trailingOnly must be a logical")
  }
  if (!is.logical(assign_logical) | length(assign_logical) != 1) {
    stop("assign_logical must be a single logical")
  }
  if (!is.logical(assign_integer) | length(assign_integer) != 1) {
    stop("assign_integer must be a single logical")
  }
  if (!is.environment(assignment_env)) {
    stop("assignment_env must be an environment")
  }

  # Grab CLI args
  command_args <- commandArgs(trailingOnly = trailingOnly)

  if (length(command_args) %% 2 != 0){
    stop("There must be an even number of arguments in key/value pairs: \n",
         paste(c("COMMAND LINE ARGS:", command_args), collapse = ' '))
  }

  # find arg names - all odd elements since args come in name/value pairs
  arg_sequence <- seq_along(command_args)
  arg_name_idx <- which(as.logical(arg_sequence %% 2))

  # Parser
  parser <- argparse::ArgumentParser() # an R6 class object

  message(paste(c("COMMAND LINE ARGS:", command_args), collapse = ' '))

  # Add an arbitrary number of named key/value arguments to the parser
  message("Adding arguments to parser")
  for (idx in arg_name_idx) {
    message(paste("Arg idx:", idx, "Arg name:", command_args[idx], command_args[idx + 1]))
    parser$add_argument(command_args[idx])
  }

  args_list <- parser$parse_args(command_args)

  if(assign_logical) message("Assigning logical type to TRUE/FALSE args (case-insensitive)")
  if(assign_integer) message("Assigning integer type to solely numeric args (e.g. no decimals)")

  for (key in names(args_list)) {

    if (toupper(args_list[[key]]) %in% c("TRUE", "FALSE") & assign_logical) {
      args_list[[key]] <- as.logical(args_list[[key]])
    }

    if (grepl("^[0-9]+$", args_list[[key]]) & assign_integer) {
      args_list[[key]] <- as.integer(args_list[[key]])
    }

  }

  message("Assigning args to chosen environment.")
  list2env(args_list, envir = assignment_env)
  message(
    paste(
      capture.output(
        print.data.frame(
          stack(args_list)[2:1],
          right = FALSE)
      ),
      collapse = '\n'
    )
  )

  return(args_list)
}
