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
#' @param required_args [list] (optional; default = NULL) Named list of required
#'   arguments to parse, with data types if desired. If you do not wish to check
#'   data types, supply `NA`.  e.g. `list(arg1 = "character", arg2 = NA)`.
#'   **NOTE:** Parsed args are all "character" by default. If you wish the
#'   parser to assign integer or logical data types, set `assign_integer` or
#'   `assign_logical` to `TRUE`, and then you may check those types as well.
#' @param trailingOnly [lgl] if TRUE, only the trailing arguments are used -
#'   passed to `commandArgs()`
#' @param assignment_env [env] which environment to assign CLI arguments to as
#'   named variables
#' @param assign_logical [lgl] if TRUE, assign true/false in parsed args to
#'   `TRUE`/`FALSE`
#' @param assign_integer [lgl] if TRUE, assign solely numeric args to integer
#'   e.g. 5 to `5L`
#' @param split_comma_str [lgl] if TRUE, split comma-separated strings into
#'  vectors
#' @param assign_NA [lgl] if TRUE, assign 'NA' in parsed args to `NA`
#' @param assign_NULL [lgl] if TRUE, assign 'NULL' in parsed args to `NULL`
#'
#' @return [named list] named list of CLI arguments
#' @export
#'
#' @examples
#' \dontrun{
#' cli_args <- parse_all_named_cli_args(
#'    trailingOnly   = TRUE,
#'    assign_logical = TRUE,
#'    assign_integer = TRUE,
#'    assign_NA       = TRUE,
#'    assign_NULL     = TRUE,
#'    assignment_env = globalenv()
#' )
#' }
parse_all_named_cli_args <- function(
    required_args   = NULL,
    trailingOnly    = TRUE,
    assign_logical  = TRUE,
    assign_integer  = TRUE,
    assign_NA       = TRUE,
    assign_NULL     = TRUE,
    split_comma_str = TRUE,
    assignment_env  = globalenv()
) {

  # Validate inputs
  if (!is.null(required_args)) {
    assert_named_list(required_args)
  }
  if (!is.logical(trailingOnly) | length(trailingOnly) != 1) {
    stop("trailingOnly must be a logical")
  }
  if (!is.logical(assign_logical) | length(assign_logical) != 1) {
    stop("assign_logical must be a single logical")
  }
  if (!is.logical(assign_integer) | length(assign_integer) != 1) {
    stop("assign_integer must be a single logical")
  }
  if (!is.logical(assign_NA) | length(assign_NA) != 1) {
    stop("assign_NA must be a single logical")
  }
  if (!is.logical(assign_NULL) | length(assign_NULL) != 1) {
    stop("assign_NULL must be a single logical")
  }
  if (!is.logical(split_comma_str) | length(split_comma_str) != 1) {
    stop("split_comma_str must be a single logical")
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

  if(split_comma_str) message("Splitting comma-separated strings into vectors.")
  if(assign_logical) message("Assigning logical type to TRUE/FALSE args (case-insensitive)")
  if(assign_integer) message("Assigning integer type to solely numeric args (e.g. no decimals)")
  if(assign_NA) message("Assigning NA (integer)/NaN (double) type to args with 'NA'/'NaN' values")

  for (key in names(args_list)) {

    if (split_comma_str & grepl(",", args_list[[key]])) {
      args_list[[key]] <- comma_string_to_vec(args_list[[key]])
    }

    if (all(grepl("^TRUE$|^FALSE$", toupper(args_list[[key]]))) & assign_logical) {
      args_list[[key]] <- as.logical(args_list[[key]])
    }

    if (all(grepl("^[0-9]+$", args_list[[key]])) & assign_integer) {
      args_list[[key]] <- as.integer(args_list[[key]])
    }

    if (all(grepl("^NA$", args_list[[key]])) & assign_NA) {
      args_list[[key]] <- NA
    }

    if (all(grepl("^NaN$", args_list[[key]])) & assign_NA) {
      args_list[[key]] <- NaN
    }

    if ((all(grepl("^NULL$", args_list[[key]])) & assign_NULL)) {
      message("Assigning NULL type to : ", key)
      args_list[key] <- list(NULL)
    }

  }

  if (!is.null(required_args)) {
    message("Checking for required arguments and types: ", toString(names(required_args)))
    assert_list_elements_and_types(
      check_list       = args_list,
      truth_list       = required_args,
      allow_data_frame = FALSE
    )
  }

  message("Assigning args to chosen environment.")
  list2env(args_list, envir = assignment_env)
  message(prt_multiline(args_list))

  # for(key in names(args_list)){
  #   # if (all(grepl("^NULL$", args_list[[key]])) & assign_NULL) {
  #   #   args_list[[key]] <- NULL
  #   # }
  #   message(key, " : ", toString(args_list[[key]]))
  #   assign(key, args_list[[key]], envir = assignment_env)
  #   if ((all(grepl("^NULL$", args_list[[key]])) & assign_NULL)) {
  #     message("Assigning NULL type to : ", key)
  #     assign(key, NULL, envir = assignment_env)
  #   }
  # }

  return(args_list)
}

