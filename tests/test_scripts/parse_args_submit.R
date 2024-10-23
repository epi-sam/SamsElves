
# Ironically, we need to parse args in order to test the arg-parsing function...

message("Starting pre-parser.")
pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--root_code')
pre_parser$add_argument('--flag1')
pre_parser$add_argument('--flag2')
pre_parser$add_argument('--flag3')
pre_parser$add_argument('--flag4')
args      <- pre_parser$parse_args(commandArgs(trailingOnly = TRUE))
root_code <- args$root_code
message("code_root from per-parser: ", root_code)
rm(pre_parser, args)

message("Sourcing arg-parsing functions from user's code repo: ", root_code)
source(file.path(root_code, "R/parse_all_named_cli_args.R"))
source(file.path(root_code, "R/assertions.R"))
source(file.path(root_code, "R/utils_cli.R"))

required_flag_list <- list(
  flag1 = "logical",
  flag2 = "integer",
  flag3 = NA, # if you don't care about the data type
  flag4 = "integer" # integer vector checking allowed by `split_comma_str` argument
)

cli_args <- parse_all_named_cli_args(
  required_args   = required_flag_list,
  trailingOnly    = TRUE,
  assign_logical  = TRUE,
  assign_integer  = TRUE,
  assign_NA       = TRUE,
  assign_NULL     = TRUE,
  split_comma_str = TRUE,
  assignment_env  = globalenv()
)

# Ensure flags come through with correct data types
stopifnot(identical(flag1, TRUE)) # logical
stopifnot(identical(flag2, 5L)) # integer
stopifnot(identical(flag3, "happy_birthday")) # character
stopifnot(identical(flag4, c(1L, 3L, 5L, 7L))) # integer vector

# Test is valid if this message throws to the logs (no prior errors).
message("Done.")
