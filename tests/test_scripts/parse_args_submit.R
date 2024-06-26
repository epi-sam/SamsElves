
# Ironically, we need to parse args in order to test the arg-parsing function...

message("Starting pre-parser.")
pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--flag1')
pre_parser$add_argument('--flag2')
pre_parser$add_argument('--flag3')
pre_parser$add_argument('--flag4')
pre_parser$add_argument('--root_code')
args      <- pre_parser$parse_args(commandArgs(trailingOnly = TRUE))
root_code <- args$root_code
message("code_root from per-parser: ", root_code)
rm(pre_parser, args)

message("Sourcing arg-parsing function from user's code repo: ", root_code)
source(file.path(root_code, "R/parse_all_named_cli_args.R"))
source(file.path(root_code, "R/assertions.R"))

required_flag_list <- list(
  flag1 = "logical",
  flag2 = "integer",
  flag3 = NA, # if you don't care about the data type
  flag4 = "character"
)

cli_args <- parse_all_named_cli_args(
  required_args  = required_flag_list,
  trailingOnly   = TRUE,
  assign_logical = TRUE,
  assign_integer = TRUE,
  assignment_env = globalenv()
)

# Ensure flags come through with correct data types
stopifnot(identical(flag1, TRUE)) # logical
stopifnot(identical(flag2, 5L)) # integer
stopifnot(identical(flag3, "happy_birthday")) # character
stopifnot(identical(flag4, "1,3,5,7")) # comma-separated character string

# Test is valid if this message throws to the logs (no prior errors).
message("Done.")
