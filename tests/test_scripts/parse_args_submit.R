
# Ironically, we need to parse args in order to test the arg-parsing function...

message("Starting pre-parser.")
pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--script_path')
pre_parser$add_argument('--root_code')
pre_parser$add_argument('--flag1')
pre_parser$add_argument('--flag2')
pre_parser$add_argument('--flag3')
pre_parser$add_argument('--flag4')
pre_parser$add_argument('--flag5')
pre_parser$add_argument('--flag6')
pre_parser$add_argument('--flag7')
args      <- pre_parser$parse_args(commandArgs(trailingOnly = TRUE))
root_code <- args$root_code
message("code_root from per-parser: ", root_code)
rm(pre_parser, args)

message("Sourcing arg-parsing functions from user's code repo: ", root_code)
lapply(list.files(file.path(root_code, "R"), full.names = TRUE, recursive = FALSE, pattern = "\\.[Rr]$"), source)

# Test binding unlock/relock
lockBinding('root_code', .GlobalEnv)

required_flag_list <- list(
  root_code = "character" # to check binding unlock/relock
  , flag1   = "logical"
  , flag2   = "integer"
  , flag3   = NA # if you don't care about the data type
  , flag4   = "integer" # integer vector checking allowed by `split_comma_str` argument
  # NULLs
  , flag5   = "NULL"
  , flag6   = "NULL"
  # NA
  , flag7   = "logical"
)


cli_args <- parse_all_named_cli_args(
  required_args     = required_flag_list
  , trailingOnly    = TRUE
  , assign_logical  = TRUE
  , assign_integer  = TRUE
  , assign_NA       = TRUE
  , assign_NULL     = TRUE
  , split_comma_str = TRUE
  , allow_rebinding = TRUE
  , assignment_env  = globalenv()
)

# Ensure flags come through with correct data types
stopifnot(identical(flag1, TRUE)) # logical
stopifnot(identical(flag2, 5L)) # integer
stopifnot(identical(flag3, "happy_birthday")) # character
stopifnot(identical(flag4, c(1L, 3L, 5L, 7L))) # integer vector

# Test is valid if this message throws to the logs (no prior errors).
message("Done.")
