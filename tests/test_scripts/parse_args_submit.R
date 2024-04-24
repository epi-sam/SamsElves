
# Ironically, we need to parse args in order to test the arg-parsing function...

pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--flag1')
pre_parser$add_argument('--flag2')
pre_parser$add_argument('--flag3')
pre_parser$add_argument('--flag4')
pre_parser$add_argument('--root_code')
args      <- pre_parser$parse_args(commandArgs(TRUE))
root_code <- args$root_code
rm(pre_parser, args)

message("Sourcing arg-parsing function from user's code repo: ", root_code)
source(file.path(root_code, "R/parse_all_named_cli_args.R"))

cli_args <- parse_all_named_cli_args(
  trailingOnly   = TRUE,
  assign_logical = TRUE,
  assign_integer = TRUE,
  assignment_env = globalenv()
)

message("Done.")
