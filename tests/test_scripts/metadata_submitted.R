message("\nStarting pre-parser.\n")
pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--script_path')
pre_parser$add_argument('--root_code')
args      <- pre_parser$parse_args(commandArgs(trailingOnly = TRUE))
root_code <- args$root_code
message("\ncode_root from per-parser: ", root_code, "\n")
rm(pre_parser, args)

invisible(
  lapply(list.files(file.path(root_code, "R"), full.names = TRUE, recursive = FALSE, pattern = "\\.[Rr]"), source)
)

cli_args <- parse_all_named_cli_args(
  required_args     = list(
    root_code = "character"
  )
  , trailingOnly    = TRUE
  , assign_logical  = TRUE
  , assign_integer  = TRUE
  , assign_NA       = TRUE
  , assign_NULL     = TRUE
  , split_comma_str = TRUE
  , allow_rebinding = TRUE
  , assignment_env  = globalenv()
)



metadata <- build_metadata_shell(code_root = root_code)
# message(prt_multiline(metadata))

message("Done: script_path=", metadata$script_path)
