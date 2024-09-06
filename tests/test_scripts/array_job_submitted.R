
message("Starting pre-parser.")
pre_parser <- argparse::ArgumentParser()
pre_parser$add_argument('--root_code')
args      <- pre_parser$parse_args(commandArgs(trailingOnly = TRUE))
root_code <- args$root_code

message("code_root from per-parser: ", root_code)

message("Slurm array task id is:", Sys.getenv("SLURM_ARRAY_TASK_ID"))

message("Done.")
