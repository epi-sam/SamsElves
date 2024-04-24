
root_code   <- dirname(dirname(getwd()))
path_script <- file.path(root_code, "tests/test_scripts/parse_args_submit.R")
std_err_path <- file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error")
message(root_code)
message(path_script)
message(std_err_path)

test_that("parse_all_named_cli_args works",
          {

            args_list <- list(
              root_code = root_code
              , flag1   = "true"
              , flag2 = 5
              , flag3 = "happy_birthday"
              , flag4 = paste(seq(1,8,2), collapse = ",")
            )

            job_id <- submit_job(
              script_path    = path_script
              , threads      = 1
              , mem_GB       = "100M"
              , runtime_min  = 1
              , archiveTF    = FALSE
              , job_name     = "test_arg_parse"
              , Account      = "proj_cov_vpd"
              , std_err_path = std_err_path
              , dry_runTF    = FALSE
              , args_list    = args_list
            )

            wait_on_slurm_job_id(job_id = job_id
                                 , initial_sleep_sec = 5
                                 , cycle_sleep_sec = 5)
            message("Sleeping 30s while logs write to disk")
            Sys.sleep(30)

            # Read std_err log from disk
            std_err_log_path <- file.path(std_err_path, paste0("test_arg_parse_e", job_id, ".log"))
            stopifnot(file.exists(std_err_log_path))
            std_err_log <- readLines(std_err_log_path)

            expect_equal(std_err_log[length(std_err_log)], "Done.")

          }
)

