
root_code    <- dirname(dirname(getwd())) # devtools::test()
# root_code    <- getwd()                 # interactive
path_script  <- file.path(root_code, "tests/test_scripts/parse_args_submit.R")
std_err_root <- file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error")
message("\nroot_code:    ", root_code)
message("path_script:  ", path_script)
message("std_err_root: ", std_err_root)
stopifnot(file.exists(path_script))

test_that("parse_all_named_cli_args works and submit_job produces the correct std_err log and sends email.",
          {

            args_list <- list(
              root_code = root_code
              , flag1   = "true"
              , flag2   = 5
              , flag3   = "happy_birthday"
              # , flag4   = vec_to_comma_string(seq(1, 7, 2)) # not required if using `arg_vecs_to_comma_str` argument
              , flag4   = seq(from = 1, to = 7, by = 2)
            )

            job_id <- submit_job(
              script_path             = path_script
              , job_name              = "test_arg_parse"
              , threads               = 1L
              , mem                   = "100M"
              , runtime_min           = 1
              , archiveTF             = FALSE
              , array_tasks_int       = NULL
              , account               = "proj_cov_vpd"
              , std_err_root          = std_err_root
              , dry_runTF             = FALSE
              , args_list             = args_list
              , send_email            = TRUE
              , arg_vecs_to_comma_str = TRUE
            )

            wait_on_slurm_job_id(job_id              = job_id
                                 , initial_sleep_sec = 5
                                 , cycle_sleep_sec   = 5)
            sleep_sec <- 20
            message("Sleeping ", sleep_sec, " seconds while logs write to disk.")
            Sys.sleep(sleep_sec)

            # Read std_err log from disk
            std_err_log_path <- file.path(std_err_root, paste0("test_arg_parse_", job_id, "e.log"))
            msg_multiline(std_err_log_path)
            stopifnot(file.exists(std_err_log_path))
            std_err_log <- readLines(std_err_log_path)

            expect_equal(std_err_log[length(std_err_log)], "Done.")

          }
)

