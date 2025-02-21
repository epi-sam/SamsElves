
root_code    <- dirname(dirname(getwd())) # devtools::test()
# root_code    <- getwd()                 # interactive
path_script  <- file.path(root_code, "tests/test_scripts/parse_args_submit.R")
std_err_root <- file.path("/mnt/share/temp/slurmoutput", Sys.info()[["user"]], "error")
message("\nroot_code:    ", root_code)
message("path_script:  ", path_script)
message("std_err_root: ", std_err_root)
stopifnot(file.exists(path_script))

test_that("parse_all_named_cli_args works and submit_job produces the correct std_err log and sends email.",
          {

            job_id <- submit_job(
              script_path             = path_script
              , job_name              = "test_arg_parse"
              , threads               = 1L
              , mem                   = "100M"
              , runtime_min           = 1
              , archiveTF             = FALSE
              , array_tasks_int       = NULL
              , partition             = "long.q"
              , account               = "proj_cov_vpd"
              , std_err_root          = std_err_root
              , send_email            = TRUE
              , arg_vecs_to_comma_str = TRUE
              , dry_runTF             = FALSE
              , args_list             = list(
                root_code = root_code
                # Test logical conversion
                , flag1   = "true"
                , flag2   = 5
                , flag3   = "happy_birthday"
                # test converting vector to comma strings
                , flag4   = seq(from = 1, to = 7, by = 2)
                # test NULL handling
                , flag5   = c('', '  ')
                , flag6   = NULL
                # test NA handling
                , flag7   = NA
              )
            )

            wait_on_slurm_job_id(job_id              = job_id
                                 , initial_sleep_sec = 15
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

