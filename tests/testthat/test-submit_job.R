# submit job single ------------------------------------------------------------

test_that(
   "submit_job input validation produces expected errors",
   {
      expect_error(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = NULL,
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = NULL,
            r_image           = NULL,
            args_list         = NULL,
            dry_runTF         = TRUE
         ),
         regexp = "Please define a valid script path to submit"
      )

      expect_error(
         submit_job(
            language          = NULL,
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = NULL,
            r_image           = NULL,
            args_list         = NULL,
            dry_runTF         = TRUE
         ),
         regexp = "Input a valid language \\(case insensitive\\): r, python"
      )

      expect_error(
         submit_job(
            language          = "ruby",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = NULL,
            r_image           = NULL,
            args_list         = NULL,
            dry_runTF         = TRUE
         ),
         regexp = "Input a valid language \\(case insensitive\\): r, python"
      )

      expect_error(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = NULL,
            r_image           = NULL,
            args_list         = NULL,
            dry_runTF         = TRUE
         ),
         regexp = "Please define a Slurm account e.g. proj_cov_vpd"
      )

      expect_error(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = "proj_cov_vpd",
            r_image           = NULL,
            args_list         = NULL,
            array_tasks_int   = 1L:10L,
            hold_for_JobIDs   = 12345,
            dry_runTF         = TRUE
         ),
         regexp = "hold_for_JobIDs must be a simple integer vector"
      )

      expect_error(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = "proj_cov_vpd",
            r_image           = NULL,
            args_list         = NULL,
            array_tasks_int   = 1L:10L,
            hold_for_JobIDs   = list(12345L),
            dry_runTF         = TRUE
         ),
         regexp = "hold_for_JobIDs must be a simple integer vector"
      )

   }
)


test_that(
   "submit_job dry-run produces expected sbatch string",
   {
      # no args
      expect_message(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = "proj_cov_vpd",
            r_image           = NULL,
            args_list         = NULL,
            dry_runTF         = TRUE
         ),
         regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_%je.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%jo.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R"
      )

      # adding args
      expect_message(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = "proj_cov_vpd",
            r_image           = NULL,
            args_list         = list(a = 'arg_a', b = 'arg_b'),
            dry_runTF         = TRUE
         ),
         regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_%je.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%jo.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --a arg_a --b arg_b"
      )

      # adding job holds before args
      expect_message(
         submit_job(
            language          = "R",
            shell_script_path = NULL,
            script_path       = "some/script/path/scriptname.R",
            std_err_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name          = NULL,
            archiveTF         = FALSE,
            mem               = "10G",
            threads           = "2",
            runtime_min       = "15",
            partition         = "all.q",
            account           = "proj_cov_vpd",
            hold_for_JobIDs   = c(1L:5L),
            r_image           = NULL,
            args_list         = list(a = 'arg_a', b = 'arg_b'),
            dry_runTF         = TRUE
         ),
         regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_%je.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%jo.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --dependency=afterok:1:2:3:4:5 --a arg_a --b arg_b"
      )

      # console-style log (stderr and stdout go to same log)
      expect_message(
         submit_job(
            language             = "R",
            shell_script_path    = NULL,
            script_path          = "some/script/path/scriptname.R",
            std_err_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name             = NULL,
            archiveTF            = TRUE,
            mem                  = "10G",
            threads              = "2",
            runtime_min          = "15",
            partition            = "all.q",
            account              = "proj_cov_vpd",
            hold_for_JobIDs      = c(1L:5L),
            r_image              = NULL,
            console_style_log_tf = TRUE,
            dry_runTF            = TRUE
         ),
         regexp = "batch -J scriptname -C archive --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd -e /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%j_console.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%j_console.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --dependency=afterok:1:2:3:4:5"
      )
   }
)

# submit job array ------------------------------------------------------------


test_that(
   "submit_job dry-run produces expected sbatch string",
   {
      expect_message(
         submit_job(
            language             = "R",
            shell_script_path    = NULL,
            script_path          = "some/script/path/scriptname.R",
            std_err_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name             = NULL,
            archiveTF            = FALSE,
            mem                  = "10G",
            threads              = "2",
            runtime_min          = "15",
            partition            = "all.q",
            account              = "proj_cov_vpd",
            r_image              = NULL,
            args_list            = NULL,
            array_tasks_int      = 1L:10L,
            hold_for_JobIDs      = 12345L,
            console_style_log_tf = FALSE,
            dry_runTF            = TRUE
         ),
         regexp = "sbatch -J scriptname --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd --array=1-10 -e /mnt/share/temp/slurmoutput/ssbyrne/error/%x_%A_%ae.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%A_%ao.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --dependency=afterok:12345"
      )

      # console-style log (stderr and stdout go to same log)
      expect_message(
         submit_job(
            language             = "R",
            shell_script_path    = NULL,
            script_path          = "some/script/path/scriptname.R",
            std_err_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
            std_out_root         = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
            job_name             = NULL,
            archiveTF            = TRUE,
            mem                  = "10G",
            threads              = "2",
            runtime_min          = "15",
            partition            = "all.q",
            account              = "proj_cov_vpd",
            hold_for_JobIDs      = c(1L:5L),
            r_image              = NULL,
            console_style_log_tf = TRUE,
            array_tasks_int      = 1L:10L,
            dry_runTF            = TRUE
         ),
         regexp = "sbatch -J scriptname -C archive --mem=10G -c 2 -t 15 -p all.q -A proj_cov_vpd --array=1-10 -e /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%A_%a_console.log -o /mnt/share/temp/slurmoutput/ssbyrne/output/%x_%A_%a_console.log /mnt/share/singularity-images/rstudio/shells/execRscript.sh -i /mnt/share/singularity-images/rstudio/latest.img -s some/script/path/scriptname.R --dependency=afterok:1:2:3:4:5"
      )
   }
)


test_that("Submitted array job works and produces the correct std_out logs, and sends email.",
          {
             root_code    <- dirname(dirname(getwd())) # devtools::test()
             # root_code    <- getwd()                 # interactive
             path_script  <- file.path(root_code, "tests/test_scripts/array_job_submitted.R")
             std_err_root <- file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error")
             std_out_root <- file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output")
             array_tasks_int <- c(6L, 8L, 17L, 22L)
             # message("\nroot_code:    ", root_code)
             # message("path_script:  ", path_script)
             # message("std_err_root: ", std_err_root)
             stopifnot(file.exists(path_script))

             job_id <- submit_job(
                script_path            = path_script
                , job_name             = "test_submit_job_array"
                , threads              = 1L
                , array_tasks_int      = array_tasks_int
                , archiveTF            = FALSE
                , mem                  = "100M"
                , runtime_min          = 5L
                , partition            = "all.q,long.q"
                , account              = "proj_cov_vpd"
                , std_err_root         = std_err_root
                , std_out_root         = std_out_root
                , console_style_log_tf = TRUE
                , send_email           = TRUE
                , args_list            = list(root_code = root_code)
                , dry_runTF            = FALSE
             )

             wait_on_slurm_job_id(job_id              = job_id
                                  , initial_sleep_sec = 5
                                  , cycle_sleep_sec   = 5)
             sleep_sec <- 20
             message("Sleeping ", sleep_sec, " seconds while logs write to disk.")
             Sys.sleep(sleep_sec)

             # Read std_err log from disk
             console_log_paths <- file.path(std_out_root, paste0("test_submit_job_array_", job_id, "_", array_tasks_int,  "_console.log"))
             msg_multiline(console_log_paths)
             stopifnot(all(file.exists(console_log_paths)))
             console_logs <- lapply(console_log_paths, readLines)

             for(log in console_logs) expect_equal(log[length(log)], "Done.")
          }
)
