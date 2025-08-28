test_str_1 <- "my_string"
test_str_2 <- c("my_string", "my_bacon")
test_str_list <- list(first = "my_string", second = "bacon")

test_regex_1 <- "string"
test_regex_2 <- c("string", "bacon")

username <- Sys.info()[["user"]]

n_rstudio_sessions <- length(system(paste0("squeue -u ", username, " | grep 'rst_ide' "), intern = TRUE))

# extract_command_string -------------------------------------------------------

test_that(
  "extract_command_string returns correct value",
  {
    result <- extract_command_string(
      submit_command_text = test_str_1, regex_to_extract = test_regex_1
    )
    expect_equal(result, "string")
  }
)

test_that(
  "extract_command_string throws proper error",
  {
    expect_error(
      extract_command_string(test_str_2, test_regex_1),
      regexp = "Must submit text length == 1"
    )

    expect_error(
      extract_command_string(test_str_2, test_regex_2),
      regexp = "Must submit text length == 1"
    )

    expect_error(
      extract_command_string(test_str_1, test_regex_2),
      regexp = "Must submit regex length == 1"
    )

    expect_error(
      extract_command_string(
        submit_command_text = test_str_1,
        regex_to_extract = test_regex_1,
        regex_to_ignore = "g"
      ),
      regexp = "No strings were extracted - inspect inputs and regex_to_ignore"
    )
  }
)

# job_finder --------------------------------------------------

test_that(
  "job_finder produces a data.frame with 0+ rows for jobs with 'rst' or 'login' in the JobName",{
    job_results <-
      job_finder(
        system_user_name = username,
        jobname_filter   = "rst|login",
        cluster_type     = "slurm"
      )
    expect_s3_class(job_results, "data.frame")
    expect_gte(nrow(job_results), 0)
  }
)

test_that(
  "job_finder errors for wrong/blank cluster name",
  {
    expect_error(
      job_finder(system_user_name = username,
                 jobname_filter = "rst",
                 cluster_type = NULL),
      regexp = "valid cluster type"
    )

    expect_error(
      job_finder(system_user_name = username,
                 jobname_filter = "rst",
                 cluster_type = "UGE"),
      regexp = "valid cluster type"
    )
  }
)

# extract_cores ----------------------------------------------------------------

test_that(
  "extract_cores returns an integer for a single Rstudio session",
  {
    expect_type(
      extract_cores(),
      "integer"
    )
  }
)

# extract_submission_commands  ------------------------------------------------------

submit_command_list <- extract_submission_commands(
  jobname_filter    = "^rst_ide",
  submitline_n_char = 500,
  regex_to_extract  = "singularity-images/rstudio/[[:graph:]]+\\.img$",
  regex_to_ignore   = "jpy",
  system_user_name  = username,
  cluster_type      = "slurm"
)

test_that(
  "extract_submission_commands returns a correctly shaped object (may FAIL if you have >1 Rstudio session, or need to define how your rstudio session jobs are named (see how n_rstudio_sessions is defined))",
  {

    submit_command_template <- c(
      submission_commands   = n_rstudio_sessions,
      extracted_cmd_strings = n_rstudio_sessions,
      n_cores               = 1
    )

    expect_equal(
      unlist(lapply(submit_command_list, length)),
      submit_command_template
    )
  }
)

test_that(
  "extract_submission_commands returns at least one character per item.",
  {
    for(submit_command_item in unlist(submit_command_list)){
      expect_gte(nchar(submit_command_item), 1)
    }
  }
)

# build_metadata_shell  --------------------------------------------------------
test_that(
  "metadata_shell produces a list with the correctly named top-level items",
  {
    metadata_shell <- build_metadata_shell(code_root = file.path("/mnt/share/code/", username, "SamsElves"))
    metadata_shell_names <- c("start_time", "user", "code_root", "script_path", "GIT", "SUBMIT_COMMANDS", "sessionInfo")
    expect_type(metadata_shell, "list")
    expect_equal(names(metadata_shell), metadata_shell_names)
  }
)

test_that(
  "metadata_shell produces correct not-found message",
  {
    expect_message(
      metadata_shell <- build_metadata_shell(code_root = file.path("/mnt/share/code/", username, "SamsElves"),
                                             jobname_filter = "JUNK_JOBNAME_FILTER"),
      regexp = "Metadata warning:

Matched no jobs to jobname_filter argument."

    )
  }
)


# submitted metadata -----------------------------------------------------------

test_that("metadata builds correctly for submitted jobs"
          , code = {

            std_out_root <- file.path("/mnt/share/temp/slurmoutput", Sys.info()[["user"]], "output")
            root_code    <- getwd()                 # interactive
            root_code    <- dirname(dirname(getwd())) # devtools::test()
            path_script  <- file.path(root_code, "tests/test_scripts/metadata_submitted.R")

            job_id <- submit_job(
              script_path            = path_script
              , threads              = 1
              , mem                  = "500M"
              , runtime_min          = 1
              , account              = "proj_cov_vpd"
              , console_style_log_tf = TRUE
              , dry_runTF            = FALSE
              , args_list            = list(root_code = root_code)
            )
            wait_on_slurm_job_id(job_id, initial_sleep_sec = 15, cycle_sleep_sec = 15)
            console_log_paths <- file.path(std_out_root, paste0("metadata_submitted_", job_id,  "_console.log"))
            msg_multiline(console_log_paths)
            stopifnot(all(file.exists(console_log_paths)))
            console_logs <- lapply(console_log_paths, readLines)

            for(log in console_logs) expect_equal(log[length(log)],  "Done: script_path=/tests/test_scripts/metadata_submitted.R")
          })

