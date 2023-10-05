test_str_1 <- "my_string"
test_str_2 <- c("my_string", "my_bacon")
test_str_list <- list(first = "my_string", second = "bacon")

test_regex_1 <- "string"
test_regex_2 <- c("string", "bacon")

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
        system_user_name = Sys.getenv()["USER"],
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
      job_finder(system_user_name = Sys.getenv()["USER"],
                 jobname_filter = "rst",
                 cluster_type = NULL),
      regexp = "valid cluster type"
    )
    
    expect_error(
      job_finder(system_user_name = Sys.getenv()["USER"],
                 jobname_filter = "rst",
                 cluster_type = "UGE"),
      regexp = "valid cluster type"
    )
  }
)


test_that(
  "metadata_shell produces a list with the correctly named top-level items",
  {
    metadata_shell <- build_metadata_shell(code_root = file.path("/mnt/share/code/", Sys.getenv()["USER"], "SamsElves"))
    metadata_shell_names <- c("start_time", "user", "CODE_ROOT", "GIT", "SUBMIT_COMMANDS")
    expect_type(metadata_shell, "list")
    expect_equal(names(metadata_shell), metadata_shell_names)
  }
)

# extract_cores ----------------------------------------------------------------

test_that(
  "extract_cores returns an integer for a single Rstudio session",
  {
    expect_type(
      extract_cores(system_user_name = Sys.getenv()["USER"], jobname_filter = "^rst_ide", cluster_type = "slurm"),
      "integer"
    )
  }
)

# extract_submission_commands  ------------------------------------------------------

test_that(
  "extract_submission_commands returns a correctly shaped object",
  {
    submit_command_list <- extract_submission_commands(
      jobname_filter = "^rst_ide",
      submitline_n_char = 500,
      regex_to_extract = "ihme/singularity-images/rstudio/[:graph:]+",
      regex_to_ignore = "jpy",
      system_user_name = Sys.getenv()["USER"],
      cluster_type = "slurm"
    )
    
    submit_command_template <- c(
      submission_commands   = 1,
      extracted_cmd_strings = 1,
      n_cores               = 1
    )
    
    expect_equal(
      unlist(lapply(submit_command_list, length)),
      submit_command_template
    )
  }
)

test_that(
  "extract_submission_commands returns at least one character per item",
  {
    for(submit_command_item in submit_command_list){
      expect_gte(nchar(submit_command_item), 1)
    }
  }
)
