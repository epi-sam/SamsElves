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

# FIXME - 2023 Sep 28 - apparently we can't call `sacct` with testthat... ----
# test_that(
#   "job_finder produces a data.frame with 0+ rows for jobs with 'rst' or 'login' in the JobName",{
#     job_results <-
#       job_finder(
#         system_user_name = Sys.getenv()["USER"],
#         jobname_filter   = "rst|login",
#         cluster_type     = "slurm"
#       )
#     expect_s3_class(job_results, "data.frame")
#     expect_gte(nrow(job_results), 0)
#   }
# )

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

# 80/20 test - low coverage integration test - working with outside system is tricky
# - one integration test, don't isolate components, interact with them all directly
# - sometimes they can take a long time - 
# - call some regex for sha string form
# - test for no errors
# - the goal is not 100% coverage, the goal is confidence
