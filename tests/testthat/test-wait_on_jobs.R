test_that(
  "wait_on_slurm_job_id dry-run produces expected command string",
  {
    expect_message(
      wait_on_slurm_job_id(
        job_id        = 1245,
        initial_sleep = 5,
        perl          = TRUE,
        dryrun        = TRUE
      ),
      regexp = "sacct --format=JobID%16,JobName%50,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20 | grep 'RUNNING\\|PENDING' | grep -P 1245"
    )
    
    expect_message(
      wait_on_slurm_job_id(
        job_id        = 1245,
        initial_sleep = 5,
        perl          = FALSE,
        dryrun        = TRUE
      ),
      regexp = "sacct --format=JobID%16,JobName%50,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20 | grep 'RUNNING\\|PENDING' | grep 1245"
    )
    
  }
)

test_that(
  "wait_on_jobs dry-run produces expected command string",
  {
    expect_message(
      wait_on_jobs(
        job_pattern         = "abcde"
        , jobname_nchar     = 5L
        , initial_sleep_sec = 5
        , file_list         = NULL
        , obj               = NULL
        , resub             = 0
        , perl              = TRUE
        , dryrun            = TRUE
      ),
      regexp = "sacct --format=JobID%16,JobName%5,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20 | grep 'RUNNING\\|PENDING' | grep -P abcde"
    )
    
    expect_message(
      wait_on_jobs(
        job_pattern         = "abcde"
        , jobname_nchar     = 5L
        , initial_sleep_sec = 5
        , file_list         = NULL
        , obj               = NULL
        , resub             = 0
        , perl              = FALSE
        , dryrun            = TRUE
      ),
      regexp = "sacct --format=JobID%16,JobName%5,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20 | grep 'RUNNING\\|PENDING' | grep abcde"
    )
    
  }
)

test_that(
  "wait_on_jobs produces correct warning",
  {
    # capture message as well for cleaner test pane output
    wait_on_jobs(
      job_pattern         = "abcde"
      , jobname_nchar     = 4L
      , initial_sleep_sec = 5
      , file_list         = NULL
      , obj               = NULL
      , resub             = 0
      , perl              = TRUE
      , dryrun            = TRUE
    ) %>% 
      expect_warning(regexp = "The job pattern abcde exceeds the current max of 4 characters, wait_on_jobs may not track this job correctly.") %>%
      expect_message(regexp = "sacct --format=JobID%16,JobName%4,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20 | grep 'RUNNING\\|PENDING' | grep -P abcde")
  }
)
