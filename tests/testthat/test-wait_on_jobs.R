test_that(
  "wait_on_slurm_job_id dry-run produces expected command string",
  {
    
    # NULL filter
    cmd_list <- wait_on_slurm_job_id(
      job_id              = c(58812401, 58659687, 58811515)
      , initial_sleep_sec = 0
      , cycle_sleep_sec   = 0
      , filter_by         = NULL
      , dryrun            = TRUE
    )
    
    expect_equal(
      cmd_list$cmd_base,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1 | grep -P '.*'"
    )
    expect_equal(
      cmd_list$cmd_pass,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1 | grep -P '.*' | grep -P 'RUNNING|PENDING'"
    )
    expect_equal(
      cmd_list$cmd_fail,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1 | grep -P '.*' | grep -P 'FAILED'"
    )
    
    # Complex filter
    cmd_list <- wait_on_slurm_job_id(
      job_id              = c(58812401, 58659687, 58811515)
      , initial_sleep_sec = 0
      , cycle_sleep_sec   = 0
      , filter_by         = c('jobidraw', 'jobname', 'user')
      , filter_regex      = "proj_geospatial"
      , dryrun            = TRUE
    )
    expect_equal(
      cmd_list$cmd_base,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1,3,4,5 | grep -P '.*' | grep -P '58812401|58659687|58811515' | grep -P proj_geospatial | grep -P ssbyrne"
      
    )
    expect_equal(
      cmd_list$cmd_pass,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1,3,4,5 | grep -P '.*' | grep -P '58812401|58659687|58811515' | grep -P proj_geospatial | grep -P ssbyrne | grep -P 'RUNNING|PENDING'"
    )
    expect_equal(
      cmd_list$cmd_fail,
      "sacct -j '58812401,58659687,58811515' --format=State%16,JobID%20,JobIDRaw%20,User%12,JobName%50,Account%20 | tail -n +3 | grep -vP 'batch|extern' | sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' | cut -d' ' -f1,3,4,5 | grep -P '.*' | grep -P '58812401|58659687|58811515' | grep -P proj_geospatial | grep -P ssbyrne | grep -P 'FAILED'"
      
    )
    
    # Check validations
    expect_warning(
      wait_on_slurm_job_id(
        job_id              = c(58812401, 58659687, 58811515)
        , initial_sleep_sec = 0
        , cycle_sleep_sec   = 0
        , filter_by         = c('account', 'jobname')
        , filter_regex      = "proj_geospatial"
        , dryrun            = TRUE
      )
      , regexp = "More than one field depends on filter_regex - this may affect  results: account, jobname"
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
