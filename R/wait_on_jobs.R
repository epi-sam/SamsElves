# Given string pattern matching job name, wait while jobs are running or pending (SLURM)
#'
#' @param job_pattern [regex] a (perl) regular expression - submitted to `gnu grep` system commend
#'
#' @param initial_sleep_sec [int] time to allow system time to find a submitted job
#' @param jobname_nchar [int] (defualt: 50) how many characters of a JobName to display and search
#' @param file_list [path/list] list of paths to files to wait on
#' @param obj 
#' @param resub 
#' @param dryrun (default FALSE) if TRUE, only message and return system command, but do not submit
#'
#' @return [std_out] message to user
#' @export
#' 
#' @import rhdf5
#' @import glue
wait_on_jobs <- function(job_pattern,
                         jobname_nchar     = 50L,
                         initial_sleep_sec = 5L,
                         file_list         = NULL,
                         obj               = NULL,
                         resub             = 0,
                         perl              = FALSE,
                         dryrun            = FALSE) {
  
  # FIXME SB - 2023 Oct 20 - UNDER CONSTRUCTION
  # - NEXT PHASE IS SORTING OUT THE FILE_LIST SECTION, MAKING IT MORE GENERIC
  
  # argument validation
  if(!is.vector(jobname_nchar, mode = "integer")) stop("jobname_nchar must be a single integer.")
  if(length(jobname_nchar) > 1) stop("jobname_nchar must be a single integer.")
  
  if(nchar(job_pattern) > jobname_nchar) {
    warning(glue::glue("The job pattern {job_pattern} exceeds the current max of {jobname_nchar} characters, wait_on_jobs may not track this job correctly."))
  }
  
  # Save SLURM get jobs command
  slurm_get_jobs_command <- glue::glue("sacct --format=JobID%16,JobName%{jobname_nchar},User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20")
  perl_stub <- ifelse(perl, "-P ", "")
  cmd <- paste0(slurm_get_jobs_command, " | grep 'RUNNING\\|PENDING' | grep ", perl_stub, job_pattern)
  
  if(dryrun) message(cmd); return(cmd)
  
  ## Give jobs time to be discoverable on the cluster
  Sys.sleep(initial_sleep_sec)
  
  ## Start timer
  job_starttime <- proc.time()
  
  # While jobs are still running or pending matching the pattern in `job_name`, sleep
  while(length(suppressWarnings(system(cmd, intern = T))) > 0 ) {
    Sys.sleep(15)
    print(round((proc.time() - start.time)[[3]],0))
  }
  
  ## End Timer
  job_runtime <- proc.time() - job_starttime
  job_runtime <- job_runtime[3]
  
  ## Check for the file list
  if (!is.null(file_list)) {
    ## Give it another sec
    Sys.sleep(5)
    missing_list <- NULL
    for (file in file_list) {
      ## Ensure that all files are there
      if (!file.exists(file)) {
        missing_list <- rbind(missing_list, file)
        ## Check obj if hdf
      } else {
        if (grepl(".h5", file_list[1])) {
          if (!(obj %in% rhdf5::h5ls(file_list)$name)) {
            missing_list <- rbind(missing_list, file)
          }
        }
      }
    }
    
    ## If missing_list > 0, break
    if (length(missing_list) > 0) {
      if (resub == 0) {
        stop(glue::glue("Job failed: {job_pattern}
                        Time elapsed: {job_runtime}
                        You are missing the following files: {toString(missing_list)}"))
      } else {
        return(1)
      }
    } else {
      return(0)
    }
  }
  
  ## Complete
  print(glue::glue("Job(s) {job_pattern} completed. Time elapsed: {job_runtime}"))
}


#' Wait for a Slurm job to finish (find `RUNNING|PENDING` State jobs).
#' 
#' Option to break if some jobs fail (find `FAILED` State jobs).
#' 
#' Option to filter `sacct` results by multiple fields with `grep -P`
#' 
#' First find all jobs with a given base `JobID` (fastest search method)
#'  - default behavior: next filter for `JobID` matching `JobIDRaw` 
#'  - there may be duplicate `JobIDRaw`, so you could also filter by active `User`
#' 
#' If you are submitting _array jobs_ they may overlap with old `JobID`s:
#'  - you'll get one `JobID` back from the system when you submit an `sbatch` for an array
#'    - it will only match a single `JobIDRaw` 
#'      - e.g. `1234` is returned for an array of `1234_1` and `1234_2`, which have `JobIDRaw` of `1234` and `1235` under the hood
#'      - if you filtered on `JobIDRaw`, you'd only find the first array job, and miss the others
#'  - instead of filtering on `JobIDRaw`, it's probably more helpful to filter on `User` and/or `JobName`
#' 
#' **NOTE:**
#' Slurm recycles the `JobID` field, which may cause ambiguity between the
#' user's current job, and another user's prior job.  This `JobID` may further
#' share an ID with a prior, recycled array job. To resolve this fundamental
#' weakness, the user may filter on various `sacct` fields. Supported fields are
#' listed below (case-insensitive). 
#' - filtering is somewhat limited compared to data.frames (`grep` limitation)
#' - all specified fields are filtered simultaneously, rather than individually 
#' 
#' Currently supported: 
#' - `NULL`       - apply no filters
#' - `"JobIDRaw"` - **default option** - strictly filter for specified `JobID` = `JobIDRaw`
#'                - filter to strictly include single jobs and _exclude any array jobs_ that may match the base `JobID`
#'                - will filter to the most recent unique `JobIDRaw` if duplicates exist (Slurm behavior at time of writing)
#' - `"User"`     - filter to only the current active user's jobs
#'                - **NOTE:** The `User` field can only find the active user in Rstudio 
#'                - Cannot find other `User`s - Singularity container limitation (returns `nobody`)
#' - `"JobName"`  - filter according to a `filter_regex` regex pattern
#' - `"Account"`  - filter according to a `filter_regex` regex pattern
#' 
#' Slurm sacct field documentation: 
#' - https://slurm.schedmd.com/sacct.html#OPT_format
#' - https://slurm.schedmd.com/sacct.html#OPT_helpformat 
#' 
#' @param job_id [int] a Slurm `JobID` (single or vector)
#' @param initial_sleep_sec [int] how long to sleep before initial check for jobs on cluster
#' @param cycle_sleep_sec [int] how long to wait between checks
#' @param filter_by [chr] vector of sacct fields to search e.g. `c("User", "JobName")` (case insensitive)
#' @param filter_regex [regex] required if `filter_by %in% c("JobName", "Account")`
#' @param break_on_failure [lgl] if _any_ of your jobs fail, should this function break?
#' @param dryrun [lgl] return a list of commands built by this function, but do not wait on jobs
#'
#' @return [std_out/std_err] std_out for sleep cycle duration & successful ending, std_err printing failed job ids
wait_on_slurm_job_id <-
  function(
    job_id,
    initial_sleep_sec = 30,
    cycle_sleep_sec   = 30,
    filter_by         = c("jobidraw"), 
    filter_regex      = NULL, 
    break_on_failure  = FALSE,
    dryrun            = FALSE
  ) {
    
    # sacct formatting 
    format_list <- list(
      state      = "State%16"
      , jobid    = "JobID%20"
      , jobidraw = "JobIDRaw%20"
      , user     = "User%12"
      , jobname  = "JobName%50"
      , account  = "Account%20"
    )
    names(format_list) <- tolower(names(format_list))
    formatting_str    <- paste0("--format=", paste(format_list, collapse = ","))
    if(!names(format_list)[[1]] == "state") stop("state must be first `format_list` object element")
    
    irrelevant_fields <- c("state", "jobid")
    regex_req_fields  <- c("jobname", "account")
    
    filter_by     <- unique(tolower(filter_by))
    valid_filters <- setdiff(names(format_list), irrelevant_fields)
    
    # argument validation
    if(!all(filter_by %in% valid_filters)) stop("filter_by must be one of: ", paste(c(valid_filters, "NULL"), collapse = ", "))
    if(is.null(filter_regex) && any(regex_req_fields %in% filter_by)) stop("must define filter_regex for: ",
                                                                           paste(regex_req_fields, collapse = ", "))
    regex_user_fields <- filter_by[filter_by %in% regex_req_fields]
    if(length(regex_user_fields) > 1) warning("More than one field depends on filter_regex - this may affect  results: ",
                                              paste(regex_user_fields, collapse = ", "))
    # State is required in first position 
    filter_by <- c("state", unique(tolower(filter_by)))
    
    start.time <- proc.time()
    Sys.sleep(initial_sleep_sec)
    
    job_id_regex_raw          <- paste(job_id, collapse = "|")
    job_id_regex_or_quoted    <- paste0("'", job_id_regex_raw, "'")
    job_id_regex_comma_quoted <- paste0("'", paste(job_id, collapse = ","), "'")
    
    # Format sacct output to flexibly filter by multiple fields
    filter_regex_list <- lapply(filter_by, function(flt){
      switch(
        flt
        , "state"    = "'.*'" # filter nothing - handles filter_by = NULL
        # , "jobid"    = job_id_regex_or_quoted # has no meaning - we're querying JobID directly
        , "jobidraw" = job_id_regex_or_quoted
        , "user"     = Sys.info()[["user"]]
        , "jobname"  = filter_regex
        , "account"  = filter_regex
      )
    })
    names(filter_regex_list) <- filter_by
    filter_idx               <- which(names(format_list) %in% filter_by)
    filter_idx_str           <- paste(filter_idx, collapse = ",")
    
    # Build the filtering commands
    filter_regex <- character()
    for(rgx in filter_regex_list){
      filter_regex <- paste0(filter_regex, " | grep -P ", rgx)
    }
    
    #' trim column headers (start on 3rd row of sacct output)
    #' trim out duplicated batch/extern lines for each job
    #' trim extra whitespace, leaving a one-space-delimiter between fields, used by `cut`
    #' select only filtering fields of choice 
    #' - cut's `-f{filter_idx}` selects `1+n` fields (e.g. `1,2,3`)
    #'   - `1` = "state" and `n` = the user-defined filtering field(s)
    # add filter_regex 
    filter_str <- paste0(
      "| tail -n +3 ",
      "| grep -vP 'batch|extern' ",
      "| sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' ", 
      "| cut -d' ' -f", filter_idx_str, 
      filter_regex
    )
    
    # Build submission commands
    cmd_base <- paste("sacct -j", job_id_regex_comma_quoted, formatting_str, filter_str)
    cmd_pass <- paste0(cmd_base, " | grep -P 'RUNNING|PENDING'")
    cmd_fail <- paste0(cmd_base, " | grep -P 'FAILED'")
    
    if(dryrun) return(list(cmd_base = cmd_base, cmd_pass = cmd_pass, cmd_fail = cmd_fail))  
    
    if(!length(suppressWarnings(system(cmd_base, intern = TRUE)))) stop ("No jobs found: ", cmd_base)
    
    # Stop for immediately FAILED jobs
    if(break_on_failure){
      fail_chk    <- suppressWarnings(system(cmd_fail, intern = TRUE))
      failed_jobs <- paste(unique(regmatches(fail_chk, regexpr(job_id_regex_raw, fail_chk))), collapse = ", ")
      if(length(fail_chk)) stop('there is a failure among the launched jobs, investigate:\n', failed_jobs)
    }
    
    print("seconds elapsed:")
    
    # Sleep while jobs matching `job_id` and user defined filters are RUNNING or PENDING 
    while(length(suppressWarnings(system(cmd_pass, intern = T))) > 0 ) {
      Sys.sleep(cycle_sleep_sec)
      print(round((proc.time() - start.time)[[3]],0))
      
      # Stop if any jobs have FAILED State
      if(break_on_failure){
        fail_chk    <- suppressWarnings(system(cmd_fail, intern = TRUE))
        failed_jobs <- paste(unique(regmatches(fail_chk, regexpr(job_id_regex_raw, fail_chk))), collapse = ", ")
        if(length(fail_chk)) stop('there is a failure among the launched jobs, investigate:\n', failed_jobs)
      }
    }
    
    # End Timer
    job.runtime <- proc.time() - start.time
    job.runtime <- round(job.runtime[3], 0)
    
    # Complete
    job_id_msg <- paste(job_id, collapse = ", ")
    print(paste0("Job(s) ", job_id_msg, " no longer PENDING, RUNNING, or FAILED. Time elapsed: ", job.runtime, " seconds"))
  }
