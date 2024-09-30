#' Wait for a Slurm job to finish (find `RUNNING|PENDING` State jobs).
#'
#' Option to break if some jobs fail (find `FAILED` State jobs).
#'
#' Option to filter `sacct` results by multiple fields with `grep -P`.
#'
#' Works in batches to accommodate grep limitations (default 500 `job_id`s)
#' - All batch 1 jobs are checked before moving to batch 2.
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
#' @param initial_sleep_sec [int] how long to sleep before initial check for
#'   jobs on cluster
#' @param cycle_sleep_sec [int] how long to wait between checks
#' @param filter_by [chr] vector of sacct fields to search e.g. `c("User",
#'   "JobName")` (case insensitive)
#' @param filter_regex [regex] required if `filter_by %in% c("JobName",
#'   "Account")`
#' @param break_on_failure [lgl] if _any_ of your jobs fail, should this
#'   function break?  Failure itself is always determined based on the user's
#'   filters, but failure _feedback_ always returns JobID, regardless of
#'   filtering. This is due to how jobs are initially queried.  This may
#'   included unwanted recycled `JobID`s, and it is **_up to the user_** to
#'   determine which are relevant to their work.
#' @param dryrun [lgl] return a list of commands built by this function, but do
#'   not wait on jobs (only returns command for the first batch - see
#'   `batch_size`)
#' @param batch_size [int] how many jobs to group together to wait on (grep
#'   limitation in max number, 500 is a good default, could increase near 900).
#'   All jobs in batch 1 must finish before batch 2 is checked.
#'
#' @return [std_out/std_err] std_out for sleep cycle duration & successful
#'   ending, std_err printing failed job ids
#' @export
wait_on_slurm_job_id <-
  function(
    job_id,
    initial_sleep_sec = 30,
    cycle_sleep_sec   = 30,
    filter_by         = c("jobidraw"),
    filter_regex      = NULL,
    break_on_failure  = FALSE,
    dryrun            = FALSE,
    batch_size        = 500
  ) {

    # sacct formatting
    format_list <- list(
      state    = "State%16",
      jobid    = "JobID%20",
      jobidraw = "JobIDRaw%20",
      user     = "User%12",
      jobname  = "JobName%50",
      account  = "Account%20"
    )
    names(format_list) <- tolower(names(format_list))
    if(!names(format_list)[[1]] == "state") stop("state must be first `format_list` object element")
    if(!names(format_list)[[2]] == "jobid") stop("jobid must be second `format_list` object element")
    formatting_str    <- paste0("--format=", paste(format_list, collapse = ","))

    irrelevant_fields <- c("state", "jobid")
    regex_req_fields  <- c("jobname", "account")

    filter_by     <- unique(tolower(filter_by))
    valid_filters <- setdiff(names(format_list), irrelevant_fields)

    # argument validation
    if(!all(filter_by %in% valid_filters)) stop("filter_by must be one of: ", paste(c(valid_filters, "(case-insensitive) or NULL"), collapse = ", "))
    if(is.null(filter_regex) && any(regex_req_fields %in% filter_by)) stop("must define filter_regex for: ",
                                                                           paste(regex_req_fields, collapse = ", "))
    regex_user_fields <- filter_by[filter_by %in% regex_req_fields]
    if(length(regex_user_fields) > 1) warning("More than one field depends on filter_regex - this may affect  results: ",
                                              paste(regex_user_fields, collapse = ", "))
    # State is required in first position
    filter_by <- c("state", unique(tolower(filter_by)))

    start.time <- proc.time()
    Sys.sleep(initial_sleep_sec)

    # Break job_id into batches
    batches_n <- ceiling(length(job_id)/batch_size)
    if(max(batches_n) > 1) print(paste("Warning: You have submitted more than", batch_size, "JobIDs." ,
                                       "Waiting for all jobs in batch_idx 1 before proceeding to batch_idx 2, etc. (grep limitation)"))

    # Loop over the jobs in batches_n (too may job_ids leads to grep errors)
    for(batch_idx in 1:batches_n){

      print(paste("Waiting on batch_idx:", batch_idx))

      job_idx <- list(
        first = batch_idx * batch_size - (batch_size - 1),
        last  = min((batch_idx * batch_size), length(job_id))
      )
      job_id_batch  <- job_id[job_idx$first : job_idx$last]

      job_id_regex_raw          <- paste(job_id_batch, collapse = "|")
      job_id_regex_or_quoted    <- paste0("'", job_id_regex_raw, "'")
      job_id_regex_comma_quoted <- paste0("'", paste(job_id, collapse = ","), "'")

      # Format sacct output to flexibly filter by multiple fields
      filter_regex_list <- lapply(filter_by, function(flt){
        switch(
          flt,
          "state"    = "'.*'", # filter nothing - handles filter_by = NULL
          # "jobid"    = job_id_regex_or_quoted, # has no meaning - we're querying JobID directly
          "jobidraw" = job_id_regex_or_quoted,
          "user"     = Sys.info()[["user"]],
          "jobname"  = filter_regex,
          "account"  = filter_regex
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

      # trim column headers (start on 3rd row of sacct output)
      # trim out duplicated batch/extern lines for each job
      # trim extra whitespace, leaving a one-space-delimiter between fields, used by `cut`
      # select only filtering fields of choice
      # - cut's `-f{filter_idx}` selects `1+n` fields (e.g. `1,2,3`)
      #   - `1` = "state" and `n` = the user-defined filtering field(s)
      # add filter_regex
      filter_str <- paste0(
        "| tail -n +3 ",
        "| grep -vP 'batch|extern' ",
        "| sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' ",
        "| cut -d' ' -f", filter_idx_str,
        filter_regex
      )

      # relevant if user does not filter by JobIDRaw (see `break_for_failed_jobs()`)
      filter_str_fail_feedback <- paste0(
        "| tail -n +3 ",
        "| grep -vP 'batch|extern' ",
        "| sed 's/ \\+/ /g; s/^[[:space:]]*//; s/[[:space:]]*$//' ",
        "| cut -d' ' -f", "1,2"
      )

      # Build submission commands
      cmd_base <- paste("sacct -j", job_id_regex_comma_quoted, formatting_str, filter_str)
      cmd_pass <- paste0(cmd_base, " | grep -P 'RUNNING|PENDING'")
      cmd_fail <- paste0(cmd_base, " | grep -P 'FAILED'")
      # need job_id included for failure feedback
      cmd_fail_feedback <-paste("sacct -j",
                                job_id_regex_comma_quoted,
                                formatting_str,
                                filter_str_fail_feedback,
                                "| grep -P 'FAILED'")

      if(dryrun) return(list(cmd_base = cmd_base, cmd_pass = cmd_pass, cmd_fail = cmd_fail, cmd_fail_feedback = cmd_fail_feedback))
      if(!length(suppressWarnings(system(cmd_base, intern = TRUE)))) stop ("No jobs found: ", cmd_base)

      # Stop for immediately FAILED jobs
      if(break_on_failure) break_for_failed_jobs(cmd_fail, cmd_fail_feedback, job_id_regex_raw, filter_by)

      print("seconds elapsed:")

      # Sleep while jobs matching `job_id` and user defined filters are RUNNING or PENDING
      while(length(suppressWarnings(system(cmd_pass, intern = T))) > 0 ) {
        Sys.sleep(cycle_sleep_sec)
        print(round((proc.time() - start.time)[[3]],0))
        # Stop if any jobs have FAILED State
        if(break_on_failure) break_for_failed_jobs(cmd_fail, cmd_fail_feedback, job_id_regex_raw, filter_by)
      }

    }

    # End Timer
    job.runtime <- proc.time() - start.time
    job.runtime <- round(job.runtime[3], 0)

    # Complete
    job_id_msg <- paste(job_id, collapse = ", ")
    print(paste0("Job(s) ", job_id_msg, " no longer PENDING, RUNNING, or FAILED. Time elapsed: ", job.runtime, " seconds"))
  }

#' Helper function for wait_on_slurm_job_id - how do you want jobs to break and display user messages?
#'
#' @param cmd_fail [chr]
#' @param cmd_fail_feedback [chr]
#' @param job_id_regex_raw [regex]
#' @param filter_by [chr]
#'
#' @return [none] stop on failure
break_for_failed_jobs <-
  function(
    cmd_fail,
    cmd_fail_feedback,
    job_id_regex_raw,
    filter_by
  ) {
    # if user is filtering on JobIDRaw, return relevant jobs and feedback, otherwise return all JobIDs found by initial Slurm query
    jobid_present     <- any(grepl("jobidraw", filter_by))
    failure_message   <- ifelse(jobid_present,
                                "There is a failure among the launched jobs, investigate.\n",
                                "There is a failure among the launched jobs, investigate. (NOTE! May include recycled JobIDs)\n")
    cmd_fail_feedback <-ifelse(jobid_present,
                               cmd_fail,
                               cmd_fail_feedback)
    fail_chk          <- suppressWarnings(system(cmd_fail, intern = TRUE))
    fail_feedback     <- suppressWarnings(system(cmd_fail_feedback, intern = TRUE))
    failed_jobs       <- paste(unique(regmatches(fail_feedback, regexpr(job_id_regex_raw, fail_feedback))), collapse = ", ")
    if(length(fail_chk)) stop(failure_message, failed_jobs)
  }
