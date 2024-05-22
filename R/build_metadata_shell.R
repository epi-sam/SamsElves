#' Build metadata from a template
#'
#' Defaults currently designed for interactive Rstudio sessions.
#'
#' @param code_root [path] Path to top-level code repo folder, require: path
#'   contains a `.git` subfolder
#' @param jobname_filter [regex] When you run `sacct -u <username>`, what
#'   `JobName` indicates your interactive Rstudio session? (case-sensitive)
#'   - see defaults
#' @param submitline_n_char [int] Length of submitted command string to expect
#'   from system `sacct -j <jobid> -o submitline\%xxx` call (set this much
#'   longer than you'd think necessary)
#' @param regex_to_extract [regex] What string do you want to extract after
#'   running  `sacct -j <jobid> -o submitline\%xxx` (default - find Rstudio
#'   image for provenance)
#' @param regex_to_ignore [regex] If your `regex_to_extract` command finds more
#'   strings than you want, this removes all strings matching this pattern
#' @param system_user_name [chr] User's identifier, according to the cluster
#' @param cluster_type [chr] Only 'slurm' currently available - methods/calls
#'   may differ by system in the future. (case-insensitive)
#' @param verbose [lgl] Do you want a std_err message of arguments to this
#'   function?
#'
#' @return [list] Full metadata shell, including git info, cluster submission
#'   commands
#'
#' @export
#'
#' @examples
#' # Make metadata shell, and message the function's key arguments to console
#' (or std_err logs)
#' metadata_shell <- build_metadata_shell(
#'   code_root = "/mnt/share/code/hiv_tb_selectid/rt-shared-functions/",
#'   verbose = TRUE
#' )
#'
#' # Extract readable git diff to see any uncommitted code changes
#' # (NULL is good - no uncommitted changes, prints nothing)
#' cat(metadata_shell$GIT$git_uncommitted)
build_metadata_shell <- function(
    code_root,
    jobname_filter    = "^rst_ide|^vscode",
    submitline_n_char = 1000L,
    regex_to_extract  = "singularity-images/rstudio/[[:graph:]]+\\.img",
    regex_to_ignore   = "jpy",
    system_user_name  = Sys.info()[["user"]],
    cluster_type      = "slurm",
    verbose           = FALSE
) {

  # ensure path to .git folder exists
  normalizePath(file.path(code_root, ".git"), mustWork = TRUE)

  git_logs     <- read.table(file.path(code_root, ".git/logs/HEAD"), sep = "\t")
  git_log_last <- git_logs[nrow(git_logs), ]
  git_hash     <- strsplit(x = git_log_last[["V1"]], split = "\\s", perl = TRUE)[[1]][2]

  GIT <- list(
    git_branch      = gsub("\n", "", readLines(file.path(code_root, ".git/HEAD"))),
    git_log_last    = git_log_last,
    git_hash        = git_hash,
    git_uncommitted = SamsElves::query_git_diff(code_root = code_root)
  )

  metadata_shell <- list(
    start_time      = as.character(Sys.time()),
    user            = Sys.info()[["user"]],
    code_root       = code_root,
    GIT             = GIT,

    SUBMIT_COMMANDS = SamsElves::extract_submission_commands(
      jobname_filter    = jobname_filter,
      submitline_n_char = submitline_n_char,
      regex_to_extract  = regex_to_extract,
      regex_to_ignore   = regex_to_ignore,
      system_user_name  = system_user_name,
      cluster_type      = cluster_type
    )

  )

  # Messaging
  if(verbose){
    message(glue::glue(
      "Metadata shell from code root: {code_root}
      - user:                  {system_user_name}
      - cluster type:          {cluster_type}
      - sacct JobName filter:  {jobname_filter}
      - extracting string:     {regex_to_extract}
      - ignoring string:       {regex_to_ignore}"
    ))
  }

  # Only want to warn once from top level
  jobs_df <- job_finder(system_user_name = system_user_name,
                        jobname_filter   = jobname_filter,
                        cluster_type     = cluster_type)

  if(nrow(jobs_df) == 0) message(glue::glue(
    "Metadata warning:
    Matched no jobs to jobname_filter argument. User argument : '{jobname_filter}'
    - Returning git information, but no system information (e.g. n_cores and rstudio image are unknown)
    - Please update: call 'sacct -u <your_username>' from the command line.
    - Use '^' with the beginning JobName string as your jobname_filter argument.
    - e.g. '^rst_ide' or '^vscode' "
  ))

  return(metadata_shell)

}

#' Find a string in a user's recent Slurm sacct
#'
#' Intended to find Rstudio singularity image versions for metadata.  Finds a
#' user-defined string from the submission command you used to start your
#' Rstudio singularity image (or something else you desire).  Extracts this
#' informtion from ALL jobs you currently have active in your sacct.
#'
#' @param jobname_filter [character|regex] When you run `sacct -u <username>`,
#'   what `NAME` do you want to filter for?
#' @param submitline_n_char [int] Length of submitted command string to expect
#'   from system (set this much longer than you'd think necessary)
#' @param regex_to_extract [character|regex] What string do you want to extract
#'   after running  `sacct -j <jobid> -o submitline\%xxx`
#' @param regex_to_ignore [character|regex] If your `regex_to_extract` command
#'   finds more strings than you want, this removes strings with the specified
#'   pattern
#' @param system_user_name [chr] User's identifier, according to the cluster
#' @param cluster_type e.g. "slurm"
#'
#' @return [list] All desired submission commands, and specific extracted text
#'   from regex_to_extract
#'
#' @export
#'
extract_submission_commands <- function(

  jobname_filter,
  submitline_n_char,
  regex_to_extract,
  regex_to_ignore,
  system_user_name,
  cluster_type

) {

  if (is.null(regex_to_extract)) {
    stop("You must specify a string to find and extract from command line submissions")
  }

  # extract submission information

  jobs_df <- job_finder(system_user_name = system_user_name,
                        jobname_filter   = jobname_filter,
                        cluster_type     = cluster_type)
  jobid_vec <- jobs_df$jobid

  # use sacct to extract original rstudio image submission commands
  # only works if rstudio was started from CLI, not from API
  # https://ihme.slack.com/archives/C01MPBPJ37U/p1659111543571669

  submit_command_list <- lapply(jobid_vec, function(job_id){

    submission_command <- system2(
      command = "/opt/slurm/bin/sacct", # use full path to pass testing
      args    = glue::glue("-j {job_id} -o submitline%{submitline_n_char}"),
      stdout  = TRUE
    )
    submission_command <- tolower(submission_command[[3]])
    submission_command <- trimws(submission_command, which = "both")
  })

  extracted_cmd_strings <- lapply(
    submit_command_list,
    extract_command_string, regex_to_extract, regex_to_ignore
  )

  n_cores <- extract_cores(system_user_name = system_user_name,
                           jobname_filter   = jobname_filter,
                           cluster_type     = cluster_type)

  out_list <- list(
    submission_commands   = submit_command_list,
    extracted_cmd_strings = extracted_cmd_strings,
    n_cores               = n_cores
  )

  return(out_list)

}

#' Find cluster jobs for a user
#'
#' Filters to jobs with State = RUNNING You can filter jobs to a string match
#' (grepl()).
#'
#' @param system_user_name [chr] string identifying user on the cluster
#' @param jobname_filter [regex] filter the user's jobs to include this string
#' @param cluster_type [chr] allows methods by cluster type, if multiple are
#'   applicable - "slurm" uses `sacct`
#'
#' @return [data.frame] long by jobid, wide by jobid and jobname
#'
job_finder <- function(system_user_name,
                       jobname_filter,
                       cluster_type = "slurm") {

  valid_cluster_types  <- c("slurm")
  valid_cluster_msg    <- paste0(valid_cluster_types, collapse = ", ")
  stop_msg_clusterType <- paste("Enter a valid cluster type, options (case-insensitive):", valid_cluster_msg)

  if(is.null(cluster_type)) stop(stop_msg_clusterType)
  if(!tolower(cluster_type) %in% valid_cluster_types) stop(stop_msg_clusterType)

  # read job accounting list from cluster
  jobs_txt <- system2(
    command = glue::glue("/opt/slurm/bin/sacct"), # use full path to pass testing
    args    = glue::glue("-u {system_user_name} --format=JobID%16,JobName%16,State%10"),
    stdout  = TRUE
  )

  jobs_df  <- read.table(text = jobs_txt, header = TRUE, sep = "")

  # format, filter & extract jobids & jobnames in a table
  names(jobs_df)      <- tolower(names(jobs_df))
  jobs_df             <- jobs_df[jobs_df$state == 'RUNNING', ]
  jobname_filter_mask <- grepl(jobname_filter, jobs_df[["jobname"]])
  jobs_df             <- jobs_df[jobname_filter_mask, ]

  return(jobs_df)
}


#' Search a cluster submitted command string for some pattern
#'
#' Intended to pull Rstudio image information
#' - Currently only supports one valid string found per submission command
#'
#' @param submit_command_text [chr] the result of calling "sacct -j <INT> -o
#'   submitline\%<INT>"
#' @param regex_to_extract [regex] pattern to extract from `submit_command_text`
#' @param regex_to_ignore [regex] ignore strings found with this pattern
#'
#' @return [chr] vector of strings extracting/ignoring as requested
#'
extract_command_string <- function (submit_command_text,
                                    regex_to_extract,
                                    regex_to_ignore = NULL) {

  # str_extract_all returns a list long by either string OR pattern
  if(length(submit_command_text) > 1) stop ("Must submit text length == 1")
  if(length(regex_to_extract) > 1) stop ("Must submit regex length == 1")

  extracted_strings <- regmatches(submit_command_text, gregexpr(regex_to_extract, submit_command_text, perl = TRUE))

  # net to catch unexpected errors
  if(length (extracted_strings) != 1) {
    stop("submit_command_list does not have only one element - investigate.
          You likely have more than one string or pattern specified. \n",
         paste(capture.output(extracted_strings), collapse = "\n"))
  }

  extracted_strings <- extracted_strings[[1]]

  # net to catch unexpected errors
  if(length (extracted_strings) != 1) {
    stop("submit_command_list does not have only one element - investigate.
          You likely have more than one string or pattern specified. \n",
         paste(capture.output(extracted_strings), collapse = "\n"))
  }

  # By default this is for finding the Rstudio image - resolve latest.img if found in the string
  # After August 2023 Slurm update, the resolved image path is no longer found.
  if(grepl("latest.img$", extracted_strings)) {
    img_path <- file.path("/mnt/share", extracted_strings)
    if (!file.exists(img_path)) {
      stop(
        "
        extract_command_string error -
        latest.img path does not exist: ", img_path
      )
    }
    # resolve the symlink latest.img points to
    extracted_strings <- system(paste("realpath", img_path) , intern = TRUE)
  }

  if(is.null(regex_to_ignore)) {
    keep_mask <- rep(TRUE, length(extracted_strings))
  } else {
    keep_mask <- unlist(
      lapply(extracted_strings, function(x) !grepl(regex_to_ignore, x, perl = TRUE))
    )
  }

  return_strings <- extracted_strings[keep_mask]
  if(length(return_strings) == 0) stop("No strings were extracted - inspect inputs and regex_to_ignore")
  return(return_strings)

}

#' Extract number of interactive user cores
#'
#' NOTE: This step finds cores with squeue, rather than sacct - eases formatting
#'
#' @param system_user_name [chr] how user is identified on the cluster
#' @param jobname_filter [regex] filter to include for `job_finder()` call
#'
#' @return [int] number of available cores for multithreading
#' - if user has more than one interactive session, defaults to 1
#'
extract_cores <- function(system_user_name,
                          jobname_filter,
                          cluster_type) {

  # Find number of cores for all the user's jobs
  job_threads_txt <- system2(
    command = "/opt/slurm/bin/squeue", # use full path to pass testing
    args    = glue::glue("-o '%.10A %.5C' -u {system_user_name}"),
    stdout  = TRUE
  )
  job_threads_df <- read.table(text = job_threads_txt, header = TRUE, sep = "")

  # Find jobids matching the user's desired string
  jobs_selected_df <- job_finder(system_user_name = system_user_name,
                                 jobname_filter   = jobname_filter,
                                 cluster_type     = cluster_type)

  # build a filter mask
  jobids_threads  <- job_threads_df$JOBID
  jobids_selected <- jobs_selected_df$jobid
  jobid_mask      <- jobids_threads %in% jobids_selected

  # Filter the thread table for jobids matching user's chosen interactive session
  n_cores_df <- job_threads_df[jobid_mask, "CPUS", drop = FALSE]

  # validate dimensions
  more_than_one_interactive_session <- ncol(n_cores_df) > 1 | nrow(n_cores_df) > 1
  if(more_than_one_interactive_session) {
    message ("Metadata warning:
             Attempting to find # of cores produced more than one option (either # or jobs or # of columns) - please inspect.
             Returning n_cores = 1, efficiency may be reduced.\n",
             paste(capture.output(n_cores_df), collapse = "\n"))
    n_cores <- 1
  } else {
    n_cores <- n_cores_df$CPUS
  }
  return(as.integer(n_cores))
}
