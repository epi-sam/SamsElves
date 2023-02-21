#' Build metadata from a template
#'
#' @param code_root [path] path to top-level code repo folder, require: path
#'   contains a `.git` subfolder
#' @param ... [named vectors] any number of named vectors to append to the
#'   metadata - be careful with this one
#'
#' @return [list] full metadata, including git info, cluster submission
#'   commands, and user-appended items
#' @import readr
#' @import data.table
#' @import stringr
#' @export
build_metadata_shell <- function(code_root, ...) {
  
  # browser()
  
  # ensure path to .git folder exists
  normalizePath(file.path(code_root, ".git"), mustWork = T)
  
  # arbitrary named list of other items to add to metadata
  CONSTANTS <- list(...)
  
  const_name_lengths <- sapply(names(CONSTANTS), nchar)
  if(any(const_name_lengths == 0)) {
    stop("All `...` arguments must be named in function call")
  }
  
  .git_logs <- data.table::fread(file.path(code_root, ".git/logs/HEAD"), header = F)
  .git_log_last <- .git_logs[nrow(.git_logs)]
  .git_hash <- stringr::str_split_fixed(.git_log_last[["V1"]], " ", n = Inf)[2]
  
  GIT <- list(
    
    git_branch      = gsub("\n", "", readr::read_file(file.path(code_root, ".git/HEAD"))),
    git_log_last    = .git_log_last,
    git_hash        = .git_hash,
    git_uncommitted = SamsElves::git_diff(code_root)
  )
  
  metadata_shell <- list(
    
    start_time      = as.character(Sys.time()),
    user            = Sys.info()[["user"]],
    GIT             = GIT,
    SUBMIT_COMMANDS = SamsElves::extract_submission_commands()
  )
  
  # add all arbitrary user elements to metadata
  metadata_shell <- append(x = metadata_shell, values = CONSTANTS)
  
  return(metadata_shell)
  
}

#' Find a string in a user's recent Slurm squeue
#'
#' Intended to find Rstudio singularity image versions for metadata.  Finds a
#' user-defined string from the submission command you used to start your
#' Rstudio singularity image (or something else you desire).  Extracts this
#' informtion from ALL jobs you currently have active in your squeue.
#'
#' @param squeue_jobname_filter [character|regex] when you run `squeue -u
#'   <username>`, what `NAME` do you want to filter for?
#' @param max_cmd_length [integer] how many characters long is your command?
#'   Increase your default if the command is truncated.  All leading/trailing
#'   whitespace is trimmed.
#' @param string_to_extract [character|regex] what string do you want to extract
#'   after running  `sacct -j <jobid> -o submitline%xxx` using
#'   `stringr::str_extract_all`
#' @param strings_to_ignore [character|regex] if your `string_to_extract`
#'   command finds more strings than you want, this removes all strings with
#'   this pattern anywhere inside using `stringr::str_detect`
#' @param user_name [character] which user's commands to find - defaults to your
#'   own
#'
#' @return [list] all desired submission commands, and specific extracted text
#'   from string_to_extract
extract_submission_commands <- function(
    
  squeue_jobname_filter = "^rst_ide",
  max_cmd_length        = 500L,
  string_to_extract     = "ihme/singularity-images/rstudio/[:graph:]+",
  strings_to_ignore     = "jpy",
  user_name             = Sys.info()[["user"]]
  
) {
  # browser()
  if (is.null(string_to_extract)) {
    stop("You must specify a string to find and extract from command line submissions")
  }
  
  # function to find user's cluster jobs
  
  job_finder <- function(user = user_name) {
    
    jobs <- system2(
      command ="squeue", 
      args = paste("-u", user), 
      stdout = T
    )
    
    jobs <- read.table(text = jobs, header = T, sep = "")
    
    # format table & extract jobids
    names(jobs) <- tolower(names(jobs))
    jobs <- jobs[, c("jobid", "name")]
    jobname_filter_mask <- grepl(squeue_jobname_filter, jobs[["name"]])
    jobs <- jobs[jobname_filter_mask, ]
    
    return(jobs)
  }
  
  # function to extract a custom string
  
  extract_command_string <- function (submit_command_element) {
    
    extracted_strings <- stringr::str_extract_all(submit_command_element, string_to_extract)
    
    # str_extract_all produces a list - need to deal with it
    if(!length (extracted_strings) == 1) {
      stop("submit_command_list has more than one element - investigate. 
             You likely have more than one pattern specified")
    }
    
    extracted_strings <- extracted_strings[[1]]
    ignore_filter <- !sapply(extracted_strings, stringr::str_detect, pattern = strings_to_ignore)
    
    # return only strings without jpy language
    return(extracted_strings[ignore_filter])
    
  }
  
  # function to extract thread from squeue
  
  extract_cores <- function(user = user_name,
                            filter = squeue_jobname_filter) {
    
    command_args <- paste("-o '%.10A %.5C' -u", user)
    
    job_threads <- system2(
      command = "squeue",
      args = command_args,
      stdout = T
    )
    
    job_threads <- read.table(text = job_threads, header = T, sep = "")
    # match Rstudio JobID to find threads
    jobs_selected <- job_finder()
    n_cores <- job_threads[job_threads$JOBID %in% jobs_selected$jobid, "CPUS", drop = F]
    # validate all dimensions before proceeding
    if(ncol(n_cores) > 1 | nrow(n_cores) > 1) {
      warning ("Attempting to find # of cores produced more than one option (either # or jobs or # of columns) - please inspect.  
               Setting to n_cores = 1, efficiency will be reduced.")
      print(n_cores)
      n_cores <- 1
    } else {
      n_cores <- n_cores$CPUS
    }
    return(n_cores)
  }
  
  # extract submission information
  
  jobs <- job_finder()
  
  submit_command_list <- list()
  
  # use sacct to extract original rstudio image submission commands
  # only works if rstudio was started from CLI, not from API
  # https://ihme.slack.com/archives/C01MPBPJ37U/p1659111543571669
  
  for (i in 1:nrow(jobs)) {
    
    job_id <- jobs[i, "jobid"]
    
    command_args <- paste(
      "-j", job_id,
      paste0("-o submitline%", max_cmd_length)
    )
    
    submission_command <- system2(
      command = "sacct",
      args = command_args, 
      stdout = T
    )[[3]]
    
    submission_command <- tolower(trimws(submission_command, which = "both"))
    
    submit_command_list[[i]] <- submission_command
    
  }
  
  extracted_cmd_strings <- lapply(submit_command_list, extract_command_string)
  
  out_list <- list(
    submission_commands = submit_command_list,
    extracted_cmd_strings = extracted_cmd_strings,
    n_cores = extract_cores(user = user_name)
  )
  
  return(out_list)
  
}