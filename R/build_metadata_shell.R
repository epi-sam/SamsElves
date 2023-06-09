#' Build metadata from a template
#'
#' @param code_root [path] path to top-level code repo folder, require: path
#'   contains a `.git` subfolder
#'
#' @return [list] full metadata, including git info, cluster submission
#'   commands, and user-appended items
#' @import readr
#' @import data.table
#' @import stringr
#' @export
build_metadata_shell <- function(code_root) {
  
  # browser()
  
  # ensure path to .git folder exists
  normalizePath(file.path(code_root, ".git"), mustWork = T)
  
  .git_logs     <- data.table::fread(file.path(code_root, ".git/logs/HEAD"), header = F)
  .git_log_last <- .git_logs[nrow(.git_logs)]
  .git_hash     <- stringr::str_split_fixed(.git_log_last[["V1"]], " ", n = Inf)[2]
  
  GIT <- list(
    git_branch      = gsub("\n", "", readr::read_file(file.path(code_root, ".git/HEAD"))),
    git_log_last    = .git_log_last,
    git_hash        = .git_hash,
    git_uncommitted = SamsElves::query_git_diff(CODE_ROOT = code_root)
  )
  
  metadata_shell <- list(
    start_time      = as.character(Sys.time()),
    user            = Sys.info()[["user"]],
    CODE_ROOT       = code_root,
    GIT             = GIT,
    SUBMIT_COMMANDS = SamsElves::extract_submission_commands()
  )
  
  return(metadata_shell)
  
}


#' Find cluster jobs for a user using 
#' 
#' You can filter jobs to a string match (grepl()).
#'
#' @param system_user_name [chr] string identifying user on the cluster
#' @param squeue_jobname_filter [regex] filter the user's jobs to include this string
#' @param cluster_type [chr] allows methods by cluster type, if multiple are applicable
#' - "slurm" uses `sacct`
#'
#' @return [data.frame] long by jobid, wide by jobid and jobname
#' @export
#'
#' @examples
job_finder <- function(system_user_name, 
                       squeue_jobname_filter,
                       cluster_type = "slurm") {
  
  valid_cluster_types <- c("slurm")
  cluster_type        <- tolower(cluster_type)
  valid_cluster_msg   <- paste0(valid_cluster_types, collapse = ", ")
  
  if(!cluster_type %in% valid_cluster_types) {
    stop(glue("Enter a valid cluster type, options (case-insensitive): {valid_cluster_msg}"))
  }
  
  jobs_txt <- system2(
    command = "sacct",
    args    = c(
      paste("-u", system_user_name),
      "--format=JobID%16,JobName%16"
    ),
    stdout  = T
  )
  
  jobs_txt <- tolower(jobs_txt)
  jobs_df  <- read.table(text = jobs_txt, header = T, sep = "")
  
  # format table & extract jobids
  jobname_filter_mask <- grepl(squeue_jobname_filter, jobs_df[["jobname"]])
  jobs_df             <- jobs_df[jobname_filter_mask, ]
  
  return(jobs_df)
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
    
  squeue_jobname_filter = "^rst_ide|^vscode",
  max_cmd_length        = 500L,
  string_to_extract     = "ihme/singularity-images/rstudio/[:graph:]+",
  strings_to_ignore     = "jpy",
  user_name             = Sys.info()[["user"]],
  cluster_type          = "slurm"
  
) {
  # browser()
  if (is.null(string_to_extract)) {
    stop("You must specify a string to find and extract from command line submissions")
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
    jobs_selected <- job_finder(system_user_name = user_name,
                                squeue_jobname_filter = squeue_jobname_filter, 
                                cluster_type = cluster_type)
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
  
  jobs <- job_finder(system_user_name = user_name,
                     squeue_jobname_filter = squeue_jobname_filter, 
                     cluster_type = cluster_type)
  
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