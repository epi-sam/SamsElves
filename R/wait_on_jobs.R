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
