# Given string pattern matching job name, wait while jobs are running or pending (SLURM)
wait_on_jobs <- function(job_pattern, initial_sleep = 5, file_list=NULL, obj=NULL, resub=0, perl = FALSE) {
  
  ## Give it a sec to launch
  Sys.sleep(initial_sleep)
  
  if(nchar(job_pattern) > 50) {
    warning(paste0("The job pattern ", job_pattern, " exceeds the current max of 50 characters, job_hold will not track this job correctly."))
  }
  
  ## Start timer
  start.time <- proc.time()
  
  # Save SLURM get jobs command
  slurm_get_jobs_command <- "sacct --format=JobID%16,JobName%50,User%20,State%16,ExitCode,NodeList%27,Partition%12,Account%20"
  
  # While jobs are still running or pending matching the pattern in `job_name`, sleep
  if(perl){
    found_jobs <- suppressWarnings(system(paste0(slurm_get_jobs_command, " | grep 'RUNNING\\|PENDING' | grep -P ", job_pattern), intern = T))
  } else {
    found_jobs <- suppressWarnings(system(paste0(slurm_get_jobs_command, " | grep 'RUNNING\\|PENDING' | grep ", job_pattern), intern = T))
  }
  while(length(found_jobs) > 0) {
    Sys.sleep(15)
  }
  
  ## End Timer
  job.runtime <- proc.time() - start.time
  job.runtime <- job.runtime[3]
  
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
          if (!(obj %in% h5ls(file_list)$name)) {
            missing_list <- rbind(missing_list, file)
          }
        }
      }
    }
    
    ## If missing_list > 0, break
    if (length(missing_list) > 0) {
      if (resub == 0) {
        stop(paste0("Job failed: ", job_pattern,
                    "\nTime elapsed: ", job.runtime,
                    "\nYou are missing the following files: ", toString(missing_list)))
      } else {
        return(1)
      }
    } else {
      return(0)
    }
  }
  
  ## Complete
  print(paste0("Job ", job_pattern, " has completed. Time elapsed: ", job.runtime))
}