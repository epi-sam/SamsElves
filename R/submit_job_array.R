#' Submit an array job to the slurm cluster (only R supported 2023-10-16)
#' 
#' See implementation here:
#' https://stash.ihme.washington.edu/projects/HIVTBID/repos/hiv_sdg_analyses/browse/launch_sdg.R?at=develop#385-428
#'
#'
#' @param language [chr] coding language for job (see valid_langs validation)
#' @param shell_script_path [path] path to shell script (language-specific)
#' @param script_path [path] full path to submitted script
#' @param std_err_path [path] path for Slurm std_err logs 
#' @param std_out_path [path] path for Slurm std_out logs 
#' @param job_name [chr] Will be name of script if NULL
#' @param archiveTF [lgl] (default FALSE) do you need an archive node?
#' @param mem_GB [chr] cluster resource requirement
#' @param threads [chr] cluster resource requirement
#' @param runtime_min [chr] cluster resource requirement
#' @param partition [chr] a.k.a. 'queue' - cluster resource requirement
#' @param Account [chr] a.k.a. 'project' - cluster resource requirement
#' @param array_first_task [chr] (default "1") index of first array task ID
#' @param array_n_tasks [chr] number of tasks per array 
#' @param hold_for_JobIDs [int] vector of jobids that must complete successfully before running this job (https://slurm.schedmd.com/sbatch.html#OPT_dependency)
#' @param r_image [chr] (default "latest.img") e.g. "/ihme/singularity-images/rstudio/ihme_rstudio_4214.img"
#' @param args_list [list, chr] optional list() of arguments, e.g. list("--arg1" = arg1, "--arg2" = arg2)
#' @param dry_runTF [lgl] (default FALSE) if TRUE, only message and return submission command, no job submission
#'
#' @return [list] 2 items - list(command submitted to cluster, cluster submission reply)
#' @export
submit_job_array <- function(
    language          = "R", 
    shell_script_path = NULL,
    script_path       = NULL,
    std_err_path      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),  # default to /ihme/temp/[cluster]/usr
    std_out_path      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"), # default to /ihme/temp/[cluster]/usr
    job_name          = NULL,
    archiveTF         = FALSE,
    mem_GB            = "10G", 
    threads           = "2", 
    runtime_min       = "15", 
    partition         = "all.q", 
    Account           = NULL,
    array_first_task  = "1", 
    array_n_tasks     = NULL, 
    hold_for_JobIDs   = NULL, 
    r_image           = NULL,
    args_list         = NULL, 
    dry_runTF         = FALSE
){
  
  # Argument validation 
  ## coding language
  valid_langs     <- c("r")
  valid_langs_msg <- paste0(valid_langs, collapse = ", ")
  if(is.null(language)) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  language        <- tolower(language)
  if(!language %in% valid_langs) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  ## others
  if(is.null(script_path))   stop("Please define a valid script path to submit")
  if(is.null(Account))       stop("Please define a Slurm Account e.g. proj_cov_vc")
  if(is.null(array_n_tasks)) stop("Please define number of jobs per array (1 to array_n_tasks)")
  if(!is.null(hold_for_JobIDs)){
     if(!is.vector(hold_for_JobIDs, mode = "integer")) stop("hold_for_JobIDs must be a simple integer vector")
  }
  
  # build log folders silently (dir.create fails naturally if directory exists)
  dir.create(std_err_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(std_out_path, recursive = TRUE, showWarnings = FALSE)
  
  # temp fix for the image not being able to find the singularity 2/8/22
  # old_path <- Sys.getenv("PATH")
  # Sys.setenv(PATH = paste(old_path, "/opt/singularity/bin", sep = ":"))
  
  # Define commands
  if (is.null(job_name)) {
    script_path_decon <- unlist(strsplit(script_path, "[/.]"))
    job_name          <- script_path_decon[length(script_path_decon) - 1]
  }
  
  ## Code language
  if(language == "r") {
    
    if(is.null(r_image)) {
      r_image_cmd <- "-i /ihme/singularity-images/rstudio/latest.img"
    } else {
      r_image_cmd <- paste0("-i ", r_image)
    }
    
    if(is.null(shell_script_path)) {
      shell_script_path <- "/ihme/singularity-images/rstudio/shells/execRscript.sh"
    }
    
  } 
  
  ## format for scheduler
  std_err_path <- file.path(std_err_path, "%x_e%j.log")
  std_out_path <- file.path(std_out_path, "%x_o%j.log")
  archive_cmd  <- ifelse(archiveTF, " -C archive", "")
  
  # build system command string
  command <- paste0(
    "sbatch",
    " -J ",    job_name,
    "",        archive_cmd,
    " --mem=", mem_GB,
    " -c ",    threads,
    " -t ",    runtime_min,
    " -p ",    partition,
    " -A ",    Account,
    " -e ",    std_err_path,
    " -o ",    std_out_path, 
    " "   ,    shell_script_path,
    " "   ,    r_image_cmd,
    " -s ",    script_path,
    paste0("--array=", array_first_task, "-", array_n_tasks)
    # , " -D ./" # FIXME SB - 2023 Oct 20 - I'm not sure I want this in here
  )
  
  # add hold_for_JobIDs if exists
  if(!is.null(hold_for_JobIDs)){
      hold_ids <- paste(hold_for_JobIDs, collapse = ":")
      command  <- paste0(command, " --dependency=afterok:", hold_ids)
  }
  
  # append extra arguments - handles NULL input by default
  for (arg_name in names(args_list)) { 
    command <- paste(command, arg_name, args_list[arg_name])
  }
  
  message(command, "\n")
  
  if(dry_runTF) return(command)
  
  submission_return <- system(command, intern = T)
  message(paste("Cluster job submitted:", job_name, "; Submission return code:", submission_return))
  
  ## and return jid
  out_list <- list(command           = command,
                   submission_return = submission_return)
  return(out_list)
}
