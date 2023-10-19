#' Submit a script as a job to the Slurm cluster
#' 
#' Function contains internal defaults for R and Python shell scripts.
#' Function will build log paths automatically.
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
#' @param r_image [chr] (default "latest.img") e.g. "/ihme/singularity-images/rstudio/ihme_rstudio_4214.img"
#' @param args_list [list, chr] optional list() of arguments, e.g. list("--arg1" = arg1, "--arg2" = arg2)
#' @param dry_runTF [lgl] (default FALSE) if TRUE, only message and return submission command, no job submission
#'
#' @return [std_err] (message) submitted command and response from system re: submitted job status
#' @export
submit_job <- function(
    language          = "R",
    shell_script_path = NULL, 
    script_path       = NULL, 
    std_err_path      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
    std_out_path      = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
    job_name          = NULL, 
    archiveTF         = FALSE,  
    mem_GB            = "10G", 
    threads           = "2", 
    runtime_min       = "15", 
    partition         = "all.q", 
    Account           = NULL, 
    r_image           = NULL,  
    args_list         = NULL,
    dry_runTF         = FALSE
) {
  
  # Argument validation 
  ## coding language
  valid_langs     <- c("r", "python")
  valid_langs_msg <- paste0(valid_langs, collapse = ", ")
  if(is.null(language)) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  language        <- tolower(language)
  if(!language %in% valid_langs) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  ## others
  if(is.null(script_path)) stop("Please define a valid script path to submit")
  if(is.null(Account))     stop("Please define a Slurm Account e.g. proj_cov_vc")

  # build log folders silently (dir.create fails naturally if directory exists)
  dir.create(std_err_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(std_out_path, recursive = TRUE, showWarnings = FALSE)
  
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
    
  } else if (language == "python") {
    
    if(is.null(shell_script_path)) {
      shell_script_path <- system.file("py/python_shell_slurm.sh", package = "SamsElves")
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
    " -s ",    script_path
  )
  
  # append extra arguments - handles NULL input by default
  for (arg_name in names(args_list)) { 
    command <- paste(command, arg_name, args_list[arg_name])
  }
  
  message(command, "\n")
  
  if(dry_runTF) return(command)
  
  submission_return <- system(command, intern = T)
  message(paste("Cluster job submitted:", job_name, "; Submission return code:", submission_return))
  return(submission_return)
  
}

# scoping
# language               = "R"
# script_path            = file.path(.bootstrap_root, "module_simmod/run_eppasm_for_location_by_draw_and_scenario.R")
# std_err_path           = file.path(.output_root, "std_err")
# std_out_path           = file.path(.output_root, "std_out")
# job_name               = .job_name
# mem_GB                 = "40G"
# shell_script_path      = NULL
# archiveTF              = T
# use_paths_file_r_image = T
# Account                = "proj_hiv"
# partition              = "all.q"
# threads                = as.character(PARAMS$n_cores_eppasm)
# runtime_min            = "60"
# job_name               = NULL
# r_image                = NULL
# args_list =
#   list("--bootstrap_root"  = .bootstrap_root,
#        "--output_root"     = .output_root,
#        "--n_cores"         = as.integer(PARAMS$n_cores_eppasm),
#        "--loc_id"          = loc_id,
#        "--available_draws" = available_draws)