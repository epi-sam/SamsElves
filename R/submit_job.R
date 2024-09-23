#' Submit a script as a job to the Slurm cluster
#'
#' Function contains internal defaults for R and Python shell scripts.
#' Function will build log paths automatically.
#'
#' @param script_path [chr] full path to submitted script
#' @param threads [int] cluster resource requirement
#' @param mem [chr] cluster resource requirement
#' @param runtime_min [num] cluster resource requirement
#' @param array_tasks_int [int] (default: NULL - if not NULL, array job is assumed) - vector of integers for you array (e.g. c(1L:10L))
#' @param archiveTF [lgl] (default FALSE) do you need an archive node?
#' @param job_name [chr] Will be name of script if NULL
#' @param partition [chr] a.k.a. 'queue' - cluster resource requirement
#' @param account [chr] a.k.a. 'project' - cluster resource requirement
#' @param hold_for_JobIDs vector of jobids that must complete successfully before running this job (https://slurm.schedmd.com/sbatch.html#OPT_dependency)
#' @param language [chr] coding language for job (see valid_langs validation)
#' @param r_image [chr] (default "latest.img") e.g. "/mnt/share/singularity-images/rstudio/ihme_rstudio_4214.img"
#' @param shell_script_path [path] path to shell script (language-specific)
#' @param std_err_root [chr] path for Slurm std_err logs
#' @param std_out_root [chr] path for Slurm std_out logs
#' @param console_style_log_tf [lgl] if TRUE, combine std_err and std_out into one log in the std_out_root
#' @param args_list [list, chr] optional named list() of arguments, e.g. list("arg1" = arg1, "arg2" = arg2)
#' @param arg_vecs_to_comma_str [lgl] if TRUE, convert atomic elements of args_list to comma-separated strings
#' @param verbose [lgl] print submission command and job_id
#' @param v_verbose [lgl] print log paths
#' @param send_email [lgl] send email on job completion?
#' @param dry_runTF [lgl] (default FALSE) if TRUE, only message and return submission command, no job submission
#'
#' @return [int] job_id of submitted job, also messsage with job_id and job_name
#' @export
submit_job <- function(
    script_path           = NULL,
    threads               = 2L,
    mem                   = "10G",
    runtime_min           = 15L,
    array_tasks_int       = NULL,
    archiveTF             = TRUE,
    job_name              = NULL,
    partition             = "all.q",
    account               = NULL,
    hold_for_JobIDs       = NULL,
    language              = "R",
    r_image               = NULL,
    shell_script_path     = NULL,
    std_err_root          = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "error"),
    std_out_root          = file.path("/mnt/share/temp/slurmoutput", Sys.getenv()["USER"], "output"),
    console_style_log_tf  = FALSE,
    args_list             = NULL,
    arg_vecs_to_comma_str = TRUE,
    verbose               = TRUE,
    v_verbose             = FALSE,
    send_email            = FALSE,
    email_address         = paste0(Sys.getenv()[["USER"]], "@uw.edu"),
    dry_runTF             = FALSE
) {

  # Argument validation
  ## coding language
  valid_langs     <- c("r", "python")
  valid_langs_msg <- paste0(valid_langs, collapse = ", ")
  if(is.null(language)) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  language        <- tolower(language)
  if(!language %in% valid_langs) stop("Input a valid language (case insensitive): ", valid_langs_msg)
  ## others
  if(is.null(script_path))     stop("Please define a valid script path to submit")
  if(is.null(account))         stop("Please define a Slurm account e.g. proj_cov_vpd")
  if(is.null(partition))       stop("Please define a Slurm partition e.g. all.q")
  if(is.null(threads))         stop("Please define a number of threads")
  if(is.null(mem))             stop("Please define a memory requirement e.g. '30G' or '300M'")
  if(is.null(runtime_min))     stop("Please define a runtime requirement")
  is_array_job <- ifelse(!is.null(array_tasks_int), TRUE, FALSE)
  if(is_array_job) stopifnot(is.integer(array_tasks_int))
  stopifnot(is.logical(console_style_log_tf))
  stopifnot(is.logical(archiveTF))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(v_verbose))
  stopifnot(is.logical(arg_vecs_to_comma_str))
  stopifnot(is.logical(send_email))
  stopifnot(is.logical(dry_runTF))
  # build log folders silently (dir.create fails naturally if directory exists)
  dir.create(std_err_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(std_out_root, recursive = TRUE, showWarnings = FALSE)
  if(!is.null(hold_for_JobIDs)){
    if(!is.vector(hold_for_JobIDs, mode = "integer")) stop("hold_for_JobIDs must be a simple integer vector")
  }

  # Define commands
  if (is.null(job_name)) {
    script_path_decon <- unlist(strsplit(script_path, "[/.]"))
    job_name          <- script_path_decon[length(script_path_decon) - 1]
  }

  # Code language
  if(language == "r") {

    if(is.null(r_image)) {
      r_image_cmd <- "-i /mnt/share/singularity-images/rstudio/latest.img"
    } else {
      r_image_cmd <- paste0("-i ", r_image)
    }

    if(is.null(shell_script_path)) {
      shell_script_path <- "/mnt/share/singularity-images/rstudio/shells/execRscript.sh"
    }

  } else if (language == "python") {

    if(is.null(shell_script_path)) {
      shell_script_path <- system.file("py/python_shell_slurm.sh", package = "SamsElves")
    }

  }

  ## format for scheduler
  # https://slurm.schedmd.com/sbatch.html#SECTION_FILENAME-PATTERN

  log_format <- ifelse(is_array_job, "%x_%A_%a", "%x_%j")

  if(console_style_log_tf){
    std_err_path <- std_out_path <- file.path(std_out_root, paste0(log_format, "_console.log"))
    # paste0(log_format, "_console.log")
  } else {
    std_err_path <- file.path(std_err_root, paste0(log_format, "e.log"))
    std_out_path <- file.path(std_out_root, paste0(log_format, "o.log"))
    # paste0(log_format, ".log")
  }

  # if(console_style_log_tf){
  #   std_err_path <- std_out_path <- file.path(std_out_root, "%x_%j_console.log")
  # } else {
  #   std_err_path <- file.path(std_err_root, "%x_e%j.log")
  #   std_out_path <- file.path(std_out_root, "%x_o%j.log")
  # }

  archive_cmd  <- ifelse(archiveTF, " -C archive", "")

  # deal with args_list as a block
  if(!is.null(args_list)){
    assert_named_list(args_list)
    # don't break backward compatibility
    names(args_list) <- gsub("^--", "", names(args_list))
    # format for scheduler
    names(args_list) <- paste0("--", names(args_list))
    # auto-convert simple vectors to comma-separated strings
    if(arg_vecs_to_comma_str) args_list <- apply_comma_string_to_list(args_list)
  }

  array_cmd_string <-
    if(is_array_job){
      ## Build array string
      array_tasks_string <-
        paste0(
          " --array=",
          ifelse(
            is_sequential_int_vec(array_tasks_int),
            paste0(min(array_tasks_int), "-", max(array_tasks_int)), # concise if sequential
            vec_to_comma_string(array_tasks_int) # specific integers otherwise
          )
        )
    } else {
      ""
    }

  email_cmd_string <-
    if(send_email){
      paste0(" --mail-type=END --mail-user=", email_address)
    } else {
      ""
    }

  # build system command string
  command <- paste0(
    "sbatch"
    , " -J ",    job_name
    , "",        archive_cmd
    , " --mem=", mem
    , " -c ",    threads
    , " -t ",    runtime_min
    , " -p ",    partition
    , " -A ",    account
    , array_cmd_string
    , email_cmd_string
    , " -e ",    std_err_path
    , " -o ",    std_out_path
    , " "   ,    shell_script_path
    , " "   ,    r_image_cmd
    , " -s ",    script_path
  )

  # add hold_for_JobIDs if exists
  if(!is.null(hold_for_JobIDs)){
    hold_ids <- paste(hold_for_JobIDs, collapse = ":")
    command  <- paste0(command, " --dependency=afterok:", hold_ids)
  }

  # append extra arguments - handles NULL input by default
  for(arg_name in names(args_list)) {
    command <- paste(command, arg_name, args_list[arg_name])
  }

  if(dry_runTF) {
    message(command, "\n")
    return(0L)
  }

  submission_return <- system(command, intern = TRUE)
  job_id <- regmatches(submission_return, gregexpr("\\d+$", submission_return))

  array_message <- ifelse(is_array_job, "array", "")

  if(length(job_id) > 1) warning("job_id from submitted job '",  job_name ,"' is longer than 1, inspect before use.")
  job_id <- as.integer(unlist(job_id))

  if(verbose)   message(paste("\n", submission_return, array_message, " : ", job_name, "\n"))
  if(v_verbose) message("Logs saved to: \n", paste0(unique(c(std_out_path, std_err_path)), collapse = "\n"), "\n")

  return(job_id)

}

# scoping
# language               = "R"
# script_path            = file.path(.bootstrap_root, "module_simmod/run_eppasm_for_location_by_draw_and_scenario.R")
# std_err_root           = file.path(.output_root, "std_err")
# std_out_root           = file.path(.output_root, "std_out")
# job_name               = .job_name
# mem                 = "40G"
# shell_script_path      = NULL
# archiveTF              = T
# use_paths_file_r_image = T
# account                = "proj_hiv"
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
