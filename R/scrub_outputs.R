#> started: 2024 Jan 08 09:52:26
#> environment of functions to remove output folder results in order to do a clean re-run with the same "date" variable
#> - simulates OOP without the need for a formal class definition - aiming only for encapsulation
#> - `date` is coupled to both inputs and outputs, esp. for `_save_results`
#> - output files must be removed before starting a new run with the same set of inputs
#>
#> Examples:
#> source(file.path(code_root, "vaccination_pipeline_functions/scrub_outputs.R"))
#>
#> Show the `prep_exp` folders that will be removed before actually doing it
#> - same syntax works for `prep_exp` and `save_results`
#> scrub_outputs$prep_exp(run_date = "2023_01_27_SB", gbd_round = "gbd2023", dryrun = TRUE)
#>
#> Complete the deletion (function will prompt user for confirmation)
#> - Remove `prep_exp` to_model folders
#>   - keep or remove admin bias and/or custom stage-1 models (see docstrings for default behavior)
#> scrub_outputs$prep_exp(
#>    run_date                = "2023_01_27_SB",
#>    gbd_round               = "gbd2023",
#>    dryrun                  = FALSE,
#>    keep_custom_stage_1_mod = TRUE/FALSE,
#>    keep_admin_bias_mod     = TRUE/FALSE
#> )
#>
#> Remove all post-processed draws
#> scrub_outputs$save_results("2023_01_27_SB", "gbd2023")
#> - Benchmarks
#> - 2024-09-19 @ 1000 draws - 8m 40.473s directly from CLI (this seems way too long)
#>                           - 39s with tool
#>
#> Remove ST-GPR models
#> - see full reproducible example in function docstrings below
#> - removing models is not currently supported by the ST-GPR api
#>   - create a list of run_ids to remove and submit a Helpdesk ticket



scrub_outputs <- local({


  # Public and private methods
  public  <- new.env()
  private <- new.env()

  private$unlink_with_progress <- function(directory_paths, chunk_size = 100) {

    message("Enumerating files...")
    file_paths <- c(
      list.files(directory_paths, recursive = TRUE, full.names = TRUE),
      # unlink folders last
      directory_paths
    )
    n_files    <- length(file_paths)

    # Initialize the progress bar
    pb <- progress::progress_bar$new(
      format = "  Deleting :n_files files [:bar] :percent in :elapsed - ETA :eta",
      total = n_files,
      width = 80,
      clear = FALSE
    )

    # Process files in chunks to update progress bar
    if(n_files > 0){
      for (i in seq(1, n_files, by = chunk_size)) {
        # Define the current chunk of files
        fpaths_chunk <- file_paths[i:min(i + chunk_size - 1, n_files)]
        # Unlink (delete) the files in the current chunk
        unlink(fpaths_chunk, recursive = TRUE)
        # Update the progress bar
        pb$tick(length(fpaths_chunk), tokens = list(n_files = n_files))
      }
    }
  }


  #' Unlink a directory, signal which are not present
  #'
  #' @param dir_vec [chr] vector of full directory paths to `unlink()`
  #'
  #' @return [none]
  private$unlink_verbose <- function(dir_vec){
    message("Removing these folders: \n  "
            , paste(dir_vec, collapse = "\n  "))

    non_existant_dirs <- dir_vec[!dir.exists(dir_vec)]

    if(length(non_existant_dirs)){
      message("\nWarning: These directories do not exist and will not be removed: \n  ", paste(non_existant_dirs, collapse = "\n  "))
    }

    private$unlink_with_progress(directory_paths = dir_vec, chunk_size = 100)
  }

  #' Remove (`unlink()`) all `_prep_exp` output folders.
  #'
  #' Note: custom stage-1 models and admin bias models are _removed by default_.
  #' - if you want to remove the `to_model` folder but you want to keep these models, set the `keep_` flags to `TRUE`
  #'
  #' @param run_date [chr] vaccines pipeline `date` defined in the config.yaml file
  #' @param gbd_round [chr] e.g. "gbd2023" - part of folder structure
  #' @param require_user_input [lgl] do you want to prompt the user for confirmation before deleting?
  #' @param keep_custom_stage_1_mod [lgl] do you want to remove custom stage 1 models associated with this `run_date`?
  #' @param keep_admin_bias_mod [lgl] do you want to remove admin bias models associated with this `run_date`?
  #' @param dryrun [lgl] if `TRUE`, don't delete folders, return vector of folders to remove
  #' @param keep_config [lgl] (default = TRUE) if `TRUE`, archive the config file
  #'
  #' @return [chr/path] vector of directories to remove
  #' @export
  #'
  #' @examples
  #' scrub_outputs$prep_exp(run_date = "2023_01_08_SB", gbd_round = "gbd2023", dryrun = TRUE)
  public$prep_exp <- function(
    run_date,
    gbd_round               = get("gbd_cycle", envir = .GlobalEnv),
    require_user_input      = TRUE,
    keep_custom_stage_1_mod = FALSE,
    keep_admin_bias_mod     = FALSE,
    # FIXME SB - 2024 Sep 13 - deprecate in the future - moved to `save_config()`
    keep_config             = TRUE,
    dryrun                  = FALSE
  ) {


    # arg validation
    stopifnot(is.character(run_date))
    if(!length(run_date) == 1) stop("Submit one run_date at a time.")
    if(!grepl("^gbd[0-9]{4}$", gbd_round)) stop("`gbd_round` should follow pattern '^gbd[0-9]{4}$'")
    stopifnot(is.logical(require_user_input))
    stopifnot(is.logical(keep_custom_stage_1_mod))
    stopifnot(is.logical(keep_admin_bias_mod))
    stopifnot(is.logical(dryrun))

    DIRS <- list(
      # bulk of pipeline results
      to_model                   = file.path("<j>/data/exp/to_model", gbd_round, run_date)
      # custom stage-1 models for ratio vaccines (inputs for ST-GPR)
      , mrbrt_cs1_cascade        = file.path("<j>/data/exp/mrbrt_coverage_ratio_custom_stage_1_cascade", run_date)
      # models for admin bias correction
      , mrbrt_cascade_admin_bias = file.path("<j>/data/exp/mrbrt_cascade", run_date)
    )

    dir_vec <- unlist(DIRS)

    # Sometimes user may way to remove coverage models from to_model folder, but keep a
    # supplementary model for another aspect of the pipeline
    # - any model in this section can be selected from the config file, and should
    #   not be coupled with the to_model folder
    if(keep_custom_stage_1_mod) dir_vec <- setdiff(dir_vec, DIRS$mrbrt_cs1_cascade)
    if(keep_admin_bias_mod)     dir_vec <- setdiff(dir_vec, DIRS$mrbrt_cascade_admin_bias)

    # message("Will remove these folders:\n  "
    #         , paste(dir_vec, collapse = "\n  "))
    cat("Will remove these folders:\n  "
        , paste(dir_vec, collapse = "\n   ")
        , "\n\n")

    if(dryrun){
      return(dir_vec)
    }

    if(require_user_input){
      user_input_int <- menu(
        choices = c("No", "Yes", "Nope!")
        , title = glue::glue(
          "Do you want to delete all outputs associated with this gbd_round & run_date:

                 `prep_exp` - {gbd_round}/{run_date}
               "
        )
      )

      if(! user_input_int %in% 2L){
        print(paste0("Exiting out."))
        return()
      }
    }

    # archive config
    # FIXME SB - 2024 Sep 13 - deprecate in the future - moved to `save_config()` so that
    # it's saved every time the pipeline runs, prospectively, and the user
    # doesn't need to remember.  Also keeps all configs in one place for
    # easier auditing.
    if(keep_config){
      path_config <- file.path(DIRS$to_model, "config_prep_exp.yaml")
      dir_archive <- file.path(dirname(DIRS$to_model), "archive_configs")
      dir.create(dir_archive, showWarnings = FALSE, recursive = TRUE, mode = "0775")
      path_archive <- file.path(dir_archive, paste0("config_prep_exp_", run_date, ".yaml"))
      file.copy(path_config, path_archive)
    }

    private$unlink_verbose(dir_vec)
    return()
  }




  #' Remove (`unlink()`) all `_save_results` output folders.
  #'
  #' @param run_date [chr] vaccines pipeline `date` defined in the config.yaml file
  #' @param gbd_round [chr] e.g. "gbd2023" - part of folder structure
  #' @param require_user_input [lgl] do you want to prompt the user for confirmation before deleting?
  #' @param dryrun [lgl] if `TRUE`, don't delete folders, return vector of folders to remove
  #' @param keep_config [lgl] (default = TRUE) if `TRUE`, archive the config file
  #'
  #' @return [chr/path] vector of directories to remove
  #' @export
  #'
  #' @examples
  #' scrub_outputs$save_results(run_date = "2023_01_08_SB", gbd_round = "gbd2023", dryrun = TRUE)
  public$save_results <- function(
    run_date,
    gbd_round          = get("gbd_cycle", envir = .GlobalEnv),
    require_user_input = TRUE,
    # FIXME SB - 2024 Sep 13 - deprecate in the future - moved to `save_config()`
    keep_config        = TRUE,
    dryrun             = FALSE
  ){

    # arg validation
    stopifnot(is.character(run_date))
    if(!length(run_date) == 1) stop("Submit one run_date at a time.")
    if(!grepl("^gbd[0-9]{4}$", gbd_round)) stop("`gbd_round` should follow pattern '^gbd[0-9]{4}$'")
    stopifnot(is.logical(require_user_input))
    stopifnot(is.logical(dryrun))

    DIRS <- list(
      modeled = file.path("<j>/data/exp/modeled", gbd_round, run_date)
      , draws = file.path("<mnt>/draws/exp", gbd_round,
                          paste0(run_date, c("", "_covidfree", "_covidimputed")))
    )
    dir_vec <- unlist(DIRS)

    # message("Will remove these folders:\n  "
    #         , paste(dir_vec, collapse = "\n  "))
    cat("Will remove these folders:\n  "
        , paste(dir_vec, collapse = "\n   ")
        , "\n\n")

    if(dryrun){
      return(dir_vec)
    }

    if(require_user_input){
      user_input_int <- menu(
        choices = c("No", "Yes", "Nope!")
        , title = glue::glue(
          "Do you want to delete all outputs associated with this gbd_round & run_date:

                 `save_results` - {gbd_round}/{run_date}
               "
        )
      )

      if(! user_input_int %in% 2L){
        print(paste0("Exiting out."))
        return()
      }
    }

    # archive config
    # FIXME SB - 2024 Sep 13 - deprecate in the future - moved to `save_config()` so that
    # it's saved every time the pipeline runs, prospectively, and the user
    # doesn't need to remember.  Also keeps all configs in one place for
    # easier auditing.
    if(keep_config){
      path_config <- file.path(DIRS$modeled, "config__save_results.yaml")
      dir_archive <- file.path(dirname(DIRS$modeled), "archive_configs")
      dir.create(dir_archive, showWarnings = FALSE, recursive = TRUE, mode = "0775")
      path_archive <- file.path(dir_archive, paste0("config__save_results_", run_date, ".yaml"))
      file.copy(path_config, path_archive)
    }

    private$unlink_verbose(dir_vec)
    return()
  }




  #' Append ST-GPR run ids to a central log of removals.
  #'
  #' Used by `public$stgpr_models()` to keep track of which runs have been removed.
  #'
  #' @param dt_stgpr_to_remove [data.table] of run_ids to remove
  #' @param to_model_root_gbdxx [path] e.g. `get("to_model_root_gbdxx", envir = .GlobalEnv)` from init.r
  #'
  #' @return [none] writes to file
  private$stgpr_append_to_removal_log <- function(
    dt_stgpr_to_remove,
    to_model_root_gbdxx
  ){
    # Append the rows that haven't already been removed, to a central log, for easier tracking
    path_log_stgpr_removed <- file.path(to_model_root_gbdxx, "log_stgpr_removed.csv")

    # If there's already a log of removals...
    if(file.exists(path_log_stgpr_removed)){
      # read it
      dt_stgpr_removed          <- data.table::as.data.table(readr::read_csv(path_log_stgpr_removed, show_col_types = FALSE))
      run_ids_previous          <- unique(dt_stgpr_removed$run_id)
      # subset your removals to only _new_ items
      dt_stgpr_to_remove_new <- dt_stgpr_to_remove[!run_id %in% run_ids_previous]

      if(nrow(dt_stgpr_to_remove_new) == 0) {
        print(paste0("No new ST-GPR runs to append to removal log."))
        return()
      }

      # append those new runs to the central log
      print(paste0("Appending new ST-GPR runs to removal log: ", path_log_stgpr_removed))
      # Using the 'append' arg doesn't allow new columns to be easily added over time, so instead rbind and overwrite
      readr::write_csv(
        x        = rbind(dt_stgpr_removed, dt_stgpr_to_remove_new, fill = TRUE)
        , file   = path_log_stgpr_removed
        , na = ""
      )

    } else {
      # ... otherwise if there's no central log of stgpr removals
      # keep all your marked runs, and make a new log
      print(paste0("Writing new ST-GPR runs to removal log: ", path_log_stgpr_removed))
      # fwrite(dt_stgpr_to_remove, path_log_stgpr_removed)
      readr::write_csv(
        x        = dt_stgpr_to_remove
        , file   = path_log_stgpr_removed
        , na = ""
      )
    }
  }



  #' Write a file of all ST-GPR run ids for some given date_versions.
  #'
  #' Option to write the file, or return for further filtering e.g. only
  #' coverage/stockout models.
  #'
  #' @param date_versions [chr] e.g. "2024_04_24"
  #' @param gbd_round [chr] (default = get("gbd_cycle", envir = .GlobalEnv)) e.g. "gbd2023"
  #' @param write_file [lgl] (default = FALSE) if `TRUE`, don't write the file, just return the data.table
  #' @param append_to_central_log [lgl] (default = TRUE) if `TRUE`, append to a central log of removals
  #' @param output_root [path] (default = "<mnt>", Sys.info()[["user"]], "stgpr_runs")
  #' @param log_name_run_ids [chr] (default = "logs/log_stgpr_run_ids.csv")
  #'
  #' @return [data.table]
  #' @export
  #'
  #' @examples
  #'  dvs <- c(
  #'    "2024_04_24"
  #'    , "2024_05_03"
  #'    , "2024_05_07"
  #'    , "2024_05_09"
  #' )
  #' scrub_outputs$stgpr_models(date_versions = dvs)
  public$stgpr_models <- function(
    date_versions,
    gbd_round             = get("gbd_cycle", envir = .GlobalEnv),
    write_file            = TRUE,
    append_to_central_log = TRUE,
    output_root           = file.path("<mnt>", Sys.info()[["user"]], "stgpr_runs"),
    log_name_run_ids      = "logs/log_stgpr_run_ids.csv"
  ) {

    # validate inputs
    stopifnot(length(gbd_round) == 1)
    if(!grepl("^gbd[0-9]{4}$", gbd_round)) stop("`gbd_round` should follow pattern '^gbd[0-9]{4}$'")
    stopifnot(length(output_root) == 1)
    stopifnot(length(log_name_run_ids) == 1)

    # using the init process would be better for consistent paths, but that requires defining the code root, etc.
    # keeping things consistent within the scrub_outputs function-set for now
    to_model_root_gbdxx <- file.path("<j>/data/exp/to_model", gbd_round)
    stopifnot(dir.exists(to_model_root_gbdxx))

    # Build paths
    dir.create(output_root, showWarnings = FALSE, recursive = TRUE, mode = "0775")
    log_paths      <- file.path(to_model_root_gbdxx, date_versions, log_name_run_ids)
    logs_found     <- log_paths[file.exists(log_paths)]
    logs_not_found <- setdiff(log_paths, logs_found)
    if(length(logs_not_found)) warning("No logs found for:\n", paste(logs_not_found, collapse = "\n"))
    logs           <- data.table::fread(logs_found, fill = TRUE)

    if(write_file) {
      output_fname <- file.path(output_root, paste0(format(Sys.time(), "%Y_%m_%d_%H%M%OS5_"), "run_ids_to_remove.csv"))
      print(paste0("Table to append to IT ticket written to ", output_fname))
      # remove columsn that could confuse the IT ticket
      keep_vars <- setdiff(names(logs), c("is_best"))
      data.table::fwrite(logs[, ..keep_vars], output_fname)
    }

    # append to central removal log
    if(append_to_central_log) private$stgpr_append_to_removal_log(dt_stgpr_to_remove = logs, to_model_root_gbdxx = to_model_root_gbdxx)

    return(logs)
  }



  # Return only public functions from the scrub_outputs environment
  return(public)

})
