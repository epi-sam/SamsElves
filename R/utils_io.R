#' SDG core function - Convenience `dir.create` wrapper with desired defaults
#' 
#' `dir.create()` natively prevents overwrites
#'
#' @param path [path] directory to create
#' @param RECURSIVE [lgl] dir.create arg (default = TRUE)
#'
#' @return [lgl] did directory creation succeed or fail?
#' @export
make_directory <- function(path, RECURSIVE = TRUE) {
  dir.create(path, recursive = RECURSIVE, showWarnings = FALSE)
}

#' Current Date-Time Stamp
#' 
#' Wrapper for `Sys.time()`.  Results are formatted as YYYY_MM_DD_hhmmssTZONE.
#' Allows user to print to stderr, stdout, and invisible returns the stamp.
#'
#' @param std_out [sdt_out] using `print()` 
#' @param std_err [std_err] message class
#'
#' @return [invisible] date-time stamp
#' @export
#'
datetime_stamp <- function(std_out = TRUE, std_err = FALSE, dt_format = "%Y_%m_%d_%H%M%S%Z"){
  dt_stamp <- format(Sys.time(), format = dt_format)
  if(std_out) print(dt_stamp)
  if(std_err) message(dt_stamp)
  invisible(dt_stamp)
}


#' SDG core function - save a file of approved format, with overwrite toggle
#'
#' @param object [obj] any R object
#' @param path_to_file [path] duh
#' @param forbid_overwrite [lgl] default: overwrite not allowed
#' @param inform_user [lgl] 
#'
#' @return [none] Saves to disk
#' @export
#' @importFrom DescTools SplitPath
#' @importFrom glue glue
#' @importFrom data.table data.table fwrite
#' @importFrom yaml write_yaml
save_file <- function(object, path_to_file, forbid_overwrite = TRUE, inform_user = FALSE){

  parent_directory <- DescTools::SplitPath(path_to_file)$dirname
  if(parent_directory == "/") stop("Parent directory is '/', likely undefined.  Inspect path_to_file")
  if(!dir.exists(parent_directory)) {
    stop(glue::glue("Parent directory does not exist, please create it first : 
                    {parent_directory}"))
  }
  
  flag_file_exists <- file.exists(path_to_file)
  
  msg_grid <- data.table(
    expand.grid(
      file_exists_bool = c(TRUE, FALSE),
      forbid_overwrite_bool = c(TRUE, FALSE)
    )
  )
  
  msg_grid$user_message <- c(
    glue("File already exists, not over-writing : {path_to_file}"),
    glue("Saved file to disk {path_to_file}"),
    glue("Overwriting file: {path_to_file}"),
    glue("Saved file to disk {path_to_file}")
  )
  
  user_msg <-
    msg_grid[file_exists_bool == flag_file_exists &
               forbid_overwrite_bool == forbid_overwrite,]$user_message
  
  if(length(user_msg) > 1) stop("Problem with save msg grid, more than one message selected - inspect")
  
  if(flag_file_exists & forbid_overwrite){
    
    msg_prt(user_msg)
    
  } else {
    
    valid_file_extensions <- paste(
      c("csv", "yaml", "rds"),
      collapse = ", "
    )
    
    ext <- find_file_extension(path_to_file)
    ext <- tolower(ext)
    
    switch(
      ext,
      "csv"  = {fwrite(object, path_to_file)},
      "yaml" = {yaml::write_yaml(object, path_to_file)},
      "rds"  = {saveRDS(object, path_to_file)},
      {
        stop(glue("This function only supports {valid_file_extensions} file extensions (case-insensitive)."), 
             glue(" Update if more options are needed. Submitted extension: {ext}"))
      }
    )
    
    if(inform_user) msg_prt(user_msg)
    
  }
}