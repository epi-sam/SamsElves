#' Convenience `dir.create` wrapper with desired defaults
#' 
#' `dir.create()` natively prevents overwrites
#'
#' @param path [path] directory to create
#' @param recursive [lgl] dir.create arg (default = TRUE)
#'
#' @return [lgl] did directory creation succeed or fail?
#' @export
make_directory <- function(path, recursive = TRUE) {
  dir.create(path, recursive = recursive, showWarnings = FALSE)
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

#' Find the file extension from a full path
#'
#' @param f_path [path]
#'
#' @return [chr] a file extension
#' @export
#' @importFrom DescTools SplitPath
#' @importFrom glue glue
find_file_extension <- function(f_path){
  
  path_deconstructed <- DescTools::SplitPath(f_path) # a list
  if(is.na(path_deconstructed$extension)) stop(glue("This path has no file extension : {path_deconstructed$normpath}"))
  return(path_deconstructed$extension)
}


#' Save file I/O with overwrite and message control
#'
#' @param object [obj] any R object
#' @param f_path [path] duh
#' @param forbid_overwrite [lgl] default: overwrite not allowed
#' @param verbose [lgl] default: silent
#'
#' @return [none] Saves to disk
#' @export
#' @importFrom DescTools SplitPath
#' @importFrom glue glue
#' @importFrom data.table data.table fwrite
#' @importFrom yaml write_yaml
save_file <- function(object, f_path, forbid_overwrite = TRUE, verbose = FALSE){

  parent_directory <- DescTools::SplitPath(f_path)$dirname
  if(parent_directory == "/") stop("Parent directory is '/', likely undefined.  Inspect f_path")
  if(!dir.exists(parent_directory)) {
    stop(glue::glue("Parent directory does not exist, please create it first : 
                    {parent_directory}"))
  }
  
  flag_file_exists <- file.exists(f_path)
  
  msg_grid <- data.table(
    expand.grid(
      file_exists_bool = c(TRUE, FALSE),
      forbid_overwrite_bool = c(TRUE, FALSE)
    )
  )
  
  msg_grid$user_message <- c(
    glue("File already exists, not over-writing: {f_path}"),
    glue("Saved file to disk {f_path}"),
    glue("Overwriting file: {f_path}"),
    glue("Saved file to disk {f_path}")
  )
  
  user_msg <-
    msg_grid[file_exists_bool == flag_file_exists &
               forbid_overwrite_bool == forbid_overwrite,]$user_message
  
  if(length(user_msg) > 1) stop("Problem with save msg grid, more than one message selected - inspect")
  
  if(flag_file_exists & forbid_overwrite){
    
    msg_prt(user_msg)
    invisible()
    
  } else {
    
    valid_file_extensions <- paste(
      c("csv", "yaml", "rds"),
      collapse = ", "
    )
    
    ext <- find_file_extension(f_path)
    ext <- tolower(ext)
    
    switch(
      ext,
      "csv"  = {data.table::fwrite(object, f_path)},
      "yaml" = {yaml::write_yaml(object, f_path)},
      "rds"  = {saveRDS(object, f_path)},
      {
        stop(glue("This function only supports {valid_file_extensions} file extensions (case-insensitive)."), 
             glue(" Update if more options are needed. Submitted extension: {ext}"))
      }
    )
    
    if(verbose) msg_prt(user_msg)
    
  }
}

#' Read a file of approved format
#'
#' @param f_path [path]
#' @param verbose [lgl]
#'
#' @return [obj] use this to assign a file to an R object
#' @export
#' @importFrom yaml read_yaml
#' @importFrom data.table fread
#' @importFrom glue glue
read_file <- function(f_path, verbose = FALSE){
  
  valid_file_extensions <- paste(
    c("csv", "yaml", "rds"),
    collapse = ", "
  )
  
  if(!file.exists(f_path)){
    
    warning("File does not exist : ", f_path)
    return(NULL) # warning returns a std_out string, for some reason
    
  } else {
    
    ext <- find_file_extension(f_path)
    if(is.numeric(ext)) stop("File extension is numeric, must be character.")
    ext <- tolower(ext)
    
    if(verbose) msg_prt(paste("Reading file:", f_path))
    
    switch(
      ext,
      "csv"  = {data.table::fread(f_path)},
      "yaml" = {yaml::read_yaml(f_path)},
      "rds"  = {readRDS(f_path)},
      {
        stop(glue("This function only supports {valid_file_extensions} file extensions (case-insensitive)."), 
             " Update if more options are needed.")
      }
    )
  }
}