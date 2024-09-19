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
find_file_extension <- function(f_path){

  path_deconstructed <- DescTools::SplitPath(f_path) # a list
  if(is.na(path_deconstructed$extension)) stop(glue::glue("This path has no file extension : {path_deconstructed$normpath}"))
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
save_file <- function(object, f_path, forbid_overwrite = TRUE, verbose = FALSE){

  parent_directory <- DescTools::SplitPath(f_path)$dirname
  if(parent_directory == "/") stop("Parent directory is '/', likely undefined.  Inspect f_path")
  if(!dir.exists(parent_directory)) {
    stop(glue::glue("Parent directory does not exist, please create it first :
                    {parent_directory}"))
  }

  flag_file_exists <- file.exists(f_path)

  msg_grid <-
    data.frame(
    expand.grid(
      file_exists_bool = c(TRUE, FALSE),
      forbid_overwrite_bool = c(TRUE, FALSE)
    )
  )

  msg_grid$user_message <- c(
    glue::glue("File already exists, not over-writing: {f_path}"),
    glue::glue("Saved file to disk {f_path}"),
    glue::glue("Overwriting file: {f_path}"),
    glue::glue("Saved file to disk {f_path}")
  )

  user_msg <- msg_grid[msg_grid$file_exists_bool == flag_file_exists &
                         msg_grid$forbid_overwrite_bool == forbid_overwrite, ]$user_message

  if(length(user_msg) > 1) stop("Problem with save msg grid, more than one message selected - inspect")

  if(flag_file_exists & forbid_overwrite){

    msg_prt(user_msg)
    invisible()

  } else {

    valid_file_extensions <- paste(
      c("csv", "yaml", "rds"),
      collapse = ", "
    )

    ext <- tolower(find_file_extension(f_path))

    switch(
      ext,
      "csv"  = {data.table::fwrite(object, f_path)},
      "yaml" = {yaml::write_yaml(object, f_path)},
      "rds"  = {saveRDS(object, f_path)},
      {
        stop(glue::glue("This function only supports {valid_file_extensions} file extensions (case-insensitive)."),
             glue::glue(" Update if more options are needed. Submitted extension: {ext}"))
      }
    )

    if(verbose) msg_prt(user_msg)

  }
}

#' Read a file of an arbitrary type
#'
#' @param path_to_file [chr] full path with extenstion
#' @param verbose [lgl] noisy or quiet function?
#' @param csv_opt [chr] namespaced function call for csv reads (default `"data.table::fread"`)
#' @param ... [any] additional arguments to pass to the reader function
#'
#' @return [file] an object of appropriate file type
#' @export
read_file <- function(path_to_file, verbose = FALSE, csv_opt = "data.table::fread", ...){

  if(verbose) message("Reading file: ", path_to_file)

  # format for the switch
  ext <- tools::file_ext(path_to_file)
  suppressWarnings(numeric_chk <- as.numeric(ext))
  if(is.numeric(numeric_chk) & !is.na(numeric_chk)) stop("File extension is numeric, must be character: ", ext)
  ext <- tolower(ext)
  if(!grepl("::", csv_opt)) stop("csv_opt must be a namespaced function call e.g. data.table::fread - instead got ", csv_opt)
  valid_file_extensions <- toString(c("csv", "yaml", "rds", "json"))

  read_fun <- switch(
    ext,
    "csv"  = getFromNamespace(
      x  = strsplit(csv_opt, "::")[[1]][2],
      ns = strsplit(csv_opt, "::")[[1]][1]
    ),
    "yaml" = yaml::read_yaml,
    "rds"  = readRDS,
    "json" = jsonlite::fromJSON,
    {
      stop(paste0("This function only supports ", valid_file_extensions, " file extensions (case-insensitive)."),
           " Update if more options are needed: ", ext)
    }
  )

  return(read_fun(path_to_file, ...))
}
