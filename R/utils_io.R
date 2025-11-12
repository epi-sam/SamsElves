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
#' Defualt behavior is to overwrite
#'
#' @param object [obj] an R object
#' @param f_path [path] a file path to save to
#' @param overwrite [lgl] default: NULL which overwrites by default until
#'   forbid_overwrite is deprecated
#' @param verbose [lgl] default: silent
#' @param forbid_overwrite [lgl]  DEPRECATED - backward compatibility preserved
#' @param ... other arguments passed to write functions"
#' @param csv_opt [chr] optional csv writer, depending on desired behavior e.g.
#'   default `readr::write_excel_csv` preserves correct diacritics, but
#'   `data.table::fwrite` is faster if diacritics are not necessary.
#'
#' @return [none] Saves to disk
#' @export
save_file <- function(object, f_path, csv_opt = "readr::write_excel_csv", overwrite = NULL, verbose = FALSE, forbid_overwrite = NULL, ...){

  stopifnot(is.logical(verbose))

  # TODO SB - 2024 Dec 05 - deprecate, and change internal logic to use `overwrite`
  # backward compatibility, handle all cases
  stopifnot(is.null(overwrite)        || is.logical(overwrite))
  stopifnot(is.null(forbid_overwrite) || is.logical(forbid_overwrite))
  if (is.null(forbid_overwrite) & is.null(overwrite)) {
    # assume overwrite - should be base behavior going forward, with overwrite = TRUE
    forbid_overwrite <- FALSE
  } else if(is.logical(forbid_overwrite) & is.logical(overwrite)){
    stop("Both `overwrite` and `forbid_overwrite` are logical, only one should be used")
  } else if (is.logical(forbid_overwrite) & is.null(overwrite)){
    message("\nsave_file: forbid_overwrite will be deprecated, use overwrite instead.  Overwriting is now the default behavior.")
  } else if(is.logical(overwrite) & is.null(forbid_overwrite)){
    forbid_overwrite <- !overwrite
  } else {
    stop("Logic error with overwrite and forbid_overwrite, inspect")
  }

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

    csv_writer <- function(object, f_path, csv_opt_ = csv_opt, ...){

      na <- ""

      write_methods <- list(
        # args must be in order of 1 = object, 2 = file path to write to
        "readr::write_excel_csv"    = list(fun = readr::write_excel_csv, args = c("x", "file"), defaults = list(quote = "needed", na = ""))
        , "readr::write_excel_csv2" = list(fun = readr::write_excel_csv2, args = c("x", "file"), defaults = list(quote = "needed", na = ""))
        , "data.table::fwrite"      = list(fun = data.table::fwrite, args = c("x", "file"), defaults = list(na = ""))
        , "utils::write.csv"        = list(fun = utils::write.csv, args = c("x", "file"), defaults = list(na = ""))
        , "utils::write.csv2"       = list(fun = utils::write.csv2, args = c("x", "file"), defaults = list(na = ""))
      )

      if(!csv_opt_ %in% names(write_methods)) stop("csv_opt must be one of: ", toString(names(write_methods)))

      write_method <- write_methods[[csv_opt_]]
      fun          <- write_method$fun
      arg_names    <- write_method$args
      defaults     <- write_method$defaults

      args <- list(...)
      # apply defaults only if user does not override
      args <- modifyList(defaults, args)
      args$object = object
      args$f_path = f_path

      names(args)[names(args) == "object"] <- arg_names[1]
      names(args)[names(args) == "f_path"] <- arg_names[2]

      do.call(fun, args)

    }

    ext <- tolower(find_file_extension(f_path))

    valid_file_extensions <- paste(
      c("csv", "yaml", "rds", "fst"),
      collapse = ", "
    )

    # 2025 Oct 15 - kludge to deal with J: drive in-place overwrite issues
    # if(file.exists(f_path)){
    #   if (verbose) message("Removing existing file before re-writing to avoid J: drive issues: ", f_path)
    #   file.remove(f_path)
    #   Sys.sleep(1)
    # }
    # switch(
    #   ext,
    #   "fst"  = {fst::write_fst(object, f_path, compress = 80, ...)},
    #   "csv"  = {csv_writer(object, f_path, ...)},
    #   "yaml" = {yaml::write_yaml(object, f_path, ...)},
    #   "rds"  = {base::saveRDS(object, f_path, ...)},
    #   {
    #     stop(glue::glue("This function only supports {valid_file_extensions} file extensions (case-insensitive)."),
    #          glue::glue(" Update if more options are needed. Submitted extension: {ext}"))
    #   }
    # )

    switch(
      ext,
      "fst"  = function(data, path, compress = 80, ...) {
        # fst are often large, timer is nice to have
        msg_tic()
        fst::write_fst(data, path, compress = compress, ...)
        msg_toc(prefix = " -- fst write time: ")
      },
      "csv"  = function(data, path, ...) {
        csv_writer(data, path, ...)
      },
      "yaml" = yaml::write_yaml,
      "rds"  = base::saveRDS,
      {
        stop(
          glue::glue(
            "Unsupported file extension: '{ext}'. Valid extensions (case-insensitive): {valid_file_extensions}. ",
            "Update if more options are needed."
          )
        )
      }
    )(object, f_path, ...)

    if(verbose) msg_prt(user_msg)

  }
}

#' Read a file of an arbitrary type
#'
#' @param path_to_file [chr] full path with extension
#' @param verbose [lgl] noisy or quiet function?
#' @param csv_opt [chr] name spaced function call for csv reads (default `"data.table::fread"`)
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
  valid_file_extensions <- toString(c("csv", "yaml", "rds", "json", "fst"))

  read_fun <- switch(
    ext,
    "csv"  = function(path, ...) {
      reader <- getFromNamespace(
        x  = strsplit(csv_opt, "::")[[1]][2],
        ns = strsplit(csv_opt, "::")[[1]][1]
      )
      reader(path, ...)
    },
    "fst"  = function(path, as.data.table = TRUE, ...) {
      # fst are often large, timer is nice to have
      msg_tic()
      .file <- fst::read_fst(path, as.data.table = as.data.table, ...)
      msg_toc(prefix = " -- fst read time: ")
      return(.file)
    },
    "yaml" = yaml::read_yaml,
    "rds"  = base::readRDS,
    "json" = jsonlite::fromJSON,
    {
      stop(
        sprintf(
          "Unsupported file extension: '%s'. Valid extensions: %s",
          ext, toString(valid_file_extensions)
        )
      )
    }
  )

  return(read_fun(path_to_file, ...))
}

#' If a file exists, get a new path with `v1`, `v2`, etc. appended
#'
#' If file does not exist, return the original path
#'
#' @param outpath [chr] full path to file
#'
#' @return [chr] new path with version number appended (if necessary)
#' @export
#'
#' @examples
#' dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
#' fname_old <- "test_file.csv"
#' file.create(file.path(tempdir(), fname_old))
#' list.files(tempdir()) # [1] "test_file.csv"
#' fname_new <- increment_file_version(file.path(tempdir(), "test_file.csv"))
#' file.create(fname_new)
#' list.files(tempdir()) # 1] "test_file_v1.csv" "test_file.csv"
#' file.remove(c(file.path(tempdir(), fname_old), fname_new))
increment_file_version <- function(outpath){
  if(file.exists(outpath)){

    fname_og    <- basename(outpath)
    dirname_og  <- dirname(outpath)
    fname_base  <- tools::file_path_sans_ext(fname_og)
    fname_ext   <- tools::file_ext(fname_og)
    outpath_new <- file.path(dirname_og, paste0(fname_base, "_v1", ".", fname_ext))
    idx         <- 2

    while(file.exists(outpath_new)){
      fname_new <- paste0(fname_base, "_v", idx, ".", fname_ext)
      outpath_new <- file.path(dirname_og, paste0(fname_new))
      idx <- idx + 1
    }

  } else {
    outpath_new <- outpath
  }

  return(outpath_new)
}


