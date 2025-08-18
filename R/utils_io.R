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
      c("csv", "yaml", "rds"),
      collapse = ", "
    )

    switch(
      ext,
      "csv"  = {csv_writer(object, f_path, ...)},
      "yaml" = {yaml::write_yaml(object, f_path, ...)},
      "rds"  = {saveRDS(object, f_path, ...)},
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


#' get the latest index for given an output dir and a date
#'
#' directories are assumed to be named in YYYY_MM_DD.VV format with sane
#' year/month/date/version values.
#'
#' @param dir [chr] path to directory with versioned dirs
#' @param date [chr] character in be YYYY_MM_DD format
#' @param suffix [chr] a suffix to append after the incremented version number index
#'
#' @return [dbl] largest version in directory tree or 0 if there are no version OR
#' the directory tree does not exist
#' @export
#'
#' @examples
#' get_latest_output_date_index("tests/testthat/fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09") # expect 2
get_latest_output_date_index <- function(dir, date, suffix = "") {

  dir <- normalizePath(dir, mustWork = TRUE)
  checkmate::assert_character(date, len = 1)
  checkmate::assert_atomic(suffix, len = 1)

  currentfolders <- list.files(dir)

  # subset to date
  pat <- sprintf("^%s[.]\\d{2}%s$", date, suffix)
  date_dirs <- grep(pat, currentfolders, value = TRUE)

  if (length(date_dirs) == 0) {
    return(0)
  }

  # get the index after day
  date_list <- strsplit(sub(sprintf("%s$", suffix), "", date_dirs), "[.]")

  inds <- unlist(lapply(date_list, function(x) x[2]))
  if (is.na(max(inds, na.rm = T))) inds <- 0

  return(max(as.numeric(inds)))
}


#' Find the latest output directory with format YYYY_MM_DD.VV
#'
#' @param root [chr] path to root of output results
#' @param suffix [chr] a suffix to append after the incremented version number index
#'
#' @return [chr] path to latest output directory
#' @export
#'
#' @examples
#' get_latest_output_dir("tests/testthat/fixtures/versioned-dirs/nested/1999_09_09") # expect "tests/testthat/fixtures/versioned-dirs/nested/1999_09_09/1999_09_09.02"
get_latest_output_dir <- function(root, suffix = "") {

  checkmate::assert_directory_exists(root)
  checkmate::assert_atomic(suffix, len = 1)

  raw <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  valid.idx <- grep(sprintf("^\\d{4}_\\d{2}_\\d{2}[.]\\d{2}%s$", suffix), raw)
  if (length(valid.idx) == 0) {
    stop(sprintf("No YYYY_MM_DD.VV<suffix> directories in %s", root))
  }
  return(file.path(root, max(raw[valid.idx])))
}


#' Increment a new output folder date-version
#'
#' Return on the date-version, not the full path.  Does not create a folder.
#'
#' @param root [chr] path to root of output results
#' @param date [chr] character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
#' @param suffix [chr] a suffix to append after the incremented version number index
#'
#' @return [dbl] new output version of the form "YYYY_MM_DD.VV"
#' @export
#'
#' @examples
#' get_new_output_dv(root = tempdir(), date = "today")
get_new_output_dv <- function(root, date = "today", suffix = ""){

  checkmate::assert_directory_exists(root)
  checkmate::assert_character(date, len = 1)
  checkmate::assert_atomic(suffix, len = 1)

  if (date == "today") {
    date <- format(Sys.Date(), "%Y_%m_%d")
  }

  cur.version <- get_latest_output_date_index(dir = root, date = date, suffix = suffix)
  dir.name <- sprintf("%s.%02i%s", date, cur.version + 1, suffix)

  while (file.exists(file.path(root, dir.name))) {
    .dtd <- unlist(strsplit(dir.name, '\\.'))[1]
    .dtv <- as.integer(sub(sprintf("%s$", suffix), "", unlist(strsplit(dir.name, '\\.'))[2]))
    dir.name <- sprintf("%s.%02i%s", .dtd, .dtv + 1, suffix)
  }

  return(dir.name)
}


#' Increment a new output folder date-version
#'
#' Get a new directory path, but don't make it
#'
#' @param root [chr] path to root of output results
#' @param date [chr] character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
#' @param suffix [chr] a suffix to append after the incremented version number index
#'
#' @return [chr] path to new output directory of the form "/<root>/YYYY_MM_DD.VV"
#' @export
#'
#' @examples
#' get_new_output_dir(root = tempdir(), date = "today")
get_new_output_dir <- function(root, date, suffix = ""){
  return(file.path(root, get_new_output_dv(root = root, date = date, suffix = suffix)))
}


#' Make a date-versioned output directory for output results.
#'
#' Returns an appropriate path to save results in, creating it if necessary.
#'
#' @param root [chr] path to root of output results
#' @param date [chr] character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
#' @param suffix [chr] a suffix to append after the incremented version number index
#'
#' @return [chr] path to new output directory of the form "/<root>/YYYY_MM_DD.VV"
#' @export
#'
#' @examples
#' \dontrun{
#' make_new_output_dir(tempdir(), date = "today")
#' }
make_new_output_dir <- function(root, date = "today", suffix = "") {
  dir.path <- get_new_output_dir(root = root, date = date, suffix = suffix)
  if (!dir.exists(dir.path)) {
    # handle quirk with singularity image default umask
    old.umask <- Sys.umask()
    on.exit(Sys.umask(old.umask))
    Sys.umask("002")
    dir.create(dir.path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  }
  return(dir.path)
}
