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
