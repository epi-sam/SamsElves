#' Convert windows file paths
#'
#' If you've copied a file path to the windows clipboard, call this function
#' without any arguments to both return a path with correct slashes, as well as
#' copy that path to the clipboard for an easy paste.
#'
#' @param path [character] a filepath to convert
#'
#' @return correctly formatted filepath (`\` becomes `/`)
#'
#' @export
prep_windows_path_for_unix <- function(path = "clipboard") {
  platform_name       <- tolower(Sys.info()[['sysname']])
  platform_is_windows <- identical("windows", platform_name)
  if(!platform_is_windows) stop("xClipboard functions are only available for the Windows Rstudio API.")
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}
