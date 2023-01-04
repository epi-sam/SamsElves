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
prep_path <- function(path = "clipboard") {
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
