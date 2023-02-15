#' Send a user-message to both stdout and stderr
#' 
#' When using `sink()` on a pipeline, some context may be more useful in stdout
#' or stderr, but it's difficult to know when.  This wrapper prints a message to
#' both locations.  
#'
#' @param string [character] message to the pipeline's user. Require: length = 1
#'
#' @return [stdout/stderr] user message to both stdout and stderr
#' 
#' @export
msg_prt <- function(string = "No message supplied") {
  stopifnot(is.character(string))
  stopifnot(length(string) == 1)
  print(string)
  message(string)
}