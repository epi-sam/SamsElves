#' Paste data.frame output for a message
#'
#' @param x [list] list or data.frame to print
#'
#' @return [chr] data.frame output as a single string
#' @export
#'
#' @examples
#' prt_multiline(head(mtcars, 2))
#' [1] "              mpg cyl disp  hp drat    wt  qsec vs am gear carb\nMazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4\nMazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4"
prt_multiline <- function(x){
  paste(capture.output(x), collapse = "\n")
}


#' Print a message with multiple lines
#'
#' @param x [list] list or data.frame to print
#'
#' @return [none] message output
#' @export
#'
#' @examples
#' msg_multiline(head(mtcars, 2))
#'                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
#' Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#' Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
msg_multiline <- function(x){
  message(prt_multiline(x))
}

#' Start tictoc timer for stderr messaging
#'
#' Just a wrapper of tictoc::tic() for fun name consistency
#'
#' @param msg [chr: default NULL] passsed to tictoc::tic()
#' @param quiet [lgl: default FALSE] passed to tictoc::tic()
#' @param func.tic [function: default NULL] passed to tictoc::tic()
#' @param ... additional arguments passed to tictoc::tic()
#'
#' @returns [none]
#' @export
#'
#' @family timers
#'
#' @examples
#' msg_tic()
#' msg_toc()
msg_tic <- function(msg = NULL, quiet = FALSE, func.tic = NULL, ...){
  tictoc::tic(msg = msg, quiet = quiet, func.tic = func.tic, ...)
}

#' Message tictoc toc time with prefix for stderr messaging
#'
#' @param prefix [chr: default " -- "] prefix to the message
#'
#' @family timers
#'
#' @returns [none]
#' @export
#'
#' @examples
#' msg_tic()
#' msg_toc()
msg_toc <- function(prefix = "-- "){
  checkmate::assert_character(prefix, len = 1)
  message(
    prefix
    , sub(
      " elapsed$"
      , ""
      , tictoc::toc(quiet = TRUE)$callback_msg[1]
    )
  )
}


#' StdErr section header with nice spacing
#'
#' @param ... [chr] passed to message()
#' @param section [chr] a section header before the message
#' @param newlines [int: default TRUE] number of newlines to add before and
#'   after message
#'
#' @returns [none] side effect - displays to std_err
#' @export
#'
#' @examples msg_section("/my/file/path", section = "my path")
msg_section <- function(..., section = "", newlines = 2L){
  if(nchar(section) > 0) section <- sprintf("%s: ", section)
  newlines <- as.integer(newlines) # backward compability when newlines was T/F
  checkmate::assert_integerish(newlines, len = 1, lower = 0)
  newline_chars <- paste0(rep("\n", newlines), collapse = "")
  message(
    newline_chars
    , "---- "
    , section
    , ...
    , " ----"
    , newline_chars
  )
}

#' Current Date-Time Stamp
#'
#' Wrapper for `Sys.time()`.  Results are formatted as YYYY_MM_DD_hhmmssTZONE.
#' Allows user to print to stderr and/or stdout, and invisibly returns the stamp.
#'
#' @param std_out [std_out: default FALSE] using `print()`
#' @param std_err [std_err: default TRUE] message class
#' @param dt_format [chr] datetime format string default "\%Y_\%m_\%d_\%H\%M\%S\%Z"
#'
#' @return [invisible] date-time stamp
#' @export
#'
#' @examples
#' datetime_stamp(std_out = TRUE)
#'
datetime_stamp <- function(std_out = FALSE, std_err = FALSE, dt_format = "%Y_%m_%d_%H%M%S%Z"){
  checkmate::assert_logical(std_out, len = 1)
  checkmate::assert_logical(std_err, len = 1)

  dt_stamp <- format(Sys.time(), format = dt_format)
  if(std_out) print(dt_stamp)
  if(std_err) message(dt_stamp)
  invisible(dt_stamp)
}


#' StdErr timestamp with nice formatting
#'
#'
#' @param dt_format [chr] datetime format string : default "\%Y_\%m_\%d \%H:\%M:\%S \%Z"
#' @param newlines [int: default 2L] number of newlines to add before and
#'  after message
#'
#' @returns [none] side effect - displays to std_err
#' @export
#'
#' @examples
#' msg_tstamp()
#'
msg_tstamp <- function(dt_format = "%Y_%m_%d %H:%M:%S %Z", newlines = 2L){
  ts <- datetime_stamp(dt_format = dt_format)
  msg_section(ts, newlines = newlines)
}
