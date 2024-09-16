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
