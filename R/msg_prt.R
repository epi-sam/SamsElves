#' Control how user-defined messages submit to std_err or std_out
#' 
#' Change this function's default output mode depending on how you debug / read logs
#'
#' @param string [chr] string length = 1
#' @param output [chr] output mode: c("message", "print", "both")
#'
#' @return [stderr/stdout] one or both
#' @export
msg_prt <- function(string = "No message supplied", output = "message") {
  
  stopifnot(is.character(string))
  stopifnot(length(string) == 1)
  
  valid_output   <- c("message", "print", "both")
  validation_msg <- paste("Submit a valid output:", paste0(valid_output, collapse = ", "))
  
  stopifnot(validation_msg = output %in% valid_output)
  
  switch(output,
         "message" = {message(string)},
         "print"   = {print(string)},
         "both"    = {message(string); print(string)})
}