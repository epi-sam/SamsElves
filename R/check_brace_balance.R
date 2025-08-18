#' Check Brace Balance in a File
#'
#' @param fpath [chr] Path to the file to check
#'
#' @returns [chr] message
#' @export
#'
#' @examples
#' check_brace_balance("R/check_brace_balance.R")
check_brace_balance <- function(fpath) {
  txt <- paste(readLines(fpath), collapse = "\n")
  chars <- strsplit(txt, "")[[1]]
  stack <- character()
  for (ch in chars) {
    if (ch %in% c("{", "(", "[")) stack <- c(ch, stack)
    if (ch %in% c("}", ")", "]")) {
      if (length(stack) == 0) return("Too many closing brackets")
      opening <- stack[1]
      if (!identical(c(opening, ch), c("{", "}")) &&
          !identical(c(opening, ch), c("(", ")")) &&
          !identical(c(opening, ch), c("[", "]")))
        return("Mismatched bracket pair")
      stack <- stack[-1]
    }
  }
  if (length(stack) > 0) "Unclosed brackets" else "Balanced"
}
