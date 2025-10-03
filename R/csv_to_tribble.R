#' Convert CSV to R tribble
#'
#' Reads a CSV file and writes an R script that defines the same table as a
#' tibble using `tibble::tribble()`.
#'
#' @param csv_input_path [chr] path to input CSV file
#' @param output_R_path [chr] path to output R file
#' @param obj_name [chr] name of the R object defined in the output file
#' @param align_commas [lgl] whether to align commas in the tribble for better
#'   readability
#' @param make_backup [lgl] whether to make a backup of the original file before
#'   styling (`styler::tidyverse_style`) and aligning commas
#'
#' @returns [invisible tibble] the data read from the CSV
#' @export
#'
csv_to_tribble <- function(
    csv_input_path
    , output_R_path
    , obj_name
    , align_commas = TRUE
    , make_backup  = FALSE
) {

  # Assertions
  checkmate::assert_file_exists(csv_input_path)
  checkmate::assert_character(output_R_path, len = 1)
  checkmate::assert_character(obj_name, len = 1)
  checkmate::assert_logical(align_commas, len = 1)
  checkmate::assert_logical(make_backup, len = 1)

  # Read in - data.table handles types better by default
  DT <- data.table::fread(csv_input_path)

  # Build header
  header <- paste0("~", names(DT), collapse = ", ")

  # Build rows

  # v3
  rows <- apply(DT, 1, function(row) {
    vals <- vapply(seq_along(row), function(i) {
      x <- row[[i]]

      # Get the original column class
      col_class <- class(DT[[i]])[1]

      if (is.na(x)) {
        "NA"
      } else if (col_class %in% c("character", "factor")) {
        paste0('"', x, '"')          # quote strings/factors
      } else if (col_class %in% c("integer", "numeric")) {
        as.character(x)              # leave numeric unquoted
      } else if (col_class == "logical") {
        if (x) "TRUE" else "FALSE"
      } else {
        paste0('"', as.character(x), '"')  # fallback
      }
    }, FUN.VALUE = character(1))

    paste(vals, collapse = ", ")
  })

  tribble_text <- c(
    sprintf("%s <- tibble::tribble(", obj_name),
    paste0("  ", header, ","),
    paste0("  ", paste(rows, collapse = ",\n  ")),
    ")"
  )

  writeLines(tribble_text, output_R_path)
  message("Wrote tribble code to: ", output_R_path)

  if (align_commas) {
    checkmate::assert_file(output_R_path, extension = "R")
    styler::style_file(
      path = output_R_path,
      style = styler::tidyverse_style,
      scope = "line_breaks"
    )
    align_commas_in_tribble(output_R_path, make_backup = make_backup)
  }

  # Return the tibble invisibly
  return(invisible(DT))
}


#' Align a tribble defined in an R file by all the commas
#'
#' Visually creates fixed-width text columns
#'
#' @param path [chr] path to R file containing a single defined
#'   `tibble::tribble()`
#' @param make_backup [lgl] whether to make a backup of the original file before
#'   styling aligning commas
#'
#' @returns [invisible character vector] the lines of the file after alignment
#' @export
#'
align_commas_in_tribble <- function(path, make_backup) {

  # Assertions
  checkmate::assert_file_exists(path)
  checkmate::assert_file(path, extension = "R")
  checkmate::assert_logical(make_backup, len = 1)

  # Read file as lines
  lines <- readLines(path)

  # Only process lines that look like rows of a tribble (start with ~ or digits/quotes/etc.)
  row_idx <- grep("^\\s*[~'\"]|^\\s*[-0-9]", lines)
  if (length(row_idx) == 0) return(invisible(NULL))

  rows <- lines[row_idx]

  # Helper: split on commas not inside quotes
  split_commas <- function(x) {
    # Find commas that are not inside quotes
    chars <- strsplit(x, "")[[1]]
    in_quote <- FALSE
    parts <- list()
    current <- ""
    for (ch in chars) {
      if (ch == "\"" || ch == "'") {
        in_quote <- !in_quote
        current <- paste0(current, ch)
      } else if (ch == "," && !in_quote) {
        parts <- append(parts, list(trimws(current)))
        current <- ""
      } else {
        current <- paste0(current, ch)
      }
    }
    parts <- append(parts, list(trimws(current)))
    unlist(parts)
  }

  split_rows <- lapply(rows, split_commas)

  # Find max width for each column
  ncols <- max(lengths(split_rows))
  col_widths <- integer(ncols)
  for (col in seq_len(ncols)) {
    col_widths[col] <- max(nchar(vapply(split_rows, function(r) if (col <= length(r)) r[col] else "", character(1))))
  }

  # Pad each row
  aligned_rows <- vapply(split_rows, function(r) {
    parts <- vapply(seq_len(ncols), function(col) {
      if (col <= length(r)) {
        sprintf(paste0("%-", col_widths[col], "s"), r[col])
      } else {
        sprintf(paste0("%-", col_widths[col], "s"), "")
      }
    }, character(1))
    paste(parts, collapse = ", ")
  }, character(1))

  # Replace lines in original
  lines[row_idx] <- aligned_rows

  if (make_backup) {
    # Make backup
    message("Backing up original file to: ", paste0(path, ".bak"))
    file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  }

  # Write back
  writeLines(lines, path)

  return(invisible(lines))
}

