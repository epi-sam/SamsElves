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


#' Clean common dirty character encodings
#'
#' Not guaranteed to work in all cases - optimized for common cases
#'
#'
#' @param x [chr] vector of character strings
#'
#' @returns [chr] vector of cleaned character strings
#' @export
#'
clean_encoding <- function(x) {

# What it fixes:
# - Invalid UTF-8 with Latin-1 bytes (C\xf4te → Côte)
# - Double-encoded mojibake (JosÃ© → José)
# - Raw Latin-1 sequences
# - Invalid high codepoints
# - Works with readr::write_excel_csv for Excel compatibility
#    - No quote explosions if using readr::write_excel_csv

  if (!is.character(x)) return(x)
  result <- character(length(x))

  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- NA_character_
      next
    }

    str <- x[i]
    enc <- Encoding(str)

    # For invalid UTF-8, work with raw bytes BEFORE R escapes them
    if (enc == "UTF-8" && !validUTF8(str)) {
      # Get raw bytes directly (before R escapes to <f4>)
      raw_bytes <- charToRaw(str)
      # Reconstruct as character with latin1 encoding
      str <- rawToChar(raw_bytes)
      Encoding(str) <- "latin1"
      # Now convert latin1 to UTF-8
      str <- iconv(str, from = "latin1", to = "UTF-8", mark = FALSE)
    }
    # Valid UTF-8 mojibake handling
    else if (enc == "UTF-8") {
      has_mojibake <- grepl('Ã[©-ÿ]|ï¿½|â€|[À-ÿ]{2,}', str, perl = TRUE)

      if (!has_mojibake) {
        codepoints <- tryCatch(utf8ToInt(str), error = function(e) integer(0))
        has_mojibake <- any(codepoints > 0x10FFFF |
                              (codepoints >= 0xE000 & codepoints <= 0xF8FF) |
                              codepoints >= 0xF0000,
                            na.rm = TRUE)
      }

      if (has_mojibake) {
        fixed <- tryCatch(
          iconv(str, from = "UTF-8", to = "latin1", mark = FALSE),
          error = function(e) NA_character_
        )
        if (!is.na(fixed)) {
          str <- fixed
          Encoding(str) <- "UTF-8"
        }
      }
    }
    # Unknown/latin1
    else if (enc %in% c("unknown", "latin1")) {
      str <- iconv(str, from = "latin1", to = "UTF-8", mark = FALSE)
    }

    result[i] <- str
  }

  Encoding(result) <- "UTF-8"
  return(result)
}


#' Save file I/O with overwrite and message control
#'
#' Defualt behavior is to overwrite
#'
#' @param object [obj] an R object
#' @param f_path [path] a file path to save to
#' @param csv_opt [chr] optional csv writer, depending on desired behavior e.g.
#'   default `readr::write_excel_csv` preserves correct diacritics, but
#'   `data.table::fwrite` is faster if diacritics are not necessary.
#' @param clean_encoding_on [chr] vector of column names to run
#'   `clean_encoding()` on
#' @param overwrite [lgl] default: NULL which overwrites by default until
#'   forbid_overwrite is deprecated
#' @param verbose [lgl] default: silent
#' @param forbid_overwrite [lgl]  DEPRECATED - backward compatibility preserved
#' @param ... other arguments passed to write functions"
#'
#' @return [none] Saves to disk
#' @export
save_file <- function(
    object
    , f_path
    , csv_opt           = "readr::write_excel_csv"
    , clean_encoding_on = NULL
    , overwrite         = NULL
    , verbose           = FALSE
    , forbid_overwrite  = NULL
    , ...
){

  stopifnot(is.logical(verbose))

  # TODO SB - 2024 Dec 05 - deprecate, and change internal logic to use `overwrite`
  # backward compatibility, handle all cases
  # TODO SB - 2025 Dec 09 - time to deprecate
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

      # If cleaning encoding and using fwrite (presumably for reading in
      # Excel), add BOM aka UTF-8 signature for Excel compatibility.
      # - https://en.wikipedia.org/wiki/Byte_order_mark
      if(!is.null(clean_encoding_on) & csv_opt == "data.table::fwrite"){
        args <- append(args, list(bom = TRUE))
      }

      # gracefully handle top level choices for chosen writer
      if('verbose' %in% names(formals(fun))){
        args <- append(args, list(verbose = verbose))
      }

      names(args)[names(args) == "object"] <- arg_names[1]
      names(args)[names(args) == "f_path"] <- arg_names[2]

      do.call(fun, args)

    }

    ext <- tolower(find_file_extension(f_path))

    valid_file_extensions <- paste(
      c("csv", "yaml", "rds", "fst"),
      collapse = ", "
    )

    # Clean character encoding
    if(!is.null(clean_encoding_on) & is.data.frame(object)){

      if(grepl("readr::write_excel_csv|data.table::fwrite", csv_opt)){

        if(verbose)message("--Cleaning character encodings: ", toString(clean_encoding_on))

        for(col in clean_encoding_on){
          if(col %in% colnames(object)){
            object <- add_column(object, col, clean_encoding(object[[col]]))
          } else {
            warning("clean_encoding_on column not found: ", col)
          }
        }
      }
    }

    switch(
      ext,
      "fst"  = function(x, path, compress = 80, ...) {
        # fst are often large, timer is nice to have
        if(verbose) msg_tic()
        fname <- basename(path)
        fst::write_fst(x, path, compress = compress, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- fst write (%s): ", fname))
      },
      "csv"  = function(x, path, ...) {
        fname <- basename(path)
        if(verbose) msg_tic()
        csv_writer(x, path, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- csv write (%s): ", fname))
      },
      # "yaml" = yaml::write_yaml,
      "yaml" = function(x, path, ...) {
        fname <- basename(path)
        if(verbose) msg_tic()
        yaml::write_yaml(x, path, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- yaml write (%s): ", fname))
      },
      # "rds"  = base::saveRDS,
      "rds"  = function(x, path, ...) {
        fname <- basename(path)
        if(verbose) msg_tic()
        base::saveRDS(x, path, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- rds write (%s): ", fname))
      },
      {
        stop(
          glue::glue(
            "Unsupported file extension: '{ext}'. Valid extensions (case-insensitive): {valid_file_extensions}. ",
            "Update if more options are needed."
          )
        )
      }
    )(object, f_path, ...)

    if(verbose) msg_prt(user_msg)

  }
}

#' Read a file of an arbitrary type
#'
#' @param path_to_file [chr] full path with extension
#' @param verbose [lgl] noisy or quiet function?
#' @param csv_opt [chr: default readr::read_csv] name spaced function call for csv reads
#' @param return_DT [lgl: default TRUE] return as.data.table?
#' @param ... [any] additional arguments to pass to the reader function
#'
#' @return [file] an object of appropriate file type
#' @export
read_file <- function(
    path_to_file
    , verbose   = FALSE
    , csv_opt   = "readr::read_csv"
    , return_DT = TRUE
    , ...
){

  if(verbose) message("Reading file: ", path_to_file)

  # format for the switch
  ext <- tools::file_ext(path_to_file)
  suppressWarnings(numeric_chk <- as.numeric(ext))
  if(is.numeric(numeric_chk) & !is.na(numeric_chk)) stop("File extension is numeric, must be character: ", ext)
  ext <- tolower(ext)
  if(!grepl("::", csv_opt)) stop("csv_opt must be a namespaced function call e.g. data.table::fread - instead got ", csv_opt)
  valid_file_extensions <- toString(c("csv", "yaml", "rds", "json", "fst"))

  read_fun <- switch(
    ext,
    "csv"  = function(path, ...) {
      reader <- getFromNamespace(
        x  = strsplit(csv_opt, "::")[[1]][2],
        ns = strsplit(csv_opt, "::")[[1]][1]
      )

      # TODO SB - 2025 Dec 09
      # Don't have time to convert this nicely to the save_file csv_reader
      # handling right now - do this in the future
      if(grepl("readr::read_csv", csv_opt)){

        fname <- basename(path)
        if(verbose) msg_tic()
        .file <- reader(path, show_col_types = FALSE, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- csv read (%s): ", fname))
        if(any(grepl("path_to_top_parent", names(.file)))) {
          # re-read with correct col type
          .file <- reader(
            path
            , show_col_types = FALSE
            , col_types = readr::cols(
              path_to_top_parent = readr::col_character()
            )
            , ...
          )
        }

          # this is all broken
        # browser()
        # withCallingHandlers(
        #   .file <- reader(
        #     path
        #     , show_col_types = FALSE
        #     # common comma-separated int field that readr::read_csv converts to numeric
        #     , col_types = {
        #       tryCatch(
        #         readr::cols(path_to_top_parent = readr::col_character())
        #         , warning = function(w){
        #           readr::cols()
        #         }
        #       )
        #     }
        #     , ...
        #   )
        #   # I don't need to see this if the column isn't present
        #   , warning = function(w){
        #     if(grepl("The following named parsers don't match the column names: path_to_top_parent", w)){
        #       invokeRestart("muffleWarning")
        #     }
        #   }
        # )

      } else {
        fname <- basename(path)
        if(verbose) msg_tic()
        .file <- reader(path, ...)
        if(verbose) msg_toc(prefix = sprintf(" -- csv read (%s): ", fname))
      }
      if(isTRUE(return_DT)) .file <- data.table::as.data.table(.file)
      return(.file)
    },
    "fst"  = function(path, as.data.table = return_DT, ...) {
      # fst are often large, timer is nice to have
      fname <- basename(path)
      if(verbose) msg_tic()
      .file <- fst::read_fst(path, as.data.table = as.data.table, ...)
      if(verbose) msg_toc(prefix = sprintf(" -- fst read (%s): ", fname))
      return(.file)
    },
    # "yaml" = yaml::read_yaml,
    "yaml" = function(path, ...) {
      fname <- basename(path)
      if(verbose) msg_tic()
      .file <- yaml::read_yaml(path, ...)
      if(verbose) msg_toc(prefix = sprintf(" -- yaml read (%s): ", basename(path)))
      return(.file)
    },
    # "rds"  = base::readRDS,
    "rds"  = function(path, ...) {
      fname <- basename(path)
      if(verbose) msg_tic()
      .file <- base::readRDS(path, ...)
      if(verbose) msg_toc(prefix = sprintf(" -- rds read (%s): ", basename(path)))
      return(.file)
    },
    # "json" = jsonlite::fromJSON,
    "json" = function(path, simplifyDataFrame = return_DT, ...) {
      fname <- basename(path)
      if(verbose) msg_tic()
      .file <- jsonlite::fromJSON(path, simplifyDataFrame = simplifyDataFrame, ...)
      if(verbose) msg_toc(prefix = sprintf(" -- json read (%s): ", basename(path)))
      return(.file)
    },
    {
      stop(
        sprintf(
          "Unsupported file extension: '%s'. Valid extensions: %s",
          ext, toString(valid_file_extensions)
        )
      )
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


