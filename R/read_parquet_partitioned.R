# started: 2025 Aug 06 18:54:46
# purpose: stgpr's api has changed, and final raked draws now stored as parquet instead of csv

#' Read a partitioned parquet file structured as a directory with manifest file
#'
#' stgpr's api has changed, and final raked draws now stored as parquet instead
#' of csv
#'
#' @param dir [chr] Directory containing partitioned parquet files e.g.
#'   /path/to/stgpr/outputs/mv_20250101_words_words_words
#' @param fnames_ignored [chr: default c("manifest", "partition")] Vector of file names to ignore (e.g.,
#'   manifest/partition files).
#' @param verbose [lgl: default TRUE] Print progress messages?
#'
#' @returns [data.table] Combined data from all parquet files in the directory.
#' @export
read_parquet_partitioned <- function(
    dir
    , fnames_ignored = c("manifest", "partition")
    , verbose = TRUE
) {

  # assertions
  checkmate::assert_character(dir, len = 1)
  checkmate::assert_directory_exists(dir)
  checkmate::assert_character(fnames_ignored)

  parquet_files <- list.files(
    dir,
    pattern    = "\\.parquet$",
    recursive  = TRUE,
    full.names = TRUE
  )

  # the manifest is metadata, can't be read and combined with data
  rgx_ignore    <- paste(fnames_ignored, collapse = "|")
  parquet_files <- parquet_files[!grepl(rgx_ignore, parquet_files)]
  if(length(parquet_files) == 0) stop("No parquet files in: ", dir)

  # Function to extract partition metadata from file path
  extract_partition_metadata <- function(file_path, base_dir) {
    # Get relative path from base directory
    rel_path <- gsub(paste0("^", normalizePath(base_dir), "/"), "", normalizePath(file_path))

    # Split path into components and extract partition info
    path_parts <- unlist(strsplit(rel_path, "/"))

    # Find parts that match partition pattern (key=value)
    partition_parts <- path_parts[grepl("^[^=]+=[^=]+$", path_parts)]

    if(length(partition_parts) == 0) {
      return(data.table::data.table())
    }

    # Parse partition key-value pairs
    partition_list <- list()
    for(part in partition_parts) {
      kv <- unlist(strsplit(part, "=", fixed = TRUE))
      if(length(kv) == 2) {
        key <- kv[1]
        value <- kv[2]

        # Try to convert to numeric if possible, otherwise keep as character
        numeric_value <- suppressWarnings(as.numeric(value))
        if(!is.na(numeric_value)) {
          partition_list[[key]] <- numeric_value
        } else {
          partition_list[[key]] <- value
        }
      }
    }

    return(data.table::as.data.table(partition_list))
  }

  # Read files and attach metadata
  if(verbose == TRUE) message(sprintf("Reading %s parquet files with partition metadata: %s", length(parquet_files), dir))

  data_list <- purrr::map(parquet_files, function(file_path) {
    # Read the parquet file
    dt <- data.table::as.data.table(arrow::read_parquet(file_path, as_data_frame = TRUE))

    # Extract partition metadata from folder structure
    metadata <- extract_partition_metadata(file_path, dir)

    # If metadata exists, add it to every row
    if(nrow(metadata) > 0 && ncol(metadata) > 0) {
      # Replicate metadata for all rows in the data
      for(col_name in names(metadata)) {
        dt[, (col_name) := metadata[[col_name]]]
      }
    }

    return(dt)
  })

  # Combine all data tables
  DT <- data.table::rbindlist(data_list, use.names = TRUE, fill = TRUE)

  # Remove pandas indexing column if present
  if("__index_level_0__" %in% names(DT)) DT[, `__index_level_0__` := NULL]

  return(DT)
}
