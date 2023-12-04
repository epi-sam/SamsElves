#' Validation of hierarchies or data for equivalence or missingness
#'
#' Returns a \code{list()} of diagnostic information, quietly or verbose, and
#' STOPS or continues a script after a validation check, based on user
#' preference.  You can save this output list to an object for investigation.
#' \itemize{
#'   \item{\code{'X'} is the left-side,'reference' data - your gold standard to compare against.}
#'   \item{\code{'Y'} is the right-side 'comparison' data to validate against \code{X}.}
#'   \item{Methods with \code{'__vec'} expect a vector, \code{'hier2__'} expect a hierarchy, \code{'__data'} expect a \code{data.frame} with \code{colsX} defined.}
#'   \item{\code{__NA/Inf/0__ } methods only require \code{X / colsX}}
#'   \item{\code{__2__, col_names}, and \code{all_equal} check methods require \code{X} and \code{Y}.}
#'   }
#'
#' This function may STOP a script if specified. Many 'methods' of comparison
#' are available - check function arguments. Suppose your pipeline depends on
#' having data for all locations in a given hierarchy. Suppose you need to
#' compare two hierarchies directly for equality. If you need to compare
#' data.frame columns with the same content but different column names, enter
#' those names in the same order in \code{colsX} and \code{colsY} arguments, and
#' the function will use \code{colsX} (left-hand data.frame) to rename all
#' columns, and output will return those names. Order of
#' operations: \code{select()}, \code{setNames()}.
#'
#' @param X Left-hand input vector or data.frame (gold standard to compare
#'   against)
#' @param Y Right-hand input vector or data.frame (compared AGAINST your gold
#'   standard, by 'method')
#' @param method method of comparison to be made:
##' \itemize{
##'  \item{\code{"vec2vec"}   : X to Y - compare two vectors for UNIQUE values. Stop_condition = \code{length(setdiff(X,Y)) > 0}}
##'  \item{\code{"hier2data"} : X to Y - compare a data from on the right against a gold-standard hierarchy on the left. Stop_condition = \code{length(setdiff(X$location_id, Y$location_id)) > 0}}
##'  \item{\code{"hier2hier"} : X to Y - compare two hierarchies for equivalence across \code{c("location_id", "location_name", "path_to_top_parent", "most_detailed")}. Stop_condition = any differences.}
##'  \item{\code{"data2data"} : X to Y - compare two generic \code{data.frames} and checks for common UNIQUE values in columns between data.frames. Stop_condition = \code{nrow(setdiff(distinct(X), distinct(Y))) > 0}.}
##'  \item{\code{"col_names"} : X to Y - compare \code{data.frames} for all same column names. Stop_condition = any differences.}
##'  \item{\code{"all_equal"} : X to Y - compare two vectors or \code{data.frames} for total equality.  WARNING: no selecting.  Stop_condition = \code{all.equal} is not TRUE.}
##'  \item{\code{"anyNAvec"}  : X only - evaluate a vector for any \code{NA or Inf}.  Stop_condition = \code{anyNA(X) | any(is.infinite(X))}}
##'  \item{\code{"anyNAdata"} : X only - evaluate a data.frame for any \code{NA or Inf}.  Stop_condition = any NA or Inf in any column in \code{colsX}}
##'  \item{\code{"allNAvec"}  : X only - evaluate a vector for all \code{NA or Inf}.  Stop_condition = \code{all(is.na(X)) | all(is.infinite(X))}}
##'  \item{\code{"allNAdata"} : X only - evaluate a data.frame for all \code{NA or Inf} in any column.  Stop_condition = all NA or Inf in any column in \code{colsX}}
##'  \item{\code{"all0vec"}   : X only - evaluate a vector for all \code{0} values.  Stop_condition = \code{all(X==0)}}
##'  \item{\code{"all0data"}  : X only - evaluate a data.frame for all \code{0} values in any column.  Stop_condition = all \code{0} in any column in \code{colsX}}
##' }
#' @param verbose Do you want verbose console output, or invisible() return of
#'   the output_list for assignment to a variable?
#' @param colsX Only for '___data' methods. Vector of column names in
#'   "reference" vector or data.frame - keep column order consistent!
#' @param colsY Only for '___2data' methods. Vector of column names in the
#'   "comparison" vector or data.frame - WARNING: MAKE SURE column order matches
#'   that of \code{colsX}
#' @param STOP Do you want to STOP your script if there is a mismatch, or allow
#'   to continue with a WARNING message?
#' @param user_message Custom message if verbose = TRUE - locate your preflight_check stop location
#'
#' @return Output_list of diagnostics that may be assigned, or printed to console (\code{verbose = TRUE})
#'
#' @examples
#'
#'# MREs -----------------
#'
#'library(dplyr)
#'
#'# HIERARCHIES
#'
#'
#'# DATA --------------------
#'
#'# full_data
#'
#' # METHODS -------------------------
#'
#' ## vec2vec ----------------------
#'
#' locs_771 <- hier_covid_771$location_id
#' locs_1020 <- hier_covid_1020$location_id
#' locs_mi <- hier_covid_mi$location_id
#'
#' preflight_checks(locs_771, locs_mi, "vec2vec", v=T) # fails with output
#' preflight_checks(locs_1020, locs_mi, "vec2vec", v=T) # passes with output
#'
#' ## NA/Inf/0 validation ----------------------
#'
#' A <- c(1:3, NaN, 4, rep(NA,3))
#' B <- c(5:6, -Inf, 7, rep(Inf,4))
#' C <- rep(NA, 8)
#' D <- rep(Inf, 8)
#' E <- rep(0,8)
#' DF <- data.frame(A, B, C, D, E)
#'
#' preflight_checks(A, method = "anyNAvec", v=T) # fails
#' preflight_checks(A, method = "allNAvec", v=T) # passes
#' preflight_checks(D, method = "allNAvec", v=T) # fails
#'
#' preflight_checks(DF, colsX = names(DF), method = "all0data", v=T) # fails
#'
#' ## hier2hier -------------------
#' # compares across c("location_id", "location_name", "path_to_top_parent", "most_detailed")
#' # save your output for inspection if not verbose
#' (pfc_pass <- preflight_checks(hier_covid_1020, hier_covid_1020, "hier2hier"))
#' (pfc_fail <- preflight_checks(hier_covid_1020, hier_covid_771, "hier2hier"))
#'
#' ## data2data -----------------
#' preflight_checks(full_data, full_data, "data2data")
#' preflight_checks(full_data, full_formatted, "data2data") # why does this pass? (OK)
#' preflight_checks(full_data, full_formatted, "data2data", colsX = c("location_id", "Date_formatted")) # pass (OK)
#'
#' ## col_names -----------------
#' preflight_checks(hier_covid_1020, hier_covid_771, "col_names")
#' preflight_checks(full_data, full_formatted, "col_names")
#'
#'## all_equal ---------------
#'
#'# simple, naive check for equality
#'preflight_checks(X = hier_covid_1020, Y = hier_covid_1020, method = "all_equal") # pass (OK)
#'preflight_checks(hier_covid_1020, hier_covid_mi) # error - not equivalent (desired behavior)
#'
#' ## hier2data -------------------
#' # defaults to compare 'location_id'
#' preflight_checks(hier_covid_771, full_data, method = "hier2data")
#'
#' # Shapefiles ---------------------------
#' # No built-in method yet, coming soon, and you can compare with a little prep work.
#' shp_main <- rgdal::readOGR("/ihme/covid-19/shapefiles/covid_simp_2.shp")
#' shp_locs <- data.frame(location_id = shp_main$loc_id) # %>% mutate(location_id = as.integer(location_id)) # better, but unnecessary
#' class(shp_locs$location_id)
#' preflight_checks(hier_covid_1020, shp_locs, "hier2data") # remember, defaults to 'location_id' only
#'
preflight_checks <- function(
  X, # first 'gold standard' vector or dataframe
  Y=NULL, # second 'to compare' vector or dataframe
  method = c("vec2vec"),
  STOP = FALSE, # should an error stop your script?
  verbose = FALSE, # would you like console or invisible() output?
  user_message = NULL, # custom message to help locate PFC stop-point
  colsX = c("location_id"), # which columns to check from X (left-side gold standard)?
  colsY = NULL # which columns to check from Y? (if column names differ from X)
) {

  # Pre-run validation -------------

  # what method? Valid?
  method_vec <- c("vec2vec", "hier2data", "hier2hier", "data2data",
                  "col_names", "all_equal",
                  "anyNAvec", "anyNAdata", "allNAvec", "allNAdata",
                  "all0vec", "all0data")
  method_vec_readable <- ("\n            vec2vec, hier2data, hier2hier, data2data : (X to Y)
            col_names, all_equal : (column names or full equality)
            anyNAvec, anyNAdata, allNAvec, allNAdata : (X only - NA and Inf)
            all0vec, all0data : (X only - all 0)")

  if(!(method %in% method_vec)){
    stop("<preflight_checks> Invalid method type. Choose: ", method_vec_readable)
  }

  # is Y required by the method?
  Y_req <- c("vec2vec", "hier2data", "hier2hier", "data2data", "col_names", "all_equal")

  if(is.null(Y) & method %in% Y_req){
    stop("<preflight_checks> Y (right side) is required for methods: ", paste(Y_req, collapse = ", "))
  }

  # allows colsX to serve for both data.frames, validates for presence of necessary columns
  if (method %in% c("hier2data", "data2data")){ # only methods that take colsX as an argument
    if (is.null(colsY) & all(colsX %in% names(Y))){
      colsY <- colsX
    } else {
      message("Selected columns in X (colsX): ", paste(colsX, collapse = ", "))
      stop("Selected columns in X are present in Y, and you have not specified 'colsY' ")
    }
  }

  # Defined Tidyverse functions
  `%>%` <- dplyr::`%>%`
  select <- dplyr::select
  filter <- dplyr::filter
  mutate <- dplyr::mutate
  setname <- dplyr::rename
  distinct <- dplyr::distinct
  all_of <- dplyr::all_of
  if_else <- dplyr::if_else
  arrange <- dplyr::arrange
  setdiff <- dplyr::setdiff
  union <- dplyr::union
  intersect <- dplyr::intersect
  copy <- data.table::copy
  # check for scriptName package - shows current filename if sourcing
  if (suppressWarnings(!requireNamespace("scriptName", quietly = TRUE))) {
    message("If you want to print the script name, load with: library(scriptName, lib = '/mnt/share/code/covid-19/r_packages') ")
  } else if (requireNamespace("scriptName", quietly = TRUE)) {
    suppressWarnings(current_filename <- scriptName::current_filename())
  }
requireNamespace("scriptName", quietly = TRUE)
  # keep copies of raw data before prepping for later Out_list info (e.g. location names)
  Xraw <- copy(X)
  Yraw <- copy(Y)
  X_name <- substitute(X) # passing name of 'X' through nested environments for later message/warning
  if(!is.null(Y)) {
    Y_name <- substitute(Y)
  } else {
    Y_name <- "NULL"
  }

  # Prep data for X and Y by selecting columns, renaming columns
  prep_dataX <- function(X = X, colsX = colsX){
    X <- X %>%
      select(all_of(colsX)) %>%
      return(X)

  }

  prep_dataY <- function(Y = Y, colsY = colsY){
    Y <- Y %>%
      select(all_of(colsY)) %>%
      setNames(colsX) %>% # rename columns
      return(Y)
  }


  # After this, colsX and colsY should be equal, and will only refer to colsX for clarity

  # function to stop parent script and give outputs, or allow parent script to continue running
  stop_or_continue <- function(Xname = X_name, Yname = Y_name,
                               STOP = STOP, verbose = verbose,
                               method = method, Out_list = Out_list,
                               helpful_message, stop_condition){

    if(stop_condition & STOP){ # print output, assign output to global env for inspection, stop the parent script

      assign(paste0("PREFLIGHT_CHECK_ERRORS_", method), Out_list, envir = .GlobalEnv) # TODO this may be dangerous
      warning("<preflight_checks>: Stop condition met:", "\n",
              user_message, "\n",
              "filename is: ", if (requireNamespace("scriptName", quietly = TRUE)) {
                c(current_filename(),"\n")
              } else{
                "scriptName not loaded\n"
              },
              helpful_message, "\n",
              "method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n",
              call. = F)
      warning(helpful_message, call. = F) # dummy warning so all warnings print correctly to console
      stop(paste0("PREFLIGHT_CHECK_ERRORS_", method), " is saved to .GlobalEnv", call. = F)


    } else if(stop_condition & !STOP & verbose){

      message("INLINE <preflight_checks>: Stop condition met, but STOP set to FALSE, showing differences above:", "\n",
              user_message, "\n",
              helpful_message, "\n",
              "method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n")

      warning("<preflight_checks>: Stop condition met, but STOP set to FALSE, showing differences above:", "\n",
              "filename is: ", if (requireNamespace("scriptName", quietly = TRUE)) {
                c(current_filename(),"\n")
              } else{
                "scriptName not loaded\n"
              },
              user_message, "\n",
              helpful_message, "\n",
              "method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n",
              call. = F)

      print(Out_list) # need both to print to console if sourcing or running line-by-line
      invisible(Out_list)

    } else if (stop_condition & !STOP & !verbose){

      message("INLINE <preflight_checks>: Stop condition met, but STOP set to FALSE, showing differences above:", "\n",
              helpful_message, "\n",
              "method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n")

      warning("<preflight_checks>: Stop condition met, but STOP set to FALSE, showing differences above:", "\n",
              helpful_message, "\n",
              "method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n",
              call. = F)

      invisible(Out_list) # return invisibly for assignment

    } else if (!stop_condition & verbose) {

      message("INLINE <preflight_checks> method is: ", method, "\n",
              "filename is: ", if (requireNamespace("scriptName", quietly = TRUE)) {
                c(current_filename(),"\n")
              } else{
                "scriptName not loaded\n"
              },
              user_message, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n",
              "Passed the stop condition - continuing script.", "\n")

      print(Out_list) # need both to print to console if sourcing or running line-by-line
      invisible(Out_list)

    } else if (!stop_condition & !verbose) {

      message("INLINE <preflight_checks> method is: ", method, "\n",
              "X (left-side): ", Xname, "\n",
              "Y (right-side): ", Yname, "\n",
              "Passed the stop condition - continuing script.", "\n")

      invisible(Out_list) # return invisibly for assignment

    } else {
      stop("<preflight_checks> Something went wrong - debug the stop_or_continue function if you see this message")
    }
  }



  # Method 1 : vec2vec ----------------------------
  # Most general - compare two vectors for equality

  check_vec2vec <- function (X,Y) {

    Out_list <- list(
      "length_X"   = length(X),
      "length_Y"   = length(Y),
      "unique_in_X_not_Y" = setdiff(X,Y),
      "unique_in_Y_not_X" = setdiff(Y,X)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = length(setdiff(X,Y)) > 0,
                     helpful_message = "There is a difference between unique vector values. See above.")

  }

  # Method 2 : data2data ----------------------------
  # Broad comparison of two datasets across many dimensions
  # Stops if distinct rows differ between datasets

  check_data2data <- function(X, Y, colsX){

    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX)
    Y <- prep_dataY(Y = Y, colsY = colsY)
    message("Columns in X: ", paste(names(X), collapse = ", "))
    message("Columns in Y: ", paste(names(Y), collapse = ", "))

    Out_list <- list(
      "dim_X" = dim(X),
      "dim_Y" = dim(Y),
      "in_X_not_Y_(unique)" = setdiff(X,Y),
      "in_Y_not_X_(unique)" = setdiff(Y,X)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = nrow(setdiff(distinct(X), distinct(Y))) > 0,
                     helpful_message = "There is a difference between data.frames. See above.")

  }

  # Method 3 : hier2data  ----------------------------
  # Compare hierarchy locations to a dataset

  check_hier2data <- function(X, Y, colsX){
    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX)
    Y <- prep_dataY(Y = Y, colsY = colsY)
    message("Columns in X: ", paste(names(X), collapse = ", "))
    message("Columns in Y: ", paste(names(Y), collapse = ", "))

    Out_list <- list(
      "locs_in_X_not_in_Y" = setdiff(X$location_id, Y$location_id),
      "locs_in_Y_not_in_X" = setdiff(Y$location_id, X$location_id),
      "names_in_X_not_in_Y" = Xraw %>% filter(location_id %in% setdiff(X$location_id, Y$location_id)) %>% select(location_id, location_name)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = length(setdiff(X$location_id, Y$location_id)) > 0,
                     helpful_message = c("Not all locations in X are present in Y: ",
                                         paste0(setdiff(X$location_id, Y$location_id), collapse = ", ")))

  }

  # Method 4 : hier2hier ----------------------------
  # compare hierarchies for equivalence on important columns

  check_hier2hier <- function(X = Xraw, Y = Yraw){
    # Bypassing normal data-prep to choose important hierarchy variables
    hier_cols <- c("location_id", "location_name", "path_to_top_parent", "most_detailed")
    X <- X %>% select(all_of(hier_cols))
    Y <- Y %>% select(all_of(hier_cols))

    path_vars <- c("location_id", "path_to_top_parent") # for finding path_to_top_parent diffs
    Xpath <- X %>% select(all_of(path_vars))
    Ypath <- Y %>% select(all_of(path_vars))

    det_vars <- c("location_id", "most_detailed")
    Xdet <- X %>% select(all_of(det_vars))
    Ydet <- Y %>% select(all_of(det_vars))

    Out_list <- list(
      "in_X_not_Y_(unique)" = setdiff(X,Y),
      "in_Y_not_X_(unique)" = setdiff(Y,X),
      "names_in_X_not_in_Y" = X %>% filter(location_id %in% setdiff(X$location_id, Y$location_id)) %>% select(location_id, location_name),
      "names_in_Y_not_in_X" = Y %>% filter(location_id %in% setdiff(Y$location_id, X$location_id)) %>% select(location_id, location_name),
      "locs_in_X_not_in_Y" = setdiff(X$location_id, Y$location_id),
      "locs_in_Y_not_in_X" = setdiff(Y$location_id, X$location_id),
      "all_mismatch_path_to_top_parent" = setdiff(union(Xpath,Ypath), intersect(Xpath,Ypath)) %>% arrange(location_id),
      "all_mismatch_most_detailed" = setdiff(union(Xdet,Ydet), intersect(Xdet,Ydet)) %>% arrange(location_id)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition =
                       !all(
                         sapply(
                           Out_list,
                           function(x) {
                             (is.data.frame(x) && nrow(x) == 0) | # are list data.frames empty?
                               (is.vector(x) && length(x) == 0) # are list vectors empty?
                           }
                         )
                       ),
                     helpful_message = c("Hierarchies are unequal somewhere in these variables: ",
                                         paste(hier_cols, collapse = ", ")))

  }

  # Method 5 : col_names ----------------------------
  # Look for only misaligned column names

  check_col_names <- function(X,Y){
    # Bypassing all data-prep
    Out_list <- list(
      "cols_in_X_not_Y" = setdiff(names(X), names(Y)),
      "cols_in_Y_not_X" = setdiff(names(Y), names(X)),
      "all_mismatch_columns" = setdiff( union( names(X), names(Y)), intersect(names(X), names(Y)) )
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition =
                       !all(
                         sapply(
                           Out_list,
                           function(x) {
                             (is.data.frame(x) && nrow(x) == 0) | # are list data.frames empty?
                               (is.vector(x) && length(x) == 0) # are list vectors empty?
                           }
                         )
                       ),
                     helpful_message = c("Columns are unequal somewhere: ", paste0(names(X), collapse = ", ")))

  }

  # Method 6 : all_equal ------------------
  # Check for equality between two objects with stringent stop behavior

  check_all_equal <- function(X, Y){

    # TODO experiment with trycatch here

    Out_list <- list(
      "all_equal" = all.equal(X,Y),
      "in_X_not_Y_(unique)" = setdiff(X,Y),
      "in_Y_not_X_(unique)" = setdiff(Y,X),
      "cols_in_X_not_Y" = setdiff(names(X), names(Y)),
      "cols_in_Y_not_X" = setdiff(names(Y), names(X))
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = !isTRUE(all.equal(X,Y)), # must be added for each method
                     helpful_message ="Dataframes are not all equal, see above.")

  }

  # Method 7 : anyNAvec ----------------------------
  # Are any missing values in a vector?

  check_anyNAvec <- function(X){

    if(!is.vector(X)){
      stop("X is not a vector, consider method = 'anyNAdata'")
    }

    Out_list <- list(
      "NA_indexes"  = which(is.na(X)),
      "Inf_indexes" = which(is.infinite(X))
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = (anyNA(X) | any(is.infinite(X))),
                     helpful_message = "There are missing/Inf values in your vector. See above.")
  }

  # Method 8 : anyNAdata ----------------------------
  # Are any missing values in some columns?

  check_anyNAdata <- function(X, colsX) {

    if(!is.data.frame(X)){
      stop("X is not a data.frame, consider method = 'anyNAvec'")
    }

    X <- X %>% select(colsX) # select cols for checking

    tmp_NA <- vector('list', length = length(colsX))
    names(tmp_NA) <- paste0(names(X), "_NA_indexes")
    for(i in 1:ncol(X)) {tmp_NA[[i]] <- which(is.na(X[,i]))}

    tmp_Inf <- vector('list', length = length(colsX))
    names(tmp_Inf) <- paste0(names(X), "_Inf_indexes")
    for(i in 1:ncol(X)) {tmp_Inf[[i]] <- which(is.infinite(X[,i]))}

    Out_list <- list(tmp_NA, tmp_Inf)
    names(Out_list) <- c("NA", "Inf")

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = (anyNA(X) |
                                         any(sapply(X, FUN = function(col) any(is.infinite(col))))),
                     helpful_message = "There are missing/Inf values in your data. NA indexes by column listed above.")

  }

  # Method 9 : allNAvec ----------------------------
  # Are all values missing in a vector?

  check_allNAvec <- function(X){
    if(!is.vector(X)){
      stop("X is not a vector, consider method = 'allNAdata'")
    }

    Out_list <- list(
      "Vec_value"  = unique(X)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = (all(is.na(X)) | all(is.infinite(X))),
                     helpful_message = "Vector is entirely NA/Inf. See above.")

  }

  # Method 10 : allNAdata ----------------------------
  # Are ANY columns ALL missing/infinite?

  check_allNAdata <- function(X, colsX){

    if(!is.data.frame(X)){
      stop("X is not a data.frame, consider method = 'allNAvec'")
    }

    X <- X %>% select(colsX) # select cols for checking

    Out_list <- list()
    for (i in colsX){
      Out_list[[i]] <- all(is.na(X[,i]) | is.infinite(X[,i]))
    }

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = sum(unlist(Out_list), na.rm = T) > 0,
                     helpful_message = "Some columns are entirely NA/Inf (TRUE in output). See above.")


  }

  # Method 11 : all0vec ----------------------------
  # Are all values 0 in a vector?

  check_all0vec <- function(X){
    if(!is.vector(X)){
      stop("X is not a vector, consider method = 'all0data' ")
    }

    Out_list <- list(
      "Vec_value"  = unique(X)
    )

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = (all(X==0)),
                     helpful_message = "Vector is all 0. See above.")

  }

  # Method 12 : all0data ----------------------------
  # Are ANY columns ALL 0?

  check_all0data <- function(X, colsX){

    if(!is.data.frame(X)){
      stop("X is not a data.frame, consider method = 'all0vec' ")
    }

    X <- X %>% select(colsX) # select cols for checking

    Out_list <- list()
    for (i in colsX){
      Out_list[[i]] <- all((X[,i]) == 0)
    }

    stop_or_continue(STOP = STOP, verbose = verbose,
                     method = method, Out_list = Out_list,
                     stop_condition = sum(unlist(Out_list), na.rm = T) > 0,
                     helpful_message = "Some columns are entirely 0 (TRUE in output). See above.")
  }


  # Switch --------------
  # call the different check functions depending on method required
  switch (method,
          "data2data"    = check_data2data(X = X, Y = Y, colsX = colsX),
          "hier2data"    = check_hier2data(X = X, Y = Y, colsX = colsX),
          "hier2hier"    = check_hier2hier(X = X, Y = Y),
          "col_names"    = check_col_names(X = X, Y = Y),
          "all_equal"    = check_all_equal(X = X, Y = Y),
          "vec2vec"      = check_vec2vec  (X = X, Y = Y),
          "anyNAvec"     = check_anyNAvec (X = X),
          "anyNAdata"    = check_anyNAdata(X = X, colsX = colsX),
          "allNAvec"     = check_allNAvec (X = X),
          "allNAdata"    = check_allNAdata(X = X, colsX = colsX),
          "all0vec"      = check_all0vec  (X = X),
          "all0data"     = check_all0data (X = X, colsX = colsX)
  )

}
