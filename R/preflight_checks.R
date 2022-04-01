# TODOs -------------

# - [ ]
#   - [ ]

# Now
# - [x] Allow for column renaming
# - [x] Limit data2data comparisons to unique values in each
# - [x] Allow for filtering as well as column selection
#   - [x] Can I allow for multiple filters?
# - [x] Add a top-level function to select, rename and filter data.frames
# - [x] define tidyverse functions to avoid require() or library()
# - [X] Add 'stop' argument
# - [x] Incorporate switch() for 'method' argument
# - [x] Write the "stop" chunk as a generalizable function
#   - [x] main chunk
#   - [x] needs a stop-condition argument, or else it stops every time
# - [ ] read through all steps and align documentation notes with actual processes
# - [ ] more toture testing, especially for stop_conditions

# This is still a bit too cumbersome
# - [x] rename 'data2data' as a more generic function, which it is
# - [x] create a true heirarchy to dataset function, with assumed inputs (pretty safe)
# - [x] hier2hier function
# - [x] column check function

# Later
# - [ ] look into tryCatch()
# - [ ] look at Tom's shapefiles

# "main" = "/ihme/covid-19/shapefiles/covid_simp_2.shp",
# Contains French Guiana 143, New Caledonia 228, Western Sahara 313
# "unmodeled" = "/home/j/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/inset_maps/noSubs/GBD_WITH_INSETS_NOSUBS.shp",
# For India special Jammu & Kashmir and Ladakh 4854
# "india_disputed" = "/home/j/WORK/11_geospatial/admin_shapefiles/current/IND_full_ad1.shp",
# "india_disputed_mask" = "/home/j/WORK/11_geospatial/admin_shapefiles/2020_05_21/lbd_disputed_mask.shp",
# "ind_lalit" = "/ihme/covid-19/shapefiles/india_shp/STATE_2020.shp",
# "mainland_china" = "/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2020/master/shapefiles/GBD2020_mapping_final.shp",
# "hong_kong_macao" = "/home/j/DATA/SHAPE_FILES/GBD_geographies/master/covid/20210210/covid_clean.shp"


# --- prep environment for development work ---
rm(list = ls(all.names = T))
source("R/env_setup_for_package_dev.R")


# FUNCTION BODY ------
#' Compare reference vectors or data.frames using Tidyverse grammar
#'
#' Returns a list of diagnostic information.  I recommend saving this output
#' to an object with a name that is diagnostically helpful if you set STOP=FALSE
#'
#' 'X' is intended as the left-side, 'reference' data - that which is your gold standard to compare against
#' 'Y' is intended as the rigth-side 'comparison' data - that which you want to validate AGAINST X
#'
#' This function may STOP a script if specified.
#'
#' Many 'methods' of comparison are available - check function arguments.
#'
#' Suppose your pipeline depends on having data for all locations in a given
#' hierarchy. Suppose you need to compare two hierarchies directly for equality
#' across a range of columns (c("location_id", "location_name",
#' "path_to_top_parent", "most_detailed") are defaults). This functions takes a
#' left (X) and right (Y) vector or data.frame as input, then compares them
#' according to a desired comparison method.
#'
#' If you need to compare data.frame columns with the same content but different
#' column names, enter those names in the same order in colsX and colsY
#' arguments, and the function will use colsX (left-hand data.frame) to rename
#' all columnn, and output will return those names.
#'
#' Filtering occurs AFTER column selection.  Format all filters according to colsX names.
#'
#' Order of operations:
#' select()
#' setNames()
#' filter()
#'
#' @param X Left-hand input vector or data.frame (gold standard to compare against)
#' @param Y Right-hand input vector or data.frame (compared AGAINST your gold standard, by 'method')
#' @param method method of comparison to be made:
#' all_equal = compare two vectors or data.frames for total equality.  WARNING: no selecting or filtering applied.  stop_condition all.equal is not TRUE.
#' data2data = compares two generic data.frames and checks for common UNIQUE values in columns between data.frames. stop_condition if nrow(setdiff(distinct(X), disctinct(Y))) > 0.
#' hier2data = compares a data frome on the right against a gold-standard heirarchy on the left. stop_condition = length(setdiff(X$location_id, Y$location_id)) > 0
#' hier2hier = compares two hierarchies for exact equivalence across c("location_id", "location_name", "path_to_top_parent", "most_detailed"). stop_condition is any differences.
#' compare_cols = compares data.frames for same column names. stop_condition is any differences.
#' @param verbose Do you want verbose console output, or invisible() return of the output_list for assignment to a variable?
#' @param colsX Only for '___2data' methods. Vector of column names in "reference" vector or data.frame - keep column order consistent!
#' @param colsY Only for '___2data' methods. Vector of column names in the "comparison" vector or data.frame - WARNING: MAKE SURE column order matches that of colsX
#' @param filter_statement # Only for '___2data' methods. All your desired filtering arguments, as a single, quoted, character string.
#' e.g. "location_id > 100 & most_detailed==0"
#' Use column names from colsX, because filtering occurs after column selection.
#' It's less cumbersome to filter before using preflight_checks, but this option
#' exists in case.
#' @param STOP Do you want to stop your script if there is a mismatch, or allow to continue with a WARNING message?
#'
#' @return Output_list
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
#'
#'MREs -----------------
#'source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
#'
#'# HIERARCHIES
#'
#'# covid
#'hier_covid_771 <- get_location_metadata(111, 771, release_id = 9)
#'hier_covid_1020 <- get_location_metadata(111, 1020, release_id = 9)
#'hier_covid_mi <- fread('/mnt/share/covid-19/model-inputs/2022_03_30.01/locations/modeling_hierarchy.csv')
#'
#'# Real data --------------------
#'
#'# full_data
#'full_data <- fread('/mnt/share/covid-19/model-inputs/2022_03_30.01/full_data_unscaled.csv')
#'full_data_prev <- fread('/mnt/share/covid-19/model-inputs/2022_03_29.01/full_data_unscaled.csv')
#'full_formatted <- fread('/mnt/share/covid-19/model-inputs/2022_03_30.01/full_data_unscaled_formatted_dates.csv')
#'
#'# methods ---------
#'
#'## all_equal ---------------
#'
#'# simple, naive check for equality
#'preflight_checks(X = hier_covid_1020, Y = hier_covid_1020, method = "all_equal") # pass (OK)
#'preflight_checks(hier_covid_1020, hier_covid_mi) # error - not equivalent (desired behavior)
#'# equivalent
#' preflight_checks(hier_covid_1020, hier_covid_1020) # pass (O)
#' # check LSVIDs against each other, using current as 'left side'
#' preflight_checks(hier_covid_1020, hier_covid_771) # fail (OK)
#' preflight_checks(hier_covid_1020, hier_covid_771, verbose = T) # prints warning, continues
#' preflight_checks(hier_covid_1020, hier_covid_771, STOP = T, verbose = T) # stops, saves errors to .GlobalEnv
#' preflight_checks(hier_covid_1020, hier_covid_1020, STOP = T, verbose = T) # keep going, see receipt of passing
#'
#' ## hier2hier -------------------
#' # save your output for inspection if not verbose
#' output_pass <- preflight_checks(hier_covid_1020, hier_covid_1020, "hier2hier")
#' output_pass$names_in_X_not_in_Y
#' output_fail <- preflight_checks(hier_covid_1020, hier_covid_771, "hier2hier")
#' output_fail$names_in_X_not_in_Y
#'
#' ## data2data -----------------
#' preflight_checks(full_data, full_data, "data2data")
#' preflight_checks(full_data, full_formatted, "data2data") # why does this pass? (OK)
#' preflight_checks(full_data, full_formatted, "data2data", colsX = c("location_id", "Date_formatted")) # pass (OK)
#' # is full_data equivalent to formatted date data?
#' preflight_checks(full_data, full_formatted, "data2data", colsX = names(full_data)) # pass (ok)
#' # compare to yesterday's data?
#' preflight_checks(full_data, full_data_prev, "data2data", colsX = names(full_data)) # fail (ok)
#'
#' ## compare_cols -----------------
#' preflight_checks(hier_covid_1020, hier_covid_771, "compare_cols")
#' preflight_checks(full_data, full_formatted, "compare_cols")
#'
#'
#' # Shapefiles ---------------------------
#' # No built-in method yet, but it won't be hard, and ther
#' shp_main <- rgdal::readOGR("/ihme/covid-19/shapefiles/covid_simp_2.shp")
#' shp_locs <- data.frame(location_id = shp_main$loc_id) # %>% mutate(location_id = as.integer(location_id)) # better, but unnecessary
#' class(shp_locs$location_id)
#' preflight_checks(hier_covid_1020, shp_locs, "hier2data") # remember, defaults to 'location_id' only
#'
preflight_checks <- function(
    X, # first 'gold standard' dataframe
    Y, # second 'to compare' dataframe
    method = c("all_equal"), # c("all_equal", "data2data", "hier2data", "hier2hier", "compare_cols")
    colsX = c("location_id"), # which columns to check from X (left-side gold stanard)?
    colsY = NULL, # which columns to check from Y? (if column names differ from X)
    filter_statement = NULL, #e.g. "location_id > 100 & most_detailed==0"
    STOP = FALSE, # should an error stop your script?
    verbose = FALSE # would you like console or invisible() output?
) {

  # what method?
  message("method is ", "'", method, "'")

  # Defined Tidyverse functions
  `%>%` <- dplyr::`%>%`
  select <- dplyr::select
  filter <- dplyr::filter
  mutate <- dplyr::mutate
  setname <- dplyr::rename
  distinct <- dplyr::distinct
  all_of <- dplyr::all_of

  # allows colsX to serve for both data.frames, with a check for presence of necessary columns
  if (is.null(colsY) & all(colsX %in% names(Y))){
    colsY <- colsX
  } else {
    stop("Not all columns in X are present in Y, and you have not specified 'colsY' ")
  }

  # keep copies of raw data before prepping for later Out_list info (e.g. location names)
  Xraw <- copy(X)
  Yraw <- copy(Y)

  # Prep data for X and Y by selecting columns, renaming columns, filtering rows
  prep_dataX <- function(X = X, colsX = colsX, filter_statement = filter_statement){
    X <- X %>%
      select(all_of(colsX)) %>%
      # if a filter statement exists, use it, otherwise include all rows
      filter(ifelse(!is.null(filter_statement), eval(parse(text = filter_statement)), all_of(colsX[1] %in% colsX[1])))
    return(X)
  }

  prep_dataY <- function(Y = Y, colsY = colsY, filter_statement = filter_statement){
    Y <- Y %>%
      select(all_of(colsY)) %>%
      setNames(colsX) %>% # rename columns
      filter(ifelse(!is.null(filter_statement), eval(parse(text = filter_statement)), all_of(colsX[1] %in% colsX[1])))
    return(Y)
  }


  # After this, colsX and colsY should be equal, and will only refer to colsX for clarity

  # function to stop parent script and give outputs, or allow parent script to continue running
  stop_or_continue <- function(STOP = STOP, method = method, Out_list = Out_list, helpful_message, stop_condition){

    if(stop_condition & STOP){ # print output, assign output to global env for inspection, stop the parent script
      assign(paste0("ERRORS_", method), Out_list, envir = .GlobalEnv) # TODO this may be dangerous
      stop(helpful_message,  paste0(": ERRORS_", method, " is saved to .GlobalEnv"))

    } else if(stop_condition & !STOP & verbose){
      warning("WARNING: Stop condition met, but STOP set to FALSE, showing differences above.")
      print(Out_list)

    } else if (stop_condition & !STOP & !verbose){
      warning("WARNING: Stop condition met, but STOP set to FALSE, showing differences above.")
      invisible(Out_list)

    } else if (!stop_condition & verbose) {
      # print(Out_list)
      message("Passed the stop condition - continuing script.")
      return(Out_list) # return verbose to console and for assignment

    } else if (!stop_condition & !verbose) {
      message("Passed the stop condition - continuing script.")
      invisible(Out_list) # return invisibly for assignment

    } else {
      stop("Something went wrong - debug the stop_or_continue function if you see this message")
    }
  }


  # Method 1 : all_equal ------------------
  # Check for equality between two objects with stringent stop behavior

  check_all_equal <- function(X, Y){

    # TODO experiment with trycatch here

    Out_list <- list(
      "all_equal" = all.equal(X,Y),
      "in_X_not_Y" = setdiff(X,Y),
      "in_Y_not_X" = setdiff(Y,X),
      "cols_in_X_not_Y" = setdiff(names(X), names(Y)),
      "cols_in_Y_not_X" = setdiff(names(Y), names(X))
    )

    stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                     stop_condition = !isTRUE(all.equal(X,Y)), # must be added for each method
                     helpful_message ="Dataframes are not all equal, see above.")
    # }
  }

  # Method 2 : data2data ----------------------------
  # Broad comparison of two datasets across many dimensions
  # Stops if distinct rows differ between datasets

  check_data2data <- function(X, Y, colsX){

    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX, filter_statement = filter_statement)
    Y <- prep_dataY(Y = Y, colsY = colsY, filter_statement = filter_statement)
    message("Columns in X: ", paste(names(X), collapse = ", "))
    message("Columns in Y: ", paste(names(Y), collapse = ", "))

    Out_list <- list(
      "dim_X" = dim(X),
      "dim_Y" = dim(Y),
      "in_X_not_Y" = setdiff(X,Y),
      "in_Y_not_X" = setdiff(Y,X),
      "distinct_X" = X %>% distinct(),
      "distinct_Y" = Y %>% distinct()
    )

    stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                     stop_condition = nrow(setdiff(distinct(X), distinct(Y))) > 0,
                     helpful_message = "There is a difference between data.frames")

    # return(Out_list)
  }

  # Method 3 : hier2data  ----------------------------
  # Compare hierarchy locations to a dataset

  check_hier2data <- function(X, Y, colsX){
    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX, filter_statement = filter_statement)
    Y <- prep_dataY(Y = Y, colsY = colsY, filter_statement = filter_statement)
    message("Columns in X: ", paste(names(X), collapse = ", "))
    message("Columns in Y: ", paste(names(Y), collapse = ", "))

    Out_list <- list(
      "locs_in_X_not_in_Y" = setdiff(X$location_id, Y$location_id),
      "locs_in_Y_not_in_X" = setdiff(Y$location_id, X$location_id),
      "names_in_X_not_in_Y" = Xraw %>% filter(location_id %in% setdiff(X$location_id, Y$location_id)) %>% select(location_id, location_name)
    )

    stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                     stop_condition = length(setdiff(X$location_id, Y$location_id)) > 0,
                     helpful_message = "Not all locations in X are present in Y")

    # return(Out_list)

  }

  # Method 4 : hier2hier ----------------------------
  # compare hierarchies for equivalence on important columns

  check_hier2hier <- function(X = Xraw, Y = Yraw){
    # Bypassing normal data-prep to choose important hierarchy variables
    hier_cols <- c("location_id", "location_name", "path_to_top_parent", "most_detailed")
    X <- X %>% select(all_of(hier_cols))
    Y <- Y %>% select(all_of(hier_cols))

    path_vars <- c("location_id", "path_to_top_parent") # for finding path_to_top_parent diffs
    Xpath <- X %>% select(path_vars)
    Ypath <- Y %>% select(path_vars)

    det_vars <- c("location_id", "most_detailed")
    Xdet <- X %>% select(path_vars)
    Ydet <- Y %>% select(path_vars)

    Out_list <- list(
      "in_X_not_Y" = setdiff(X,Y),
      "in_Y_not_X" = setdiff(Y,X),
      "names_in_X_not_in_Y" = X %>% filter(location_id %in% setdiff(X$location_id, Y$location_id)) %>% select(location_id, location_name),
      "names_in_Y_not_in_X" = Y %>% filter(location_id %in% setdiff(Y$location_id, X$location_id)) %>% select(location_id, location_name),
      "locs_in_X_not_in_Y" = setdiff(X$location_id, Y$location_id),
      "locs_in_Y_not_in_X" = setdiff(Y$location_id, X$location_id),
      "all_mismatch_path_to_top_parent" = setdiff(union(Xpath,Ypath), intersect(Xpath,Ypath)),
      "all_mismatch_most_detailed" = setdiff(union(Xdet,Ydet), intersect(Xdet,Ydet))
    )

    stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                     stop_condition = !all(sapply(Out_list, function(x){(class(x) %in% c("data.table", "tibble", "data.frame") &&
                                                                                 nrow(x) == 0) | (class(x) %in% c("character", "numeric", "integer") && length(x) == 0)})),
                     helpful_message = c("Hierarchies are unequal somewhere in these variables, please check output: ", paste(hier_cols, collapse = ", ")))

    # return(Out_list)

  }
  # Method 5 : compare_cols ----------------------------
  # Look for only misaligned column names

  check_compare_cols <- function(X,Y){
    # Bypassing all data-prep
    Out_list <- list(
      "cols_in_X_not_Y" = setdiff(names(X), names(Y)),
      "cols_in_Y_not_X" = setdiff(names(Y), names(X)),
      "all_mismatch_columns" = setdiff( union( names(X), names(Y)), intersect(names(X), names(Y)) )
    )

    stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                     stop_condition = !all(sapply(Out_list, function(x){(class(x) %in% c("data.table", "tibble", "data.frame") &&
                                                                                 nrow(x) == 0) | (class(x) %in% c("character", "numeric", "integer") && length(x) == 0)})),
                     helpful_message = c("Columns are unequal somewhere, please check output."))

  }

  # Switch --------------
  # call the different check functions depending on method required
  switch (method,
          "all_equal"    = check_all_equal(X = X, Y = Y),
          "data2data"    = check_data2data(X = X, Y = Y, colsX = colsX),
          "hier2data"    = check_hier2data(X = X, Y = Y, colsX = colsX),
          "hier2hier"    = check_hier2hier(X = X, Y = Y),
          "compare_cols" = check_compare_cols(X = X, Y = Y)
  )

}


# SANDBOX -----------------

# MREs -----------------------------

source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
hier_covid_771 <- getloc


# Vetting -----------------------
X <- hier_covid
Y <- copy(X)
Y <- hier_gbd
thing1 <- hier_covid %>% select(location_id, path_to_top_parent)
thing2 <- hier_covar %>% select(location_id, path_to_top_parent)
setdiff(union(thing1, thing2), intersect(thing1, thing2))
# setdiff(thing1, thing2)
# setdiff(thing2, thing1)
X %>% left_join(Y, by = c("location_id", "path_to_top_parent"))

# compare_cols
preflight_checks(Equal1, Equal2, method = "compare_cols")
preflight_checks(Equal1, Equal2, method = "compare_cols", STOP = T)
preflight_checks(Diff1, Diff2, method = "compare_cols", STOP = F)
preflight_checks(Diff1, Diff2, method = "compare_cols", STOP = T)

#hier2hier
preflight_checks(hier_covid, hier_covid, method = "hier2hier")

# hier2data

preflight_checks(hier_covid, full_data, method = "hier2data", STOP = F)
preflight_checks(hier_covid, hier_covid, method = "hier2data", STOP = F)
hier_covid %>% filter(location_id %in% setdiff(hier_covid$location_id, full_data$location_id) ) %>%  select(location_id, location_name)


# data2data
X <- Diff1
Y <- Diff2
colsX <- c("location_id", "path_to_top_parent")


preflight_checks(Diff1, Diff2, method = "data2data", colsX = "location_id")
preflight_checks(Diff1, Diff2, method = "data2data", colsX = c("location_id", "path_to_top_parent"))

preflight_checks(Equal1, Equal2, method = "data2data", colsX = c("location_id", "path_to_top_parent"))

hier_check <- hier_covid %>% filter(most_detailed==1)
check270 <- preflight_checks(hier_check, full_data, method = 'data2data', colsX = c("location_id"), STOP = F)
xlocs <- ERRORS_data2data$in_X_not_Y
ylocs <- ERRORS_data2data$in_Y_not_X
hier_check %>% filter(location_id %in% xlocs$location_id)
ylocsnames <- full_data %>% filter(location_id %in% ylocs$location_id) %>% distinct(location_id, location_name)

check270 <- preflight_checks(full_data, full_data, method = 'data2data', colsX = c("location_id"), STOP = T)

# still a bit cumbersome to use...


# all_equal
# naive, no method specified
preflight_checks(Equal1, Equal2, STOP=F)
preflight_checks(Equal1, Equal2, STOP=T)
preflight_checks(Diff1, Diff2, method = "all_equal", STOP = F)
preflight_checks(Diff1, Diff2, method = "all_equal", STOP = T)


# early column mismatch
preflight_checks(X = Diff2,
                 Y = Diff1,
                 method = "data2data",
                 colsX = c("location_id", "lancet_label")) # should stop
preflight_checks(X = Diff2,
                 Y = Diff1,
                 method = "data2data",
                 colsX = c("location_id", "location_name")) # should continue with warning


# --- test some things ---

# lots of output, maybe too much
all.equal(HIER$covariate_with_aggregates_hierarchy$location_id, HIER$gbd_analysis_hierarchy$location_id)
# output if equal?
all.equal(equal1, equal2) #TRUE

setdiff(names(HIER$covariate_with_aggregates_hierarchy), names(HIER$gbd_analysis_hierarchy))
setdiff(names(HIER$gbd_analysis_hierarchy), names(HIER$covariate_with_aggregates_hierarchy))
setdiff(names(h_covid), names(h_gbd))
setdiff(names(h_gbd), names(h_covid))
