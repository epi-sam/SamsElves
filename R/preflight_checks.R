# TODOs -------------

# Now
# - [ ]
#   - [ ]

# - [x] Allow for column renaming
# - [x] Limit hier2data comparisons to unique values in each
# - [x] Allow for filtering as well as column selection
#   - [x] Can I allow for multiple filters?
# - [x] Add a top-level function to select, rename and filter data.frames
# - [x] define tidyverse functions to avoid require() or library()
# - [ ] read through all steps and align documentation notes with actual processes
# - [ ] Decide how to functionalize 'verbose' argument in the code
# - [X] Add 'stop' argument
# - [x] Incorporate switch() for 'method' argument
# - [x] Write the "stop" chunk as a generalizable function
#   - [x] main chunk
#   - [x] needs a stop-condition argument, or else it stops every time

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
#' @param X Left-hand input vector or data.frame (depending on desired check method)
#' @param Y Right-hand input vector or data.frame (depending on desired check method)
#' @param method method of comparison to be made:
#' all_equal = compare two vectors or data.frames for total equality, and receive verbose output if not.  WARNING: no selecting or filtering applied.  STOPS if all.equal is not TRUE.
#' hier2data = compares a hierarchy to incoming raw_data or full_data, and checks for common UNIQUE values in columns between data.frames. STOPS if nrow(setdiff(distinct(X), disctinct(Y)))>0.
#' @param verbose
#' @param colsX Vector of column names in "reference" vector or data.frame - keep column order consistent!
#' @param colsY Vector of column names in the "comparison" vector or data.frame - WARNING: MAKE SURE column order matches that of colsX
#' @param filter_statement # all your desired filtering arguments, as a single, quoted, character string.
#' Use column names from colsX, as filtering occurs after column selection
#' e.g. "location_id > 100 & most_detailed==0"
#' @param STOP Do you want to stop your script if there is a mismatch?
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
#'
#' Diff1 <- get_location_metadata(location_set_id = 111, location_set_version_id = 1020, release_id = 9)
#' Diff2 <- get_location_metadata(location_set_id = 35, release_id = 9)
#'
#' # you may tidy-select columns in the function call
#' preflight_checks(Diff1, Diff2, method = "all_equal", STOP = T, colsX = Diff1 %>% select(contains("location")) %>% names)
#'
#' When comparing a hierarchy to data/populations/shapefiles, I recommend
#' selecting columns that would create a distinct match (e.g. location_id), and
#' using method = "hier2data"
#'
#' When comparing two hierarchies, I recommend selecting a set of
#' columns that should ALL be the same (e.g. c("location_id",
#' "path_to_top_parent")), and using method = "all_equal"
#'
preflight_checks <- function(
    X, # first dataframe
    Y, # second dataframe
    method = c("all_equal"), # "all_eqal" "hier2data"
    colsX = c("location_id", "location_name", "path_to_top_parent", "most_detailed"), # which columns to check from X?
    colsY = NULL, # which columns to check from Y? (if column names differ from X)
    filter_statement = NULL,
    STOP = TRUE,
    verbose # how do I want to functionalize this?
) {

  # Defined Tidyverse functions
  `%>%` <- magrittr::`%>%`
  select <- dplyr::select
  filter <- dplyr::filter
  mutate <- dplyr::mutate
  setname <- dplyr::rename

  # allows colsX to serve for both data.frames, with a check for presence of necessary columns
  if (is.null(colsY) & all(colsX %in% names(Y))){
    colsY <- colsX
  } else {
    stop("Not all columns in X are present in Y, and you have not specified 'colsY' ")
  }


  # Prep data for X and Y by selecting columns, renaming columns, filtering rows
  prep_dataX <- function(X = X, colsX = colsX){
    X <- X %>%
      select(all_of(colsX)) %>%
      # if a filter statement exists, use it, otherwise include all rows
      filter(ifelse(!is.null(filter_statement), eval(parse(text = filter_statement)), all_of(colsX[1] %in% colsX[1])))
    return(X)
  }

  prep_dataY <- function(Y = Y, colsY = colsY){
    Y <- Y %>%
      select(all_of(colsY)) %>%
      setNames(colsX) %>% # rename columns
      filter(ifelse(!is.null(filter_statement), eval(parse(text = filter_statement)), all_of(colsX[1] %in% colsX[1])))
    return(Y)
  }


  # After this, colsX and colsY should be equal, and will only refer to colsX for clarity

  # function to stop parent script and give outputs, or allow parent script to continue running
  stop_or_continue <- function(STOP = STOP, method = method, Out_list = Out_list, helpful_message, stop_condition){
    if(STOP){ # print output, assign output to global env for inspection, stop the parent script
      print(Out_list)
      assign(paste0("ERRORS_", method), Out_list, envir = .GlobalEnv) # TODO this may be dangerous
      if(stop_condition){
        stop(helpful_message,  paste0(": ERRORS_", method, " is saved to .GlobalEnv"))
      } else {
        print(Out_list)
        warning(helpful_message)
      }
    }
  }

  # Method 1 : all_equal ------------------
  # Check for exact equality between two objects with stringent stop behavior
  check_all_equal <- function(X, Y, colsX){

    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX)
    Y <- prep_dataY(Y = Y, colsY = colsY)

    message("Method is 'all_equal' ")
    # TODO experiment with trycatch here
    if (isTRUE(all.equal(X,Y))) { # 1.1 first check equality (requires isTRUE wrapper - does not return FALSE otherwise)
      Out_list <- message("Both dataframes are equal!") # if both equal, end here

    } else { # 1.2 if not all_equal, print verbose output, and STOP
      Out_list <- list(
        "all_equal" <- all.equal(X,Y),
        "in_X_not_Y" <- setdiff(X[,colsX],Y[,colsX]),
        "in_Y_not_X" <- setdiff(Y[,colsX],X[,colsX]),
        "cols_X_not_Y" = setdiff(names(X), names(Y)),
        "cols_Y_not_X" = setdiff(names(Y), names(X))
      )

      stop_or_continue(STOP = STOP, method = method, Out_list = Out_list,
                       stop_condition = !isTRUE(all.equal(X,Y)), # must be added for each method
                       helpful_message ="Dataframes are not all equal, see above.")
    }
  }

  # Method 2 : hier2data ----------------------------
  # Broad comparison of a hierarchy to a dataset across many dimensions
  # Stops if
  check_hier2data <- function(X, Y, colsX){

    # Prepare data for check functions
    X <- prep_dataX(X = X, colsX = colsX)
    Y <- prep_dataY(Y = Y, colsY = colsY)

    message("Method is 'hier2data' ")
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
                     stop_condition = nrow(setdiff(distinct(X), disctinct(Y)))>0,
                     helpful_message = "There is a difference between data.frames")

    return(Out_list)
  }

  # Method 3 : compare_locs ----------------------------
  # Compare location_ids and path_to_top_parent

  # Method 4 : compare_cols ----------------------------
  # Look for only misaligned column names

  # call different check functions depending on method required
  switch (method,
          "all_equal" = check_all_equal(X = X, Y = Y, colsX = colsX),
          "hier2data" = check_hier2data(X = X, Y = Y, colsX = colsX)
  )

}


# SANDBOX -----------------

# Checks ------------

# PRE-WORKFLOW

# early column mismatch
preflight_checks(X = Diff2,
                 Y = Diff1,
                 colsX = c("location_id", "location_name"))

# all_equal
# naive, no method specified
preflight_checks(Equal1, Equal2)
preflight_checks(Equal1, Equal2, method = "all_equal")
preflight_checks(Diff1, Diff2, method = "all_equal", STOP = T)

# hier2data
X <- Diff1
Y <- Diff2
colsX <- c("location_id", "path_to_top_parent")


preflight_checks(Diff1, Diff2, method = "hier2data", colsX = "location_id")
preflight_checks(Diff1, Diff2, method = "hier2data", colsX = c("location_id", "path_to_top_parent"))

preflight_checks(Equal1, Equal2, method = "hier2data", colsX = c("location_id", "path_to_top_parent"))





# --- test some things ---

# lots of output, maybe too much
all.equal(HIER$covariate_with_aggregates_hierarchy$location_id, HIER$gbd_analysis_hierarchy$location_id)
# output if equal?
all.equal(equal1, equal2) #TRUE

setdiff(names(HIER$covariate_with_aggregates_hierarchy), names(HIER$gbd_analysis_hierarchy))
setdiff(names(HIER$gbd_analysis_hierarchy), names(HIER$covariate_with_aggregates_hierarchy))
setdiff(names(h_covid), names(h_gbd))
setdiff(names(h_gbd), names(h_covid))
