# TODOs -------------

# - [ ] Allow for filtering as well as column selection

# SANDBOX -----------------

# --- prep environment ---
rm(list = ls(all.names = T))
source("R/env_setup_for_package_dev.R")

# --- test some things ---

# lots of output, maybe too much
all.equal(HIER$covariate_with_aggregates_hierarchy$location_id, HIER$gbd_analysis_hierarchy$location_id)
# output if equal?
all.equal(equal1, equal2) #TRUE

setdiff(names(HIER$covariate_with_aggregates_hierarchy), names(HIER$gbd_analysis_hierarchy))
setdiff(names(HIER$gbd_analysis_hierarchy), names(HIER$covariate_with_aggregates_hierarchy))
setdiff(names(h_covid), names(h_gbd))
setdiff(names(h_gbd), names(h_covid))


# FUNCTION BODY ------
#' Title
#'
#' @param X Left-hand input vector or data.frame (depending on desired check type)
#' @param Y Right-hand input vector or data.frame (depending on desired check type)
#' @param type Type of comparison to be made:
#' all_equal = compare two vectors or data.frames for total equality, and receive verbose output if not
#' hier2data = compares a hierarchy to incoming raw_data or full_data, and checks for common UNIQUE values in columns between data.frames
#' @param verbose
#' @param colsX
#' @param colsY
#'
#' @return
#' @export
#'
#' @examples
preflight_checks <- function(
    X, # first dataframe
    Y, # second dataframe
    type = c("all_equal", "hier2data"), # 2 hierarchies, 2 datasets, 1 hierarchy & 1 dataset, etc.
    colsX = c("location_id", "location_name", "path_to_top_parent", "most_detailed"), # which columns to check from X?
    colsY = NULL, # which columns to check from Y? (if column names differ from X)
    verbose # get full output from all.equal
) {

  if (type == "all_equal"){ # 1. compare two vectors or data.frames for total equality, and receive verbose output if not

    print("Type is all_equal")

    if (isTRUE(all.equal(X,Y))) { # 1.1 first check equality (requires isTRUE wrapper - does not return FALSE otherwise)

      Out_list <- message("Both dataframes are equal!") # if both equal, end here

    } else { # 1.2 if not all_equal, print verbose output, and STOP
      Out_list <- all.equal(X,Y)
      stop("Dataframes are not all equal, see above.", call. = T)
    }

  } else if (type == "hier2data") { # 2. compares a hierarchy to incoming raw_data or full_data, and checks for common UNIQUE values in columns between data.frames

    print("Type is hier2data")
    print(colsX)
    print(colsY)

    # select columns from both data.frames according to X, unless column names for Y are specified explicitly
    X <- X %>% select(all_of(colsX))
    if (is.null(colsY)){
      Y <- Y %>% select(all_of(colsX))
    } else {
      Y <- Y %>% select(all_of(colsY))
    }

    message("There are set differences - check output list!")

    Out_list <- list(
      "X_not_Y" = setdiff(X,Y),
      "Y_not_X" = setdiff(Y,X)
    )

  }

  return(Out_list)

}



# Checks ------------

# all_equal
# naive, no type specified
preflight_checks(Equal1, Equal2)

preflight_checks(Equal1, Equal2, type = "all_equal")

# hier2data
X <- Diff1
Y <- Diff2
colsX <- c("location_id", "path_to_top_parent")


preflight_checks(Diff1, Diff2, type = "hier2data", colsX = "location_id")
preflight_checks(Diff1, Diff2, type = "hier2data", colsX = c("location_id", "path_to_top_parent"))

preflight_checks(Equal1, Equal2, type = "hier2data", colsX = c("location_id", "path_to_top_parent"))
