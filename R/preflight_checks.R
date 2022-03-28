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
preflght_checks <- function(
    X, # first dataframe
    Y, # second dataframe
    cols = c("location_id", "location_name", "path_to_top_parent", "most_detailed"), # which columns to check?
    type = c("2 hierarchy"), # 2 hierarchies, 2 datasets, 1 hierarchy & 1 dataset, etc.
    verbose # get full output from all.equal
) {

  X <- X %>% select(cols)
  Y <- Y %>% select(cols)

  if (all.equal(X,Y)) { # 1. first check equality

    message("Both dataframes are equal!") # if both equal, end here
  }

}
