# started: 2025 Sep 05
#
# purpose: suite of functions to attach location_id, ihme_loc_id, and
# parent_location_id in different contexts. Since these perform column ADDITION,

#' Use `ihme_loc_id` to merge on `location_id` from a location table
#'
#' User control of variable names for `location_id` and `ihme_loc_id` in case
#' they differ from the defaults.
#'
#' Designed to gracefully move on if `location_id` column is already present
#' (since this is a standard column name).
#'
#' @param df [data.frame] of data with `ihme_loc_id` column
#' @param hierarchy [data.frame] location table with `ihme_loc_id` and `location_id` columns
#' @param loc_id_varname [chr: default "location_id"] variable name in `df` for `location_id`
#' @param ihme_loc_varname [chr: default "ihme_loc_id"] variable name in `df` for `ihme_loc_id`
#'
#' @returns [data.frame] input `df` with `location_id` column added if not already present
#' @export
attach_location_id <- function(
    df
    , hierarchy
    , loc_id_varname   = "location_id"
    , ihme_loc_varname = "ihme_loc_id"
){
  checkmate::assert_data_frame(df)
  checkmate::assert_data_frame(hierarchy)
  checkmate::assert_choice(ihme_loc_varname, names(df))
  # order matters - be careful
  vars_req <- c('location_id', 'ihme_loc_id')
  vars_new <- c(loc_id_varname, ihme_loc_varname)
  checkmate::assert_subset(x = vars_req, choices = names(hierarchy))

  hierarchy <- subset(hierarchy, select = vars_req) # safe for data.frame or data.table
  names(hierarchy) <- vars_new

  if(!loc_id_varname %in% names(df)) {
    df <- merge(df, hierarchy, by = ihme_loc_varname, all.x = TRUE)
  }
  return(df[])
}

#' Use `location_id` to merge on `ihme_loc_id` from a location table.
#'
#' User control of variable names for `location_id` and `ihme_loc_id` in case
#' they differ from the defaults.
#'
#' Designed to gracefully move on if `ihme_loc_id` column is already present
#' (since this is a standard column name).
#'
#' @param df [data.frame] of data with `location_id` column
#' @param hierarchy [data.frame] location table with `ihme_loc_id` and
#'   `location_id` columns
#' @param loc_id_varname [chr: default "location_id"] variable name in `df` for
#'   `location_id`
#' @param ihme_loc_varname [chr: default "ihme_loc_id"] variable name in `df`
#'   for `ihme_loc_id`
#'
#' @returns [data.frame] input `df` with `ihme_loc_id` column added if not
#'   already present
#' @export
attach_ihme_loc_id <- function(
    df
    , hierarchy
    , loc_id_varname   = "location_id"
    , ihme_loc_varname = "ihme_loc_id"
){
  checkmate::assert_data_frame(df)
  checkmate::assert_data_frame(hierarchy)
  checkmate::assert_choice(loc_id_varname, names(df))
  # order matters - be careful
  vars_req <- c('location_id', 'ihme_loc_id')
  vars_new <- c(loc_id_varname, ihme_loc_varname)
  checkmate::assert_subset(x = vars_req, choices = names(hierarchy))

  hierarchy <- subset(hierarchy, select = vars_req) # safe for data.frame or data.table
  names(hierarchy) <- vars_new

  if (!ihme_loc_varname %in% names(df)) {
    df <- merge(df, hierarchy, by = loc_id_varname, all.x = TRUE)
  }
  return(df[])
}


#' Add `parent_location_id` column to a data.frame
#'
#' Uses `location_id` column in `df` and `hierarchy` table to find the parent
#' location_id at a given `parent_level` for each location_id in `df`.
#'
#' Designed to fail if `parent_location_id` column is already present (since
#' this is a non-standard column name).
#'
#' @param df [data.frame] some table with columns `location_id`
#' @param hierarchy [data.frame] ihme location hierarchy (get_location_metadata)
#' @param parent_level [int] single parent level for all location_ids in df
#' @param allow_self_as_parent [lgl: default FALSE] if TRUE, allow location_id
#'   at parent_level to have itself as parent_location_id
#' @param new_varname [string: default 'parent_location_id'] name of new column
#'   to create
#'
#' @returns [data.frame] with new 'parent_location_id' column
#' @export
attach_parent_location_id <- function(
    df
    , hierarchy
    , parent_level
    , allow_self_as_parent = FALSE
    , new_varname          = "parent_location_id"
){
  checkmate::assert_data_frame(df)
  checkmate::assert_data_frame(hierarchy)
  checkmate::assert_integerish(parent_level, len = 1)
  checkmate::assert_choice("location_id", names(df))
  # assert no names overlap df column names
  checkmate::assert_disjunct(new_varname, names(df))
  df <- add_new_column(
    x         = df
    , varname = new_varname
    , vec     = parents_of_children_vec(
      child_loc_id_vec     = df$location_id
      , hierarchy            = hierarchy
      , parent_level_vec     = parent_level
      , allow_self_as_parent = allow_self_as_parent
    )
  )
  return(df[])
}

#' Wrapper for attach_parent_location_id for national location_id (level 3)
#'
#' Attach `national_location_id` column to a data.frame using `location_id`.
#'
#' National location_id is defined as the parent location_id at level 3.
#'
#' Designed to fail if `national_location_id` column is already present (since
#' this is a non-standard column name).
#'
#' @param hierarchy [data.frame] ihme location hierarchy (get_location_metadata)
#' @param df [data.frame] some table with columns `location_id`
#' @param allow_self_as_parent [lgl: default TRUE] if TRUE, allow location_id at
#'   level 3 to have itself as national_location_id
#' @param new_varname [string: default 'national_location_id'] name of new
#'   column to create
#'
#' @returns [data.frame] with new 'parent_location_id' column
#' @export
attach_national_location_id <- function(
    df
    , hierarchy
    , allow_self_as_parent = TRUE
    , new_varname          = "national_location_id"
) {
  df <- attach_parent_location_id(
    df
    , hierarchy            = hierarchy
    , parent_level         = 3
    , allow_self_as_parent = allow_self_as_parent
    , new_varname          = new_varname
  )
  return(df[])
}

#' Attach `national_ihme_loc_id` column to a data.frame using `location_id`
#'
#' Wrapper for `attach_national_location_id` and `attach_ihme_loc_id`.
#'
#' Designed to fail if `national_ihme_loc_id` column is already present (since
#' this is a non-standard column name).
#'
#'
#' @param df [data.frame] some table with columns `location_id`
#' @param hierarchy [data.frame] ihme location hierarchy (get_location_metadata)
#' @param allow_self_as_parent [lgl: default TRUE] if TRUE, allow location_id at
#'   level 3 to have itself as national_location_id
#' @param new_varname [string: default 'national_ihme_loc_id'] name of new
#'   column to create (the national ihme_loc_id of the location_id column in
#'   df). Used for merging on ihme_loc_id, and user may decide to retain this
#'   column if desired.
#' @param keep_nat_loc_id [lgl: default FALSE] if TRUE, keep the intermediate
#'   national_location_id column
#'
#' @returns [data.frame] with new 'national_ihme_loc_id' column
#' @export
attach_national_ihme_loc_id <- function(
    df
    , hierarchy
    , allow_self_as_parent = TRUE
    , new_varname          = "national_ihme_loc_id"
    , var_nat_loc_id       = "national_location_id"
    , keep_nat_loc_id      = FALSE
){

  checkmate::assert_disjunct(new_varname, names(df))

  df <- attach_national_location_id(
    df
    , hierarchy            = hierarchy
    , allow_self_as_parent = allow_self_as_parent
    , new_varname          = var_nat_loc_id
  )

  df <- attach_ihme_loc_id(
    df
    , hierarchy        = hierarchy
    , loc_id_varname   = var_nat_loc_id
    , ihme_loc_varname = new_varname
  )

  if(keep_nat_loc_id == FALSE) df <- drop_column(df, var_nat_loc_id)

  return(df[])
}
