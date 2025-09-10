#' Map ISO3 codes for France level 3 (Metropolitan + overseas territories)
#'
#' @returns [chr] named vector mapping certain `ihme_loc_id` values to their ISO3 codes
#' @export
#'
#' @examples
#' iso_map_France_level_3()
iso_map_France_level_3 <- function(){
  # Metropolitan France and overseas territories
  iso_map <- c(
    FRA = "FRA_97896"
    , GUF = "FRA_338"
    , GLP = "FRA_350"
    , MTQ = "FRA_363"
    , MYT = "FRA_364"
    , REU = "FRA_387"
  )
  return(iso_map)
}

#' Map certain `ihme_loc_id` values to their ISO3 codes
#'
#' User control of variable name for `ihme_loc_id` in case it differs from the
#' default.
#'
#' Started to handle Metropolitan France + overseas DROMCOM territories, may
#' expand to other location subsets later.
#'
#' @param dt [data.table] of data with `ihme_loc_id` column
#' @param iso_varname [chr: default "ihme_loc_id"] variable name in `dt` for
#'   `ihme_loc_id`
#' @param loc_subsets [chr or NULL: default NULL] subsets of mappings to apply,
#'   NULL means all
#' @param subset_exact [chr or NULL: default NULL] if not NULL, only apply
#'   mappings for these exact `ihme_loc_id` or `ISO3` values
#' @param direction [chr: default "ltr"] direction of mapping, "ltr" means
#'   `ihme_loc_id` to `ISO3`, "rtl" means `ISO3` to `ihme_loc_id`
#'
#' @returns [data.table] input `dt` with `ihme_loc_id` values replaced by ISO3
#'   codes where applicable
#' @export
#'
#' @examples
#' dt <- data.table::data.table(ihme_loc_id = c("FRA", "USA", "GLP", "CAN"), value = 1:4)
#' dt_mapped <- map_iso_to_ihme_loc_id(data.table::copy(dt), loc_subsets = c("France"))
#' print(dt)
#' print(dt_mapped)
map_iso_to_ihme_loc_id <- function(
    dt
    , iso_varname = "ihme_loc_id"
    , loc_subsets = NULL
    , subset_exact = NULL
    , direction = "ltr"
){
  checkmate::assert_data_table(dt)
  checkmate::assert_choice(iso_varname, names(dt))
  checkmate::assert_character(iso_varname, len = 1)
  checkmate::assert_character(loc_subsets, null.ok = TRUE, min.len = 1)
  checkmate::assert_choice(direction, c("ltr", "rtl"))

  valid_subsets <- c(
    "France"
  )

  if(is.null(loc_subsets)) loc_subsets <- valid_subsets
  checkmate::assert_subset(loc_subsets, valid_subsets)

  iso_map <- c()

  # Build up iso_map in sets (for readability and control)
  if("France" %in% loc_subsets){
    # French overseas territories
    iso_map <- c(
      iso_map
      , iso_map_France_level_3()
    )
  }

  if(!is.null(subset_exact)){
    checkmate::assert_character(subset_exact, any.missing = FALSE, min.len = 1)
    iso_map <- iso_map[iso_map %in% subset_exact]
  }

  if(direction == "rtl"){
    iso_rev <- setNames(names(iso_map), iso_map)
    iso_map <- iso_rev
  }

  # Do the remapping
  message(sprintf("Replacing %s values with ISO3 codes: ", iso_varname))
  for (idx in seq_along(iso_map)){
    .x <- names(iso_map)[idx]
    .y <- iso_map[idx]
    if(.x %in% dt[[iso_varname]]){
      message(" -- mapping ", .x, " to ", .y)
      dt[get(iso_varname) == .x, (iso_varname) := .y]
    }
  }

  return(dt[])
}
