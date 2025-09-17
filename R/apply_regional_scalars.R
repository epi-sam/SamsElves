#' Multiply data.table columns by GBD regional scalars
#'
#' Support function for aggregate_parents_from_children
#'
#' @param DT [data.table] with columns `location_id`, `year_id`, and variables
#'   to be aggregated
#' @param vars_to_multiply [chr] vector of variable names in `DT` to be
#'   multiplied by regional scalars
#' @param release_id [int] release_id for regional scalars
#' @param location_set_id [int] location_set_id for regional scalars
#'
#' @returns [data.table] input `DT` with `vars_to_multiply` multiplied by
#' regional scalars where matched by `location_id` and `year_id`
#' @export
apply_regional_scalars <- function(DT, vars_to_multiply, release_id, location_set_id){

  checkmate::assert_data_table(DT)
  if(!exists("get_regional_scalars", envir = .GlobalEnv)){
    stop("apply_regional_scalars requires user to source get_regional_scalars() from central functions")
  }
  checkmate::assert_subset(
    c(vars_to_multiply, "location_id", "year_id" )
    , choices = colnames(DT)
  )
  checkmate::assert_disjunct("regional_scalar", colnames(DT))
  checkmate::assert_integerish(release_id, len = 1, any.missing = FALSE)
  checkmate::assert_integerish(location_set_id, len = 1, any.missing = FALSE)

  year_ids <- min(DT$year_id):max(DT$year_id)
  message(sprintf("Reading regional scalars from database for release_id %s, location_set_id %s, year_ids %s"
                  , release_id, location_set_id, paste0(range(year_ids), collapse = "-")))

  regional_scalars <- get_regional_scalars(
    release_id        = release_id
    , location_set_id = location_set_id
    , year_id         = year_ids
  )

  DT <- merge(
    DT
    , regional_scalars[, .(location_id, year_id, regional_scalar = mean)]
    , by = c("location_id", "year_id")
    , all.x = TRUE
  )

  DT[is.na(regional_scalar), regional_scalar := 1]
  DT[, (vars_to_multiply) := lapply(.SD, function(x) x * regional_scalar), .SDcols = vars_to_multiply]
  DT[, regional_scalar := NULL]

  return(DT)
}
