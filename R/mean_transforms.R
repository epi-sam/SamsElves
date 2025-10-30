#' Yearly Mean Comparison
#'
#' Pivot a table of long means wider by selected years (2 or more).  Works for any 'central
#' tendency' variable (e.g., mean, median, point_estimate).
#'
#' @param DT [data.table] of long means with a 'year_id' column
#' @param yr_vec [integer] vector of years to compare (at least 2)
#' @param central_varname [string: default 'mean'] name of the central tendency
#'   variable column
#' @param id_varnams [chr: default `find_id_varnames()`] id variable names that
#'   describe the squareness of DT
#'
#'
#' @returns [data.table] input `DT` pivoted wider with mean columns for each
#'   year
#' @export
#'
#' @examples
#' DT <- data.table::data.table(
#'   location_id = rep(1:3, each = 4)
#'   , year_id    = rep(2000:2003, times = 3)
#'   , mean       = rnorm(12, mean = 100, sd = 10)
#' )
#' means_year_compare(
#'   DT = DT
#'   , yr_vec = c(2000,2003)
#'   , central_varname = "mean"
#'   , id_varnams = c("location_id", "year_id")
#' )
means_year_compare <- function(
    DT
    , yr_vec
    , central_varname = "mean"
    , id_varnams      = find_id_varnames(DT, verbose = FALSE)
) {

  checkmate::assert_data_table(DT)
  checkmate::assert_integerish(yr_vec, min.len = 2)
  assert_x_in_y(central_varname, colnames(DT))
  assert_x_in_y("year_id", colnames(DT))
  id_varnams <- setdiff(id_varnams, central_varname)
  assert_square(DT, id_varnams)

  names_prefix <- sprintf("%s_", central_varname)
  DTW <- data.table::as.data.table(
    tidyr::pivot_wider(
      data           = DT[year_id %in% yr_vec]
      , names_from   = "year_id"
      , values_from  = central_varname
      , names_prefix = names_prefix
    )
  )

  DTW[]
}

