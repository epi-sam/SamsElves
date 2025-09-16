source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
age_metadata <- get_age_metadata(release_id = 34)
age_metadata <- age_metadata |>
  dplyr::arrange(age_group_years_start) |>
  dplyr::select(age_group_id, age_group_name) |>
  dplyr::mutate(
    age_group_name = factor(age_group_name, levels = age_group_name)
  )

sex_metadata <- data.frame(
  sex_id = 1:3,
  sex_name = c('Male', 'Female', 'Both')
)

source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

location_metadata <- get_location_metadata(location_set_id = 35, release_id = 34)
location_metadata <- location_metadata[,.(location_id, ihme_loc_id, location_name, super_region_name)]

usethis::use_data(
  age_metadata, sex_metadata, location_metadata,
  internal = TRUE,
  overwrite = TRUE
)
