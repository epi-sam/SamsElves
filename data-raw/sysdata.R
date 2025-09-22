plot_age_metadata <- ihme::get_age_metadata(release_id = 34)
plot_age_metadata <- plot_age_metadata |>
  dplyr::arrange(age_group_years_start) |>
  dplyr::select(age_group_id, age_group_name) |>
  dplyr::mutate(
    age_group_name = factor(age_group_name, levels = age_group_name)
  )

plot_sex_metadata <- data.frame(
  sex_id = 1:3,
  sex_name = c('Male', 'Female', 'Both')
)

location_metadata_35 <- ihme::get_location_metadata(location_set_id = 35, release_id = 34)
location_metadata_22 <- ihme::get_location_metadata(location_set_id = 22, release_id = 34)
plot_location_metadata <- rbind(location_metadata_35, location_metadata_22) |>
  dplyr::select(location_id, ihme_loc_id, location_name, super_region_name) |>
  unique()

usethis::use_data(
  plot_age_metadata, plot_sex_metadata, plot_location_metadata,
  internal = TRUE,
  overwrite = TRUE
)
