# .Rprofile must add NCH library to .libPaths for this to work

release_id_current <- 34

# FIXME SB - 2025 Sep 29 - merges are losing age groups as well, dropping rows from DT
# - need to get all age groups from this call

plot_age_metadata <- ihme::get_age_metadata(release_id = release_id_current)
plot_age_metadata <- plot_age_metadata |>
  dplyr::arrange(age_group_years_start) |>
  dplyr::select(age_group_id, age_group_name) |>
  dplyr::mutate(
    age_group_name = factor(age_group_name, levels = age_group_name)
  )

plot_sex_metadata <- data.frame(
  sex_id = 1:3
  , sex_name = c('Male', 'Female', 'Both')
)

# Neither location set is a full envelope for the other - combine for all
# possible locations.
location_metadata_35   <- ihme::get_location_metadata(
  location_set_id = 35
  , release_id    = release_id_current
)
location_metadata_22   <- ihme::get_location_metadata(
  location_set_id = 22
  , release_id    = release_id_current
)
plot_location_metadata <- rbind(location_metadata_35, location_metadata_22) |>
  dplyr::select(location_id, ihme_loc_id, location_name, super_region_name) |>
  unique()

internal_metadata <- list(
  age        = plot_age_metadata
  , sex      = plot_sex_metadata
  , location = plot_location_metadata
)

usethis::use_data(
  internal_metadata
  , internal  = TRUE
  , overwrite = TRUE
)
