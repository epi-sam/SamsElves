# SamsElves 0.3.24

- assert_square - better verbosity control

# SamsElves 0.3.23

- wait_on_slurm_job_id - new option to break on timeout
- increment_file_version - bugfix to file extension finder

# SamsElves 0.3.22

2025-07-11

- wait_on_slurm_job_id.R
  - now breaks on failure by default

# SamsElves 0.3.21

2025-07-03

- Bugfixes to assert_square
  - fixed not stopping if hard_stop = TRUE due to on.exit behavior
  
# SamsElves 0.3.20

2025-07-01 

- Bugfixes to assert_square
  - always return list (required in production)
  - assert_no_na option
  - arg to stop if DT has no rows (empty)


# 2025-05-05 v.0.3.19

- feature:
  - `build_metadata_shell` now tries to find the script_path used to call it, or return NA
    - tries with both interactive and submitted scripts
    - submitted scritps require a `script_path` object in .GlobalEnv (read from the CLI), which is now provided by `submit_job` by default

# 2025-03-09 v0.3.18

- feature:
  - parse_all_named_cli_args allows unlock/lockBinding for variables that may already be locked at time of parsing


# 2025-03-09 v0.3.17

- bugfix:
  - save_file csv_writer rewrite - default now only quotes necessary strings (those withd delimeters)


# 2025-02-21 v0.3.16

- feature:
  - save_file now has csv_opt argument and new default (readr::write_excel_csv) to preserve diacritics

- bugfix:
  - assert_square has option for 0-row data.table, which is used by aggregate_parents_from_children


# 2025-02-21 v0.3.15

- improved:
  - replaced sys.getenv calls with better cross-platform sys.info


# 2024-10-22 v0.3.14

- improved:
  - build_metadata_shell finds number of cores using 'nproc'



# 2024-10-22 v0.3.13

- bugfix:
  - aggregate_from_children_to_parents 
    - assert complete cases


# 2024-10-22 v0.3.12

- added:
  - aggregate_from_children_to_parents and tests
    - roll up values from a data set according to a hierarchy



# 2024-10-22 v0.3.11

- behavior change:
  - save_file overwrites by default - this is normal save function behavior
    - forbid_overwrite argument is in sunset period



# 2024-10-22 v0.3.10

- documentation updates:
  - aggregate_parents_from_children and supporting assertions



# 2024-10-22 v0.3.9

- bugfix:
  - aggregate_parents_from_children and supporting assertions



# 2024-10-22 v0.3.8

- added:
  - get_new_output_dv



# 2024-10-22 v0.3.7

- added:
  - aggregate_parents_from_children and supporting assertions



# 2024-10-22 v0.3.6

- bugfixes:
  - submit_job and parse_all_named_cli_args - fine-tuned NULL and NA value handling
  - messaging system - more internally consistent implementation



# 2024-10-22 v0.3.5

- bugfixes:
  - apply_comma_string_to_list - failed for NULL items
  - parse_all_named_cli_args - new options to assign NA, NaN and NULL types (default TRUE)
  - wait_on_slurm_job_ids - improved messaging



# 2024-09-30 v0.3.4

- added:
  - utils_io.R from ihme.covid package
    - `make_new_output_dir` - create a version-incremented run-date folder based on a 'YYYY_MM_DD.VV' run-date folder structure
      - supported by `get_latest_output_date_index` and `get_new_output_dir`
    - `get_latest_output_dir` - get the latest output directory based on a 'YYYY_MM_DD.VV' run-date folder structure
    - added tests, updated deprecated methods
- documented:
  - some previously undocumented helper functions for various methods (not exported)



# 2024-09-23 v0.3.3

- updated:
  - `build_metadata_shell`
    - now trims sessionInfo for `otherPkgs` as well as `loadedOnly` to reduce metadata bloat.
- added:
  - `increment_file_version`
    - adds a `_v1`/`_v2`/etc. to the end of a file name to increment the version number
    - retains original file extension



# 2024-09-19 v0.3.2

- updated:
  - `read_file`
    - now includes option for custom csv reading function since `data.table::fread` can have quotation-doubling issues
    - also includes `...` arg to pass additional user-desired args to the reader function (works for any underlying reader function)



# 2024-09-18

- deprecated:
  - `submit_job_array` (functionality now handled by submit_job natively)
- updated:
  - `submit_job`
  - parse_all_named_cli_args
    - Now accepts atomic vectors for CLI arguments, converts to a comma-separated string, and then converts back to a vector of the correct type when parsed (behavior allowed by default, but can be turned off).
    - option to send email to user when job completes
  - `build_metadata_shell`
    - switched from sacct to squeue for performance
    - now includes a selection of sessionInfo for R version, package versions, etc. for pipeline provenance



# 2024-09-06

- added:
  - utils
- refined:
  - `submit_job` & `submit_job_array`
    - added console-style log option (combine stderr and stdout)



# 2023-12-04

- deprecated: 
  - `define_roots`
  - `detach_user_packages`
  - `get_version_from_path`
  - `preflight_checks`
    
- added:
  - **test suite**
  - is_x
  - `make_versioned_dir`
  - `prep_path`
  - `submit_job`
  - `submit_job_array`
  - utils_io
  - `wait_on_jobs`


# 2022-04-15

- added
  - `preflight_checks`
  - `children_of_parents`
