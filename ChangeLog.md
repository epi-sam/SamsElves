ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

## 2024-09-30 v.0.3.4

- added:
  - utils_io.R
    - `make_new_output_dir` - create a version-incremented run-date folder, supported by `get_latest_output_dir` and `get_latest_output_date_index`
    - added tests, updated deprecated methods
- documented:
  - some previously undocumented helper functions for various methods (not exported)



## 2024-09-23 v.0.3.3

- updated:
  - `build_metadata_shell`
    - now trims sessionInfo for `otherPkgs` as well as `loadedOnly` to reduce metadata bloat.
- added:
  - `increment_file_version`
    - adds a `_v1`/`_v2`/etc. to the end of a file name to increment the version number
    - retains original file extension



## 2024-09-19 v.0.3.2

- updated:
  - `read_file`
    - now includes option for custom csv reading function since `data.table::fread` can have quotation-doubling issues
    - also includes `...` arg to pass additional user-desired args to the reader function (works for any underlying reader function)



## 2024-09-18

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



## 2024-09-06

- added:
  - utils
- refined:
  - `submit_job` & `submit_job_array`
    - added console-style log option (combine stderr and stdout)



## 2023-12-04

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


## 2022-04-15

- added
  - `preflight_checks`
  - `children_of_parents`
