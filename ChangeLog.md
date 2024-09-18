ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

## 2024-09-18

- updated
- submit_job
- parse_all_named_cli_args
  - Now accepts atomic vectors for CLI arguments, converts to a comma-separated string, and then converts back to a vector of the correct type when parsed (behavior allowed by default, but can be turned off).
build_metadata_shell
- build_metadata_shell
  - switched from sacct to squeue for performance
  - now includes a selection of sessionInfo for R version, package versions, etc. for pipeline provenance


## 2024-09-06

- added:
  - utils
- refined 
  - submit_job
  - submit_job_array
    - added console-style log option (combine stderr and stdout)

## 2023-12-04

- deprecated: 
  - define_roots
  - detach_user_packages
  - get_version_from_path
  - preflight_checks
    
- added:
  - **test suite**
  - is_x
  - make_versioned_dir
  - prep_path
  - submit_job
  - submit_job_array
  - utils_io
  - wait_on_jobs


## 2022-04-15

- added
  - preflight_checks
  - children_of_parents
