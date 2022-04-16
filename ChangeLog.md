ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

2022-04-15

# preflight_checks
- convert the following
  - deprecate:
    - all_equal
    - filter statement
  - rename
    - 'compare_cols' -> 'col_names'
    'ERRORS' to "PREFLIGHT_CHECKS_ERRORS"
  - add
    - 'vec2vec' method (generalized vectors - first method)
  - double check - where is this already used?
    - all_plots for sure 
    - any others?
    
  - cleaned up:
    - stop_or_continue behavior
    - output (now using dplyr::setdiff and others)
    - output warning messages
    - reordered output messages to appear in one block, below printed Output_list
