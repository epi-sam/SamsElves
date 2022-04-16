ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

2022-04-15

# preflight_checks
- [ ] convert the following
  - [ ] deprecate:
    - [ ] filter statement
  - [ ] rename
    - [ ] 'compare_cols' -> 'col_names'
    - [x] 'ERRORS' to "PREFLIGHT_CHECKS_ERRORS"
  - [ ] add
    - [ ] 'vec2vec' method (generalized vectors - [ ] first method)
  - [ ] double check; where is this already used?
    - [ ] all_plots for sure 
    - [ ] any others?
    
  - [ ] clean up:
    - [x] stop_or_continue behavior
    - [x] output (now using dplyr::setdiff and others)
    - [x] output warning messages
      - [x] both inline messages and end-of-run warnings
      - [x] improved formatting, only one message per function call
    - [x] improved print/return to console behavior
    - [x] reordered output messages to appear in one block, below printed Output_list
    
    - [ ] Documentation
      - [ ] hier2data MREs
