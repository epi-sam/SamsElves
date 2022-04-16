ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

2022-04-15

# preflight_checks
- [ ] convert the following
  - [x] deprecate:
    - [x] filter statement
  - [x] rename
    - [x] 'compare_cols' -> 'col_names'
    - [x] 'ERRORS' to "PREFLIGHT_CHECKS_ERRORS"
  - [ ] add
    - [x] 'vec2vec' method (generalized vectors; first method)
      - [x] add to switch
      - [x] test it out
      - [x] document
      - [ ] reorder MREs so this comes first, gives a clear example
    - [ ] 'anyNAvec'
      - [ ] add to switch
      - [ ] add to validation pre-check
      - [ ] test it out
      - [ ] document
    - [ ] 'allNAvec'
      - [ ] add to switch
      - [ ] add to validation pre-check
      - [ ] test it out
      - [ ] document
    - [ ] 'anyNAdata' - many cols
      - [ ] add to switch
      - [ ] add to validation pre-check
      - [ ] test it out
      - [ ] document
    - [ ] 'allNAdata' - many cols
      - [ ] add to switch
      - [ ] add to validation pre-check
      - [ ] test it out
      - [ ] document
  - [ ] double check; where is preflight_checks already used?
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
      - [ ] vec2vec MREs (replace all_equal)
      - [ ] hier2data MREs
