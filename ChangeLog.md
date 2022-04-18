ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

2022-04-15

# preflight_checks
- [x] convert the following
  - [x] deprecate:
    - [x] filter statement
  - [x] rename
    - [x] 'compare_cols' -> 'col_names'
    - [x] 'ERRORS' to "PREFLIGHT_CHECKS_ERRORS"
  - [x] add
    - [x] 'vec2vec' method (generalized vectors; first method)
      - [x] add to switch
      - [x] test it out
      - [x] document
      - [x] reorder MREs so this comes first, gives a clear example
    - [x] 'anyNAvec'
      - [x] add to switch
      - [x] add to validation pre-check
      - [x] test it out
      - [x] document
    - [x] 'allNAvec'
      - [x] add to switch
      - [x] add to validation pre-check
      - [x] test it out
      - [x] document
    - [x] 'anyNAdata' - many cols
      - [x] add to switch
      - [x] add to validation pre-check
      - [x] test it out
      - [x] document
    - [x] 'allNAdata' - many cols
      - [x] add to switch
      - [x] add to validation pre-check
      - [x] test it out
      - [x] document
    - [x] 'all0vec'
    - [x] ' all0data
  - [ ] double check; where is preflight_checks already used?
    - [x] all_plots for sure 
    - [x] either keep functionality or replace
    
  - [x] clean up:
    - [x] stop_or_continue behavior
    - [x] output (now using dplyr::setdiff and others)
    - [x] output warning messages
      - [x] both inline messages and end-of-run warnings
      - [x] improved formatting, only one message per function call
    - [x] improved print/return to console behavior
    - [x] reordered output messages to appear in one block, below printed Output_list
    
    - [x] Documentation
      - [x] vec2vec MREs (replace all_equal)
      - [x] hier2data MREs
  - [x] Improve output
    - [x] scriptName to print the script that's running when sourced
    - [x] added user_message argument that prints when verbose = TRUE
    - [x] improve readability of available methods
    

# children_of_parents
- [ ] Add option to just print a vector of locations
  - [ ] Add switch
  - [ ] add new argument
  - [ ] allow "include_parent" argument to work for both options
  - [ ] add docstrings
  - [ ] add new MRE
  - [ ] test
