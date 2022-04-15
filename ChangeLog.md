ChangeLog for SamsElves Package

--------------------------------------------------------------------------------

2022-04-15

# preflight_checks
- convert the following
  - deprecate:
    - hier2hier
    - hier2data
  - rename
    - 'data2data' -> 'DFs' (generalized dataframes -second method)
    - 'compare_cols' -> 'Columns'
  - remove
    - 'All_equal' as default method - have none (but keep the method as last option)
  - add
    - 'Vecs' method (generalized vectors - first method)
    
