if(FALSE){ # for debugging - tests use a different folder structure
  library(data.table)
  source("tests/testthat/fixtures/DT_draws.R")
}
source("/fixtures/DT_draws.R")

# FIXME SB - 2025 Sep 29 - This is very fragile - if you don't have the default
# facet/page columns, you drop them from the DT.  Also, the scatters aren't
# returned, they're just ggarranged, making this function difficult to test in
# the current form.
# These should all allow NULL inputs if not relevant to data/faceting
# - by_page_var       = 'year_id'
# - facet_var         = 'age_group_id'
# - color_scatter_var = 'location_id'

# FIXME SB - 2025 Sep 29 - WARNING
# Warning message:
#   In `[.data.table`(DT, , `:=`(mean, base::rowMeans(.SD)), .SDcols = vars_draws) :
#   A shallow copy of this data.table was taken...
DT_summary <- get_draw_pe_ui_difference(DT)
# FIXME SB - both required, even if not used in plots (one dimension only)
# - this should allow NULL inputs
DT_summary[, age_group_id := NULL]
DT_summary[, sex_id := NULL]

test_that("plot_draw_pe_distribution works",
          {

            p_list <- plot_draw_pe_distribution(
              DT         = DT_summary
              , make_pdf = FALSE
              , verbose = TRUE

              # FIXME SB - 2025 Sep 29 - shouldn't require these if not used
              # Error in switch(var_name, location_id = "super_region_name", age_group_id = "age_group_name",  :
              # EXPR must be a length 1 vector
              # , facet_var = NULL

              # Assertion on 'facet_var' failed: Must be a subset of {'age_group_id','sex_id','location_id','year_id'}, but has additional elements {''}.
              # , facet_var = ''
            )

          })
