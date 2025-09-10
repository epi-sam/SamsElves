test_that("map_iso_to_ihme_loc_id works",
          {
            dt <- data.table::data.table(
              ihme_loc_id = c("FRA", "USA", "GLP", "CAN")
              , value = 1:4
            )
            dt_mapped <- map_iso_to_ihme_loc_id(
              data.table::copy(dt)
              , loc_subsets = c("France")
              , iso_varname = "ihme_loc_id"
              , direction = "ltr"
              )
            expect_equal(
              dt_mapped,
              structure(
                list(
                  ihme_loc_id = c("FRA_97896", "USA", "FRA_350", "CAN")
                  , value = 1:4
                )
                , class = c("data.table", "data.frame")
              )
            )
          })
