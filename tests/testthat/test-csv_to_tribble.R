if (FALSE) {
  objname  <- "DT"
  fname    <- "agg_data.csv"
  fname    <- "agg_hier.csv"
  csv_path <- file.path(sprintf("tests/testthat/fixtures/%s", fname))
}

test_that("csv_to_tribble works",
          {
            objname <- "DT"

            # File with only data
            fname    <- "agg_data.csv"
            csv_path <- file.path(sprintf("fixtures/%s", fname))
            objname  <- "DT"
            outpath  <- file.path(tempdir(), sprintf("%s.R", objname))
            csv_to_tribble(
              csv_input_path  = csv_path
              , output_R_path = outpath
              , obj_name      = objname
              , align_commas  = TRUE
              , make_backup   = TRUE
            )

            # interactive inspection
            # rstudioapi::documentOpen(outpath)
            # rstudioapi::documentOpen(paste0(outpath, ".bak"))

            # Read in both temp files and compare to original
            original <- data.table::fread(csv_path)
            source(outpath)
            DT <- data.table::as.data.table(DT)
            expect_equal(original, DT)
            rm(DT)
            source(paste0(outpath, ".bak"))
            DT <- data.table::as.data.table(DT)
            expect_equal(original, DT)

            rm(original, DT)

            # File with quoted comma-separated strings ("1,2,3")
            fname    <- "agg_hier.csv"
            csv_path <- file.path(sprintf("fixtures/%s", fname))
            outpath  <- file.path(tempdir(), sprintf("%s.R", objname))
            csv_to_tribble(
              csv_input_path  = csv_path
              , output_R_path = outpath
              , obj_name      = objname
              , align_commas  = TRUE
              , make_backup   = TRUE
            )

            # interactive inspection
            # rstudioapi::documentOpen(outpath)
            # rstudioapi::documentOpen(paste0(outpath, ".bak"))

            # Read in both temp files and compare to original
            original <- data.table::fread(csv_path)
            source(outpath)
            DT <- data.table::as.data.table(DT)
            msg_multiline(head(original))
            msg_multiline(head(DT))
            expect_equal(original, DT)
            rm(DT)
            source(paste0(outpath, ".bak"))
            DT <- data.table::as.data.table(DT)
            expect_equal(original, DT)
          })
