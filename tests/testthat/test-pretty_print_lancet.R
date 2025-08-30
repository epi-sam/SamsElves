
# Unit tests ----

test_that("mid_dot works",
          {
            expect_equal(mid_dot(), "·")
          })
test_that("en_dash works",
          {
            expect_equal(en_dash(), "–")
          })
test_that("thin_space works",
          {
            expect_equal(thin_space(), " ")
          })

test_that("fround works",
          {
            expect_equal(fround(0.123456789), "0·1")
            expect_equal(fround(0.123456789, digits = 3), "0·123")
            expect_equal(fround(0.123456789, digits = 3, nsmall = 4), "0·1230")
          })

test_that("fround_dtype works",
          {
            expect_equal(fround_dtype(0.123456789), "0·1%")
            expect_equal(fround_dtype(0.123456789, d_type = "pp"), "0·1 pp")
            expect_equal(fround_dtype(0.123456789, d_type = "count", digits = 3, nsmall = 4), "0·1230")
          })


test_that("set_magnitude works",
          {
            expect_equal(
              set_magnitude(c(1, 1e3, 1e6, 1e9, 1e12, -1e6))
              , list(
                mag         = c("", "", "M", "B", "B", "M")
                , mag_label = c("", "", "million ", "billion ", "billion ", "million ")
                , denom     = c(1, 1, 1e+06, 1e+09, 1e+09, 1e+06)
              )
            )
          })



test_that("fmt_magnitude works",
          {
            #' fmt_magnitude(123456789) # "123.5 million"
            expect_equal(fmt_magnitude(123456789), "123.5 million")
          })

test_that("fround_mag_clu works",
          {
            expect_equal(fround_mag_clu(clu = c(central = 0.2, lower = 0.1, upper = 0.3), d_type = "prop"), c(central = "0·2", lower = "0·1", upper = "0·3"))
            expect_equal(fround_mag_clu(clu = c(central = 0.2, lower = -0.1, upper = 0.3), d_type = "prop"), c(central = "0·2", lower = "–0·1", upper = "0·3"))
            expect_equal(fround_mag_clu(clu = c(central = 95e6, lower = 89e6, upper = 101e6), d_type = "count"), c(central = "95·0", lower = "89·0", upper = "101"))
            expect_equal(fround_mag_clu(clu = c(central = 95e6, lower = 96e6, upper = 97e6), d_type = "count"), c(central = "95·0", lower = "96·0", upper = "97·0"))
          })

test_that("fround_mag_clu isn't great for small decimal/integer count data sets - this should get fixed at some point",
          {
            expect_equal(fround_mag_clu(clu = c(central = 1, lower = 0.2, upper = 2), d_type = "count"), c(central = "1", lower = "0·2", upper = "2"))
          })

test_that("format_lancet_clu works",
          {
            expect_equal(format_lancet_clu(central = 0.994, lower = 0.984, upper = 0.998, d_type = "prop")
                         , "99·4% (98·4–99·8)")
            expect_equal(format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
                         , c("99·4% (98·4–99·8)", "99·4% (98·4–99·8)"))
            expect_equal(format_lancet_clu(central = c(0.994, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "prop")
                         , c("99·4% (–15·0 to 99·8)", "99·4% (98·4–99·8)"))
            expect_equal(format_lancet_clu(central = c(-0.05, 0.994), lower = c(-0.15, 0.984), upper = c(0.998, 0.998), d_type = "pp")
                         , c("a decrease of 5·0 pp (–15·0 to 99·8)", "99·4 pp (98·4–99·8)"))
            expect_equal(format_lancet_clu(central = rep(2e6, 2), lower = rep(.5e6, 2), upper = rep(3e6, 2), d_type = "count")
                         , c("2·00 million (0·50–3·00)", "2·00 million (0·50–3·00)"))
            expect_equal(format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4)
                         , c("a decrease of 99·4% (98·4–99·8)", "a decrease of 99·4% (98·4–99·8)"))
            expect_equal(format_lancet_clu(central = c(-0.994, -0.994), upper = c(-0.984, -0.984), lower = c(-0.998, -0.998), d_type = "prop",  digits_round_prop = 4, UI_only = TRUE)
                         , c("98·4–99·8", "98·4–99·8"))
          })


# Integration tests -----

test_that("format_lanced_clu errors correctly", {
  expect_error(
    format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998), d_type = "prop")
    , "Assertion on 'length\\(clu_lengths\\) == 1' failed: Must be TRUE."
  )
  expect_error(
    format_lancet_clu(central = c(0.994, 0.999), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "prop")
    , "upper is less than/equal to central at index: 2 : \\(0.998 < 0.999\\)"
  )
  expect_error(
    format_lancet_clu(central = c(0.994, 0.994), lower = c(0.984, 0.984), upper = c(0.998, 0.998), d_type = "propeller")
    , "Assertion on 'd_type' failed: Must be element of set \\{'prop','pp','count'\\}, but is 'propeller'."
  )

})

test_that("format_lancet_dt works",
          {
            DT_count <- data.table::data.table(
              location_did    = rep(1, 2)
              , location_name = rep("Global", 2)
              , me_name       = c("vacc_dpt1", "vacc_dpt3")
              , mean          = c(55.8e6, 54.7e6)
              , lower         = c(50.7e6, 48.6e6)
              , upper         = c(60.7e6, 59.6e6)
            )

            expect_equal(
              format_lancet_dt(dt = DT_count, d_type = "count", central_var = 'mean')
              , structure(
                list(
                  location_did = c(1, 1),
                  location_name = c("Global", "Global"),
                  me_name = c("vacc_dpt1", "vacc_dpt3"),
                  clu_fmt = c("55·8 million (50·7–60·7)", "54·7 million (48·6–59·6)")
                ),
                row.names = c(NA, -2L),
                class = c("data.table", "data.frame")
              )
            )

            DT_prop <- data.table::data.table(
              location_did    = rep(1, 3)
              , location_name = rep("Global", 3)
              , me_name       = c("vacc_dpt1", "vacc_dpt3", "wacky_fun")
              , mean          = c(.558, .547, -0.1)
              , lower         = c(.507, .486, -0.25)
              , upper         = c(.607, .596, 1.3)
            )

            expect_equal(
              format_lancet_dt(dt = DT_prop, d_type = "prop", central_var = 'mean')
              ,
              structure(
                list(
                  location_did = c(1, 1, 1),
                  location_name = c("Global", "Global", "Global"),
                  me_name = c("vacc_dpt1", "vacc_dpt3", "wacky_fun"),
                  clu_fmt = c("55·8% (50·7–60·7)", "54·7% (48·6–59·6)", "a decrease of 10·0% (–25·0 to 130·0)")
                ),
                row.names = c(NA, -3L),
                class = c("data.table", "data.frame")
              )
            )
          })
