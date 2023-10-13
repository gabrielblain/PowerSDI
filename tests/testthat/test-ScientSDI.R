
# in most of these tests, `Good = "no"` because it's faster and is tested 1X

test_that("ScientSDI properly fetches and calculates values TS=1, distr=GEV",
          {
            vcr::use_cassette("ScientSDI_TS-1_Good-No_distr-GEV", {
              s_sdi <- ScientSDI(
                lon = -47.3,
                lat = -22.87,
                start.date = "1991-01-01",
                end.date = "2022-12-31",
                TS = 1,
                Good = "no"
              )
            })

            expect_type(s_sdi, "list")
            expect_length(s_sdi, 2)
            expect_named(s_sdi, c("SDI", "DistPar"))
          })

test_that("ScientSDI properly fetches and calculates values, TS=1, distr=GLO",
          {
            vcr::use_cassette("ScientSDI_TS-1_Good-No_distr-GLO", {
              s_sdi <- ScientSDI(
                lon = -47.3,
                lat = -22.87,
                start.date = "1991-01-01",
                end.date = "2022-12-31",
                TS = 1,
                distr = "GLO",
                Good = "no"
              )

              expect_type(s_sdi, "list")
              expect_length(s_sdi, 2)
              expect_named(s_sdi, c("SDI", "DistPar"))
            })
          })

test_that("ScientSDI properly fetches and calculates values, TS=4, distr=GLO",
          {
            vcr::use_cassette("ScientSDI_TS-4_Good-No_distr-GLO", {
              s_sdi <- ScientSDI(
                lon = -47.3,
                lat = -22.87,
                start.date = "1991-01-01",
                end.date = "2022-12-31",
                TS = 4,
                Good = "no"
              )

              expect_type(s_sdi, "list")
              expect_length(s_sdi, 2)
              expect_named(s_sdi, c("SDI", "DistPar"))
            })
          })

test_that("ScientSDI properly fetches and calculates values, Good='yes'",
          {
            vcr::use_cassette("ScientSDI_TS-4_Good-Yes", {
              s_sdi <- ScientSDI(
                lon = -47.3,
                lat = -22.87,
                start.date = "1991-01-01",
                end.date = "2022-12-31",
                TS = 4,
                Good = "yes"
              )

              expect_type(s_sdi, "list")
              expect_length(s_sdi, 4)
              expect_named(s_sdi, c("SDI", "DistPar", "GoodFit", "Normality"))

              expect_named(
                s_sdi[[1]],
                c(
                  "Year",
                  "Month",
                  "quart.month",
                  "Rain",
                  "PE.Harg",
                  "PE.PM",
                  "PPE.Harg",
                  "PPE.PM",
                  "SPI",
                  "SPEI.Harg",
                  "SPEI.PM",
                  "Categ.SPI",
                  "Categ.SPEI.Harg",
                  "Categ.SPEI.PM"
                )
              )
              expect_equal(s_sdi[[1]]$Rain[1], 260.76, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$PE.Harg[1], 174.228016, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$PE.PM[1], 148.660225, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$PPE.Harg[1], 86.531984, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$PPE.PM[1], 112.099775, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$SPI[1], 0.409543, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$SPEI.Harg[1], 0.061162, tolerance = 0.01)
              expect_equal(s_sdi[[1]]$Categ.SPI[1], "Normal")
              expect_equal(s_sdi[[1]]$Categ.SPEI.Harg[1], "Normal")
              expect_equal(s_sdi[[1]]$Categ.SPEI.PM[1], "Normal")
            })
          })

## expect messages for removed rows ----

expect_message(
  s_sdi <- ScientSDI(
    lon = -48.11,
    lat = -24.81,
    start.date = "1991-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "no",
    distr = "GEV",
    PEUplim = 45
  ),
  "above limit: 48, 52, 96, 336"
)
expect_length(s_sdi, 2)
expect_equal(s_sdi[[1]]$PE.PM[1], 30.120835, tolerance = 0.01)

## Check user inputs, expect errors ----

expect_error(
  ScientSDI(
    lon = -47.3,
    lat = -22.87,
    start.date = "2015-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "maybe"
  )
)

expect_error(
  s_sdi <- ScientSDI(
    lon = -48.11,
    lat = -24.81,
    start.date = "1991-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "yes",
    distr = "GEV",
    PELowlim = 100
  ),
  "There are not enough rows in the data"
)

expect_error(
  s_sdi <- ScientSDI(
    lon = -48.11,
    lat = -24.81,
    start.date = "1991-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "yes",
    distr = "GEV",
    PELowlim = "onehundred"
  ),
  "Please, provide appropriate numerical values for `RainUplim` or"
)

expect_error(
  s_sdi <- ScientSDI(
    lon = -48.11,
    lat = -24.81,
    start.date = "1991-01-01",
    end.date = "1991-01-02",
    TS = 1,
    Good = "yes",
    distr = "GEV",
    PELowlim = 100
  ),
  "Please select a longer period between start.date and end.date."
)
