
# in most of these tests, `Good = "no"` because it's faster and is tested 1X
test_that("ScientSDI properly fetches and calculates values TS=1, distr=GEV",
          {
            vcr::use_cassette("ScientSDI", {
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
            vcr::use_cassette("ScientSDI", {
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

test_that("ScientSDI properly fetches and calculates values, distr=GLO,
          good=yes",
          {
            vcr::use_cassette("ScientSDI", {
              s_sdi <- ScientSDI(
                lon = -47.3,
                lat = -22.87,
                start.date = "1991-01-01",
                end.date = "2022-12-31",
                distr = "GLO",
                Good = "yes"
              )

              expect_type(s_sdi, "list")
              expect_length(s_sdi, 4)
              expect_named(s_sdi, c("SDI", "DistPar", "GoodFit", "Normality"))
            })
          })

test_that("ScientSDI properly fetches and calculates values, TS=4, distr=GLO",
          {
            vcr::use_cassette("ScientSDI", {
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
            vcr::use_cassette("ScientSDI", {
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
              expect_equal(
                s_sdi[[1]]$Rain[1:10],
                c(
                  260.76,
                  338.79,
                  359.57,
                  336.22,
                  236.5,
                  248.75,
                  198.76,
                  223.75,
                  360.42,
                  245.98
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PE.Harg[1:10],
                c(
                  152.659403425914,
                  145.896009143681,
                  139.457767681156,
                  135.433846339862,
                  119.544849667494,
                  114.479425623325,
                  111.612945595566,
                  106.907680085454,
                  107.008645749893,
                  105.666232270404
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PE.PM[1:10],
                c(
                  132.034697939064,
                  123.627657536289,
                  119.917308569187,
                  117.341358194535,
                  103.305687785089,
                  98.0509744109017,
                  99.7318359574366,
                  94.3866880409517,
                  85.6493596073208,
                  90.9219118138176
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PPE.Harg[1:10],
                c(
                  108.100596574086,
                  192.893990856319,
                  220.112232318844,
                  200.786153660138,
                  116.955150332506,
                  134.270574376675,
                  87.1470544044343,
                  116.842319914546,
                  253.411354250107,
                  140.313767729596
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PPE.PM[1:10],
                c(
                  128.725302060936,
                  215.162342463711,
                  239.652691430812,
                  218.878641805465,
                  133.194312214911,
                  150.699025589098,
                  99.0281640425634,
                  129.363311959048,
                  274.770640392679,
                  155.058088186182
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$SPI[1:10],
                c(
                  0.409543204634262,
                  1.32329551119963,
                  1.73669251746959,
                  1.4562523684948,
                  0.9173381440806,
                  1.35163443304433,
                  1.05139101697662,
                  1.56390357609342,
                  2.99975144511263,
                  2.29194297954978
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$SPEI.Harg[1:10],
                c(
                  0.113858299490916,
                  1.2456232844912,
                  1.94152836951902,
                  1.43833614411158,
                  0.875021064254541,
                  1.3598868087475,
                  1.04701422560855,
                  1.67950700819748,
                  2.64231692991216,
                  2.8474815696763
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$Categ.SPI[1:10],
                c(
                  "Normal",
                  "mod.wet",
                  "sev.wet",
                  "mod.wet",
                  "Normal",
                  "mod.wet",
                  "mod.wet",
                  "sev.wet",
                  "ext.wet",
                  "ext.wet"
                )
              )
              expect_equal(
                s_sdi[[1]]$Categ.SPEI.Harg[1:10],
                c(
                  "Normal",
                  "mod.wet",
                  "sev.wet",
                  "mod.wet",
                  "Normal",
                  "mod.wet",
                  "mod.wet",
                  "sev.wet",
                  "ext.wet",
                  "ext.wet"
                )
              )
              expect_equal(
                s_sdi[[1]]$Categ.SPEI.PM[1:10],
                c(
                  "Normal",
                  "mod.wet",
                  "ext.wet",
                  "mod.wet",
                  "Normal",
                  "mod.wet",
                  "mod.wet",
                  "sev.wet",
                  "ext.wet",
                  "ext.wet"
                )
              )
            })
          })

## expect messages for removed rows ----
test_that("ScientSDI emits a message and removes rows above the limit", {
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
    regexp = "above limit: 1108, 1248"
  )
  expect_length(s_sdi, 2)
  expect_equal(s_sdi[[1]]$PE.PM[1], 28.37215, tolerance = 0.01)
})

## Check user inputs, expect errors ----
test_that("ScientSDI errors with an invalid entry for 'Good'", {
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
})

test_that("ScientSDIerros with not enough data to run", {
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
})

test_that("ScientSDI errors with invalid values for 'Rainuplim'", {
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
})

test_that("ScientSDI errors when dates are too close together", {
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
})
