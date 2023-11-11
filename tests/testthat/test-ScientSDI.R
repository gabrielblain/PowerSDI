
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
                  174.228015754468,
                  164.683675547223,
                  156.318877888947,
                  150.86490803681,
                  134.937672017853,
                  129.033076756106,
                  125.847383112599,
                  120.816243002795,
                  118.231282515205,
                  117.77245474405
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PE.PM[1:10],
                c(
                  148.660224757601,
                  137.358427091596,
                  132.252420318406,
                  128.642540863799,
                  114.842677721689,
                  109.1183629887,
                  110.801975549217,
                  105.112425280346,
                  93.5987033974773,
                  100.141483003263
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PPE.Harg[1:10],
                c(
                  86.531984245532,
                  174.106324452777,
                  203.251122111053,
                  185.35509196319,
                  101.562327982147,
                  119.716923243895,
                  72.9126168874014,
                  102.933756997205,
                  242.188717484795,
                  128.20754525595
                ),
                tolerance = 0.01
              )
              expect_equal(
                s_sdi[[1]]$PPE.PM[1:10],
                c(
                  112.099775242399,
                  201.431572908404,
                  227.317579681594,
                  207.577459136201,
                  121.657322278311,
                  139.6316370113,
                  87.9580244507831,
                  118.637574719654,
                  266.821296602523,
                  145.838516996737
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
                  0.0611618853284877,
                  1.19136216670346,
                  1.91248853558433,
                  1.42349468460309,
                  0.840207121767301,
                  1.32867405085524,
                  1.01264420925793,
                  1.62442243541364,
                  2.6730076111041,
                  2.98197586317061
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
    regexp = "above limit: 48, 52"
  )
  expect_length(s_sdi, 2)
  expect_equal(s_sdi[[1]]$PE.PM[1], 30.12083, tolerance = 0.01)
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

test_that("ScientSDI errors with not enough data to run", {
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

test_that("ScientSDI emits a warning when <30 years is selected", {
  expect_warning(
    s_sdi <- ScientSDI(
      lon = -48.11,
      lat = -24.81,
      start.date = "1993-01-01",
      end.date = "2020-12-31",
      TS = 1,
      Good = "no"
    ),
    regexp = "A period of 30 years is normal for calibrating standardised "
  )
})
