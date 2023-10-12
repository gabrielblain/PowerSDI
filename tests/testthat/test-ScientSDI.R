
test_that("ScientSDI properly fetches and calculates values or errors", {
  skip_if_offline()
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

  ## TS = 1 ----

  s_sdi <- ScientSDI(
    lon = -47.3,
    lat = -22.87,
    start.date = "1991-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "no"
  )

  expect_type(s_sdi, "list")
  expect_length(s_sdi, 2)
  expect_named(s_sdi, c("SDI", "DistPar"))


  ## TS = 4 (default) ----

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

  ## Good = "yes" ----

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

  ## distr = "GEV" ----

  s_sdi <- ScientSDI(
    lon = -48.11,
    lat = -24.81,
    start.date = "1991-01-01",
    end.date = "2022-12-31",
    TS = 1,
    Good = "no",
    distr = "GEV",
    RainUplim = 250
  )

  expect_type(s_sdi, "list")
  expect_length(s_sdi, 2)
  expect_named(s_sdi, c("SDI", "DistPar"))

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
  expect_equal(s_sdi[[1]]$PE.Harg[1], 30.11415, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$PE.PM[1], 30.12083, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$PPE.Harg[1], 0.8758454, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$PPE.PM[1], 0.8691654, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$SPI[1], -0.7457067, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$SPEI.Harg[1], -0.772679, tolerance = 0.01)
  expect_equal(s_sdi[[1]]$Categ.SPEI.Harg[1], "Normal")
  expect_equal(s_sdi[[1]]$SPEI.PM[1], -0.774377, tolerance = 0.01)

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
})
