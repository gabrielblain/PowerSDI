
test_that("OperatSDI works as expected in example", {
  vcr::use_cassette("OperatSDI-HS", {
    data("DistPar")
    osdi <- OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-01",
      end.date = "2023-06-30",
      parms = DistPar
    )
    expect_s3_class(osdi, "data.frame")
    expect_length(osdi, 12)
    expect_equal(nrow(osdi), 6)
    expect_named(
      osdi,
      c(
        "Lon",
        "Lat",
        "Year",
        "Month",
        "quart.month",
        "Rain",
        "PE",
        "PPE",
        "SPI",
        "SPEI",
        "Categ.SPI",
        "Categ.SPEI"
      )
    )
    expect_equal(osdi[, "Lon"], c(-47.3, -47.3, -47.3, -47.3, -47.3, -47.3))
    expect_equal(osdi[, "Lat"],
                 c(-22.67, -22.67, -22.67, -22.67, -22.67, -22.67))
    expect_equal(osdi[, "Year"], c(2023, 2023, 2023, 2023, 2023, 2023))
    expect_equal(osdi[, "Month"], c(5, 5, 6, 6, 6, 6))
    expect_equal(osdi[, "quart.month"], c(19, 20, 21, 22, 23, 24))
    expect_equal(osdi[, "Rain"],
                 c(11.32, 34.6, 35.93, 39.21, 46.94, 15.36),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        93.398055164382,
        91.4362597205983,
        86.2179712791408,
        85.1970945920589,
        81.0599379146688,
        78.6346906850623
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -82.078055164382,
        -56.8362597205983,
        -50.2879712791408,
        -45.9870945920589,
        -34.1199379146688,
        -63.2746906850623
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589525,
        -0.385541271014979,
        -0.183884807837741,
        0.195580476882116,
        -0.386626155683377
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.876080819273402,
        -0.643505897961827,
        -0.465928871841173,
        -0.261613415849469,
        0.233705486100684,
        -0.433331351592998
      ),
      tolerance = 0.1
    )
    expect_equal(osdi[, "Categ.SPI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
    expect_equal(osdi[, "Categ.SPEI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
  })
})

test_that("OperatSDI works as expected w/ PEMethod=PM", {
  vcr::use_cassette("OperatSDI-PM", {
    osdi <- OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-01",
      end.date = "2023-06-30",
      PEMethod = "PM",
      parms = DistPar
    )
    expect_s3_class(osdi, "data.frame")
    expect_length(osdi, 12)
    expect_equal(nrow(osdi), 6)
    expect_named(
      osdi,
      c(
        "Lon",
        "Lat",
        "Year",
        "Month",
        "quart.month",
        "Rain",
        "PE",
        "PPE",
        "SPI",
        "SPEI",
        "Categ.SPI",
        "Categ.SPEI"
      )
    )

    expect_equal(osdi[, "Lon"], c(-47.3, -47.3, -47.3, -47.3, -47.3, -47.3))
    expect_equal(osdi[, "Lat"],
                 c(-22.67, -22.67, -22.67, -22.67, -22.67, -22.67))
    expect_equal(osdi[, "Year"], c(2023, 2023, 2023, 2023, 2023, 2023))
    expect_equal(osdi[, "Month"], c(5, 5, 6, 6, 6, 6))
    expect_equal(osdi[, "quart.month"], c(19, 20, 21, 22, 23, 24))
    expect_equal(osdi[, "Rain"],
                 c(11.32, 34.6, 35.93, 39.21, 46.94, 15.36),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        140.158552712686,
        149.314819414548,
        139.883948394412,
        144.102577406368,
        135.963707185575,
        132.552852346312
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -128.838552712686,
        -114.714819414548,
        -103.953948394412,
        -104.892577406368,
        -89.0237071855748,
        -117.192852346312
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589524,
        -0.385541271014979,
        -0.183884807837741,
        0.195580476882115,
        -0.386626155683378
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -2.17062067194088,
        -2.09486022918856,
        -2.36852896026298,
        -3.92364479231243,
        -1.51192614071924,
        -4.35395047715826
      ),
      tolerance = 0.1
    )
    expect_equal(osdi[, "Categ.SPI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
    expect_equal(osdi[, "Categ.SPEI"],
                 c(
                   "ext.dry",
                   "ext.dry",
                   "ext.dry",
                   "ext.dry",
                   "sev.dry",
                   "ext.dry"
                 ))
  })
})

test_that("OperatSDI works as expected w/ PEMethod=PM, dist=GLO", {
  vcr::use_cassette("OperatSDI-PM", {
    osdi <- OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-01",
      end.date = "2023-06-30",
      distr = "GLO",
      PEMethod = "PM",
      parms = DistPar
    )
    expect_s3_class(osdi, "data.frame")
    expect_length(osdi, 12)
    expect_equal(nrow(osdi), 6)
    expect_named(
      osdi,
      c(
        "Lon",
        "Lat",
        "Year",
        "Month",
        "quart.month",
        "Rain",
        "PE",
        "PPE",
        "SPI",
        "SPEI",
        "Categ.SPI",
        "Categ.SPEI"
      )
    )

    expect_equal(osdi[, "Lon"], c(-47.3, -47.3, -47.3, -47.3, -47.3, -47.3))
    expect_equal(osdi[, "Lat"],
                 c(-22.67, -22.67, -22.67, -22.67, -22.67, -22.67))
    expect_equal(osdi[, "Year"], c(2023, 2023, 2023, 2023, 2023, 2023))
    expect_equal(osdi[, "Month"], c(5, 5, 6, 6, 6, 6))
    expect_equal(osdi[, "quart.month"], c(19, 20, 21, 22, 23, 24))
    expect_equal(osdi[, "Rain"],
                 c(11.32, 34.6, 35.93, 39.21, 46.94, 15.36),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        140.158552712686,
        149.314819414548,
        139.883948394412,
        144.102577406368,
        135.963707185575,
        132.552852346312
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -128.838552712686,
        -114.714819414548,
        -103.953948394412,
        -104.892577406368,
        -89.0237071855748,
        -117.192852346312
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589524,
        -0.385541271014979,
        -0.183884807837741,
        0.195580476882115,
        -0.386626155683378
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.869564987719618,
        -0.843375808988047,
        -0.935077359265666,
        -1.33720536025451,
        -0.618389343897046,
        -1.42260223092696
      ),
      tolerance = 0.1
    )
    expect_equal(osdi[, "Categ.SPI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
    expect_equal(osdi[, "Categ.SPEI"],
                 c(
                   "Normal",
                   "Normal",
                   "Normal",
                   "mod.dry",
                   "Normal",
                   "mod.dry"
                 ))
  })
})

test_that("OperatSDI fails w/ bad distr", {
  data("DistPar")
  expect_error(
    OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-01",
      end.date = "2023-06-30",
      parms = DistPar,
      distr = "bad"
    )
  )
})

test_that("OperatSDI fails w/ bad PEMethod", {
  data("DistPar")
  expect_error(
    OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-01",
      end.date = "2023-06-30",
      parms = DistPar,
      PEMethod = "AM"
    )
  )
})

# this fails but because {nasapower} catches it, not because this package does...
test_that("OperatSDI fails w/ bad date format", {
  data("DistPar")
  expect_error(
    OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "06-01-2023",
      end.date = "2023-06-30",
      parms = DistPar,
      distr = "GEV"
    ),
    regexp = "5-12-08 is not a valid entry for date. Enter as YYYY-MM-DD."
  )
})

test_that("OperatSDI fails w/ dates too close", {
  data("DistPar")
  expect_error(
    OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "2023-06-29",
      end.date = "2023-06-30",
      parms = DistPar,
      distr = "GEV"
    ),
    regexp = "Time difference between `end.date` and `start.date`"
  )
})

test_that("OperatSDI fails w/ missing parms", {
  data("DistPar")
  expect_error(
    OperatSDI(
      lon = -47.3,
      lat = -22.67,
      start.date = "06-01-2023",
      end.date = "2023-06-30",
      distr = "GEV"
    ),
    regexp = "It seems that you don't have the distributions' parameters"
  )
})
