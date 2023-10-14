test_that("OperatSDI works as expected in example", {
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
  expect_equal(osdi[, "Lat"], c(-22.67, -22.67, -22.67, -22.67, -22.67, -22.67))
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
