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
               c(16.14,
                 46.97,
                 48.19,
                 49.18,
                 56.03,
                 16.01),
               tolerance = 0.1)
  expect_equal(
    osdi[, "PE"],
    c(
      92.9613637756484,
      90.4573894709113,
      84.6331313597626,
      84.2380514879889,
      80.470469798979,
      78.5558914484918
    ),
    tolerance = 0.1
  )
  expect_equal(
    osdi[, "PPE"],
    c(
      -76.8213637756484,
      -43.4873894709113,
      -36.4431313597626,
      -35.0580514879889,
      -24.440469798979,
      -62.5458914484918
    ),
    tolerance = 0.1
  )
  expect_equal(
    osdi[, "SPI"],
    c(
      -0.711827592268294,
      -0.0861677742340985,
      -0.0401492206818073,
      0.0723873113854012,
      0.373391649134867,
      -0.355352529150732
    ),
    tolerance = 0.1
  )
  expect_equal(
    osdi[, "SPEI"],
    c(
      -0.702144254646389,
      -0.273748861100733,
      -0.0652718012173975,
      0.075782232626725,
      0.444732507259655,
      -0.399191255734487
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
  expect_error(OperatSDI(
    lon = -47.3,
    lat = -22.67,
    start.date = "2023-06-01",
    end.date = "2023-06-30",
    parms = DistPar,
    distr = "bad"
  ))
})

test_that("OperatSDI fails w/ bad PEMethod", {
  data("DistPar")
  expect_error(OperatSDI(
    lon = -47.3,
    lat = -22.67,
    start.date = "2023-06-01",
    end.date = "2023-06-30",
    parms = DistPar,
    PEMethod = "AM"
  ))
})

# this fails but because {nasapower} catches it, not because this package does...
test_that("OperatSDI fails w/ bad date format", {
  data("DistPar")
  expect_error(OperatSDI(
    lon = -47.3,
    lat = -22.67,
    start.date = "06-01-2023",
    end.date = "2023-06-30",
    parms = DistPar,
    distr = "GEV"
  ))
})
