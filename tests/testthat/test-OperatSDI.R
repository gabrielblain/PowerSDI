

test_that("OperatSDI works as expected in example", {
  vcr::use_cassette("OperatSDI-HS", {
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
        79.4021491975414,
        77.2887684422473,
        72.7382753982665,
        71.0434518271779,
        67.5909689784445,
        64.8060544474655
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -68.0821491975414,
        -42.6887684422473,
        -36.8082753982665,
        -31.8334518271779,
        -20.6509689784445,
        -49.4460544474655
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
        -0.433915296540335,
        -0.252392352116453,
        -0.0749384759119569,
        0.162662067295644,
        0.518966324971316,
        0.116415753685488
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
        90.1205821981333,
        88.070088043992,
        82.1028375249963,
        77.81230954449,
        69.8280874392652,
        69.7586238134786
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -78.8005821981333,
        -53.470088043992,
        -46.1728375249964,
        -38.60230954449,
        -22.8880874392652,
        -54.3986238134786
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
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.485394334878798,
        -0.345103969149215,
        -0.163349652114086,
        0.142781562089768,
        0.560134125535894,
        0.181995340717053
      ),
      tolerance = 0.1
    )
    expect_equal(osdi[, "Categ.SPI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
    expect_equal(osdi[, "Categ.SPEI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
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
        90.1205821981333,
        88.070088043992,
        82.1028375249963,
        77.81230954449,
        69.8280874392652,
        69.7586238134786
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -78.8005821981333,
        -53.470088043992,
        -46.1728375249964,
        -38.60230954449,
        -22.8880874392652,
        -54.3986238134786
      ),
      tolerance = 0.01
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
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.0926067131721325,
        -0.0048911664412907,
        0.115089358597543,
        0.333638734413817,
        0.664103255778851,
        0.363123196739047
      ),
      tolerance = 0.01
    )
    expect_equal(osdi[, "Categ.SPI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
    expect_equal(osdi[, "Categ.SPEI"],
                 c("Normal", "Normal", "Normal", "Normal", "Normal", "Normal"))
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
