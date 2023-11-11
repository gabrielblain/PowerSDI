
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
                 c(11.32, 34.6, 35.4, 59.87, 80.66, 48.99),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        93.3980724171991,
        91.4362787378043,
        86.589872394302,
        85.3028898528977,
        80.8505592616573,
        78.3901418112906
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -82.0780724171991,
        -56.8362787378043,
        -51.189872394302,
        -25.4328898528977,
        -0.19055926165727,
        -29.4001418112906
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589524,
        -0.402212292093659,
        0.310509584101494,
        0.774498903683356,
        0.655054855871761
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.876081406872403,
        -0.643506442737831,
        -0.494718988988015,
        0.321261022148455,
        0.858163136836273,
        0.66690136597896
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
                 c(11.32, 34.6, 35.4, 59.87, 80.66, 48.99),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        102.75593991822,
        101.24525284016,
        96.2679849596477,
        93.6726005244522,
        84.9711134475672,
        87.3231020219224
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -91.4359399182195,
        -66.6452528401605,
        -60.8679849596477,
        -33.8026005244522,
        -4.31111344756725,
        -38.3331020219224
      ),
      tolerance = 0.1
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589524,
        -0.402212292093659,
        0.310509584101494,
        0.774498903683356,
        0.655054855871761
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.839556780144381,
        -0.679073406398272,
        -0.568361259075333,
        0.257404159344443,
        0.841190831073442,
        0.578089034650871
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
                 c(11.32, 34.6, 35.4, 59.87, 80.66, 48.99),
                 tolerance = 0.1)
    expect_equal(
      osdi[, "PE"],
      c(
        102.75593991822,
        101.24525284016,
        96.2679849596477,
        93.6726005244522,
        84.9711134475672,
        87.3231020219224
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "PPE"],
      c(
        -91.4359399182195,
        -66.6452528401605,
        -60.8679849596477,
        -33.8026005244522,
        -4.31111344756725,
        -38.3331020219224
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "SPI"],
      c(
        -0.982842281872578,
        -0.497989940589524,
        -0.402212292093659,
        0.310509584101494,
        0.774498903683356,
        0.655054855871761
      ),
      tolerance = 0.01
    )
    expect_equal(
      osdi[, "SPEI"],
      c(
        -0.295911266293377,
        -0.206916031236537,
        -0.142521338006024,
        0.420758045719821,
        0.905745960635682,
        0.679111408858534
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
