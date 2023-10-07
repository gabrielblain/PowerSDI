test_that("Reference() calculates SPI/SPEI using GEV & HS w/ TS4", {
  data("refHS")

  expect_warning(Reference(
    ref = refHS,
    distr = "GEV",
    PEMethod = "HS",
    TS = 4L
  ),
  regexp = "The latest quart.month period is not complete")

  ref_gev <- Reference(
    ref = refHS,
    distr = "GEV",
    PEMethod = "HS",
    TS = 4
  )
  expect_s3_class(ref_gev, "data.frame")
  expect_length(ref_gev, 10)
  expect_equal(nrow(ref_gev), 1435)
  expect_named(
    ref_gev,
    c(
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
})


test_that("Reference() calculates SPI/SPEI using GLO & HS w/ TS4", {
  data("refHS")

  expect_warning(Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "HS",
    TS = 4
  ),
  regexp = "The latest quart.month period is not complete")

  ref_glo <- Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "HS",
    TS = 4
  )
  expect_s3_class(ref_glo, "data.frame")
  expect_length(ref_glo, 10)
  expect_equal(nrow(ref_glo), 1435)
  expect_named(
    ref_glo,
    c(
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
})

# test user input checks ----

test_that("Reference() stops on invalid PEMethod", {
  expect_error(Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "GS",
    TS = 4
  ),
  regexp = "`PEMethod` should be set to either 'HS' or 'PM'.*")
})

test_that("Reference() stops on invalid distr", {
  expect_error(Reference(
    ref = refHS,
    distr = "GLOW",
    PEMethod = "HS",
    TS = 4
  ),
  regexp = "`distr` should be set to either 'GEV' or 'GLO'.*")
})

test_that("Reference() stops on invalid TS value", {
  expect_error(Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "HS",
    TS = 0.1
  ),
  regexp = "TS must be a whole value ranging between 1 and 96.*")

  expect_error(Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "HS",
    TS = 1.1
  ),
  regexp = "TS must be a whole value ranging between 1 and 96*")

  expect_error(Reference(
    ref = refHS,
    distr = "GLO",
    PEMethod = "HS",
    TS = 100
  ),
  regexp = "TS must be a whole value ranging between 1 and 96*")
})
