test_that("Reference() calculates SPI/SPEI using GEV & HS w/ TS4", {
  expect_warning(
    ref_gev <- Reference(
      ref = refHS,
      distr = "GEV",
      PEMethod = "HS",
      TS = 4L
    ),
    "The latest quart.month period is not complete"
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

  expect_equal(
    ref_gev$Rain[1:10],
    c(
      517.7,
      530.5,
      455.5,
      242.5,
      202.5,
      149.3,
      135.3,
      347.8,
      256.8,
      250.2
    ),
    tolerance = 0.1
  )
  expect_equal(
    ref_gev$PE[1:10],
    c(
      151.51484229481,
      149.085625905187,
      148.730014928848,
      137.443125147119,
      127.995698624078,
      129.712688148129,
      124.511899737206,
      115.146547681111,
      116.458264326486,
      108.969475304204
    ),
    tolerance = 0.1
  )

  expect_equal(
    ref_gev$PPE[1:10],
    c(
      366.18515770519,
      381.414374094813,
      306.769985071152,
      105.056874852881,
      74.5043013759225,
      19.5873118518713,
      10.7881002627939,
      232.653452318889,
      140.341735673514,
      141.230524695796
    ),
    tolerance = 0.1
  )

  expect_equal(
    ref_gev$SPI[1:10],
    c(
      2.2726620181654,
      2.27714084882156,
      1.91776980161342,
      0.748993763410941,
      0.434572929905318,
      -0.116602821250248,
      -0.1579964385339,
      1.96308365370141,
      1.39509585295987,
      1.6415716285752
    ),
    tolerance = 0.1
  )

  expect_equal(
    ref_gev$SPEI[1:10],
    c(
      2.10571151816844,
      2.19000429999202,
      1.77871012111374,
      0.67594956846407,
      0.399478705108477,
      0.0355288934418908,
      -0.0971844132189002,
      2.01361366292708,
      1.55693457747235,
      1.81488661351313
    ),
    tolerance = 0.1
  )

  expect_equal(
    ref_gev$Categ.SPI[1:10],
    c(
      "ext.wet",
      "ext.wet",
      "sev.wet",
      "Normal",
      "Normal",
      "Normal",
      "Normal",
      "sev.wet",
      "mod.wet",
      "sev.wet"
    )
  )

  expect_equal(
    ref_gev$Categ.SPEI[1:10],
    c(
      "ext.wet",
      "ext.wet",
      "sev.wet",
      "Normal",
      "Normal",
      "Normal",
      "Normal",
      "ext.wet",
      "sev.wet",
      "sev.wet"
    )
  )
})


test_that("Reference() calculates SPI/SPEI using GLO & HS w/ TS4", {
  expect_warning(
    ref_glo <- Reference(
      ref = refHS,
      distr = "GLO",
      PEMethod = "HS",
      TS = 4L
    ),
    "The latest quart.month period is not complete"
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

  expect_equal(
    ref_glo$Rain[1:10],
    c(
      517.7,
      530.5,
      455.5,
      242.5,
      202.5,
      149.3,
      135.3,
      347.8,
      256.8,
      250.2
    ),
    tolerance = 0.1
  )
  expect_equal(
    ref_glo$PE[1:10],
    c(
      151.51484229481,
      149.085625905187,
      148.730014928848,
      137.443125147119,
      127.995698624078,
      129.712688148129,
      124.511899737206,
      115.146547681111,
      116.458264326486,
      108.969475304204
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$PPE[1:10],
    c(
      366.18515770519,
      381.414374094813,
      306.769985071152,
      105.056874852881,
      74.5043013759225,
      19.5873118518713,
      10.7881002627939,
      232.653452318889,
      140.341735673514,
      141.230524695796
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$SPI[1:10],
    c(
      2.2726620181654,
      2.27714084882156,
      1.91776980161342,
      0.748993763410941,
      0.434572929905318,
      -0.116602821250248,
      -0.1579964385339,
      1.96308365370141,
      1.39509585295987,
      1.6415716285752
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$SPEI[1:10],
    c(
      2.0484784193881,
      2.11276926710058,
      1.79495229585095,
      0.756133104534819,
      0.459409497099322,
      0.0195320712562312,
      -0.121174626129315,
      1.97671118833318,
      1.58719496809762,
      1.80750965633644
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$Categ.SPI[1:10],
    c(
      "ext.wet",
      "ext.wet",
      "sev.wet",
      "Normal",
      "Normal",
      "Normal",
      "Normal",
      "sev.wet",
      "mod.wet",
      "sev.wet"
    )
  )

  expect_equal(
    ref_glo$Categ.SPEI[1:10],
    c(
      "ext.wet",
      "ext.wet",
      "sev.wet",
      "Normal",
      "Normal",
      "Normal",
      "Normal",
      "sev.wet",
      "sev.wet",
      "sev.wet"
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
