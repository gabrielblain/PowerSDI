
## Method = "HS" ----

test_that("Reference() works as expected in example", {
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
    tolerance = 0.01
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
    tolerance = 0.01
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
    tolerance = 0.01
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
    tolerance = 0.01
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

## Method = "PM" -----

test_that("Reference() calculates SPI/SPEI using GEV & PM w/ TS4", {
  ref_gev <- Reference(
    ref = refPM,
    distr = "GEV",
    PEMethod = "PM",
    TS = 4L
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
      352.37,
      354.43,
      333.99,
      223.02,
      224.44,
      196.08,
      220.88,
      380.11,
      285,
      253.48
    ),
    tolerance = 0.01
  )
  expect_equal(
    ref_gev$PE[1:10],
    c(
      129.680970873538,
      125.817221761724,
      123.368573047977,
      111.274831585327,
      105.795861324817,
      107.611289852481,
      102.423439585401,
      90.6403414919855,
      96.4932881727808,
      92.5905065013557
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_gev$PPE[1:10],
    c(
      222.689029126462,
      228.612778238276,
      210.621426952023,
      111.745168414673,
      118.644138675183,
      88.4687101475186,
      118.456560414599,
      289.469658508015,
      188.506711827219,
      160.889493498644
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_gev$SPI[1:10],
    c(
      1.46519296007417,
      1.61278839499981,
      1.39053741462298,
      0.573924325713957,
      0.921708775264062,
      0.699728370335599,
      1.36209784511568,
      2.99975144511263,
      2.51167741199298,
      2.37026724613154
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_gev$SPEI[1:10],
    c(
      1.50931225566312,
      1.80214589657084,
      1.36579315604598,
      0.546724767747658,
      0.946328515139361,
      0.814668569844952,
      1.37326596082147,
      2.44686909226193,
      2.26637470415321,
      2.22142968705348
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_gev$Categ.SPI[1:10],
    c(
      "mod.wet",
      "sev.wet",
      "mod.wet",
      "Normal",
      "Normal",
      "Normal",
      "mod.wet",
      "ext.wet",
      "ext.wet",
      "ext.wet"
    )
  )

  expect_equal(
    ref_gev$Categ.SPEI[1:10],
    c(
      "sev.wet",
      "sev.wet",
      "mod.wet",
      "Normal",
      "Normal",
      "Normal",
      "mod.wet",
      "ext.wet",
      "ext.wet",
      "ext.wet"
    )
  )
})


test_that("Reference() calculates SPI/SPEI using GLO & HS w/ TS4", {
  ref_glo <- Reference(
    ref = refPM,
    distr = "GLO",
    PEMethod = "PM",
    TS = 4L
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
      352.37,
      354.43,
      333.99,
      223.02,
      224.44,
      196.08,
      220.88,
      380.11,
      285,
      253.48
    ),
    tolerance = 0.01
  )
  expect_equal(
    ref_glo$PE[1:10],
    c(
      129.680970873538,
      125.817221761724,
      123.368573047977,
      111.274831585327,
      105.795861324817,
      107.611289852481,
      102.423439585401,
      90.6403414919855,
      96.4932881727808,
      92.5905065013557
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$PPE[1:10],
    c(
      222.689029126462,
      228.612778238276,
      210.621426952023,
      111.745168414673,
      118.644138675183,
      88.4687101475186,
      118.456560414599,
      289.469658508015,
      188.506711827219,
      160.889493498644
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$SPI[1:10],
    c(
      1.46519296007417,
      1.61278839499981,
      1.39053741462298,
      0.573924325713957,
      0.921708775264062,
      0.699728370335599,
      1.36209784511568,
      2.99975144511263,
      2.51167741199298,
      2.37026724613154
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$SPEI[1:10],
    c(
      1.5127259508262,
      1.72374724914942,
      1.41589158933947,
      0.609941429365168,
      1.03344120455704,
      0.872954823158599,
      1.42429241279852,
      2.36621176787509,
      2.17824619265434,
      2.13752609645037
    ),
    tolerance = 0.01
  )

  expect_equal(
    ref_glo$Categ.SPI[1:10],
    c(
      "mod.wet",
      "sev.wet",
      "mod.wet",
      "Normal",
      "Normal",
      "Normal",
      "mod.wet",
      "ext.wet",
      "ext.wet",
      "ext.wet"
    )
  )

  expect_equal(
    ref_glo$Categ.SPEI[1:10],
    c(
      "sev.wet",
      "sev.wet",
      "mod.wet",
      "Normal",
      "mod.wet",
      "Normal",
      "mod.wet",
      "ext.wet",
      "ext.wet",
      "ext.wet"
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

test_that("Reference() errors w/ 'HS' and cols != 8 in ref", {
  expect_error(ref_glo <- Reference(
    ref = refPM,
    distr = "GLO",
    PEMethod = "HS",
    TS = 4L
  ),
  "It should be 8.")
})

test_that("Reference() errors w/ 'PM' and cols != 11 in ref", {
  expect_error(
    ref_glo <- Reference(
      ref = refHS,
      distr = "GLO",
      PEMethod = "PM",
      TS = 4L
    ),
    "It should be 11."
  )
})
