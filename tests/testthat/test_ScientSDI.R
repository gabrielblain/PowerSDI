
test_that("ScientSDI properly fetches and calculates values, no", {
  skip_if_offline()
  vcr::use_cassette("ScientSDI_no", {
    s_sdi <- ScientSDI(
      lon = -47.3,
      lat = -22.87,
      start.date = "2015-01-01",
      end.date = "2022-12-31",
      TS = 1,
      Good = "no"
    )
  })
  expect_type(s_sdi, "list")
  expect_length(s_sdi, 2)
  expect_named(s_sdi, c("SDI", "DistPar"))
})

test_that("ScientSDI properly fetches and calculates values, yes", {
  skip_if_offline()
  vcr::use_cassette("ScientSDI_yes", {
    s_sdi <- ScientSDI(
      lon = -47.3,
      lat = -22.87,
      start.date = "2015-01-01",
      end.date = "2022-12-31",
      TS = 1,
      Good = "yes"
    )
  })
  expect_type(s_sdi, "list")
  expect_length(s_sdi, 4)
  expect_named(s_sdi, c("SDI", "DistPar", "GoodFit", "Normality"))
})

test_that("ScientSDI stops on invalid `Good` value", {
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

