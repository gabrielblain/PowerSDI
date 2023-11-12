
test_that("PlotData creates proper plots", {
  skip_if_offline()
  vcr::use_cassette("PlotData", {
    PlotData_a <- PlotData(
      lon = -47.3,
      lat = -22.87,
      start.date = "2021-12-28",
      end.date = "2022-12-31"
    )
    vdiffr::expect_doppelganger("disp-PlotData-a", PlotData_a)
  })
})

test_that("PlotData stops if not enough time between dates", {
  skip_if_offline()
  expect_error(PlotData(
    lon = -47.3,
    lat = -22.87,
    start.date = "2021-12-28",
    end.date = "2021-12-31"
  ))
})

test_that("PlotData stops if invalid date format is supplied", {
  skip_if_offline()
  expect_error(PlotData(
    lon = -47.3,
    lat = -22.87,
    start.date = "2020-12-28",
    end.date = "12-28-21"
  ))
})
