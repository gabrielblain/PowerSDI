test_that("PlotData creates proper plots", {
  skip_if_offline()
  PlotData_a <- PlotData(
    lon = -47.3,
    lat = -22.87,
    start.date = "2021-12-28",
    end.date = "2022-12-31"
  )
  vdiffr::expect_doppelganger("disp-PlotData-a", PlotData_a)
})
