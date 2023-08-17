test_that(".check_dates corrects backwards dates", {
  start <- as.Date("2020-01-31")
  end <- as.Date("2010-01-01")
  expect_message(.check_dates(user.dates = c(start, end)),
                 regexp = "Your start and end dates were reversed. They have*")
  dates <- .check_dates(user.dates = c(start, end))
  expect_equal(dates[[1]], end)
  expect_equal(dates[[2]], start)
})
