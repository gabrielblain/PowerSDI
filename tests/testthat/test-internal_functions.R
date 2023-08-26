
test_that(".check_dates corrects backwards dates", {
  start <- as.Date("2020-01-31")
  end <- as.Date("2010-01-01")
  expect_message(.check_dates(user.dates = c(start, end)),
                 regexp = "Your start and end dates were reversed. They have*")
  dates <- .check_dates(user.dates = c(start, end))
  expect_equal(dates[[1]], end)
  expect_equal(dates[[2]], start)
})

test_that(".check_dates stops with too small time step", {
  start <- as.Date("2020-01-01")
  end <- as.Date("2020-01-31")
  expect_error(.check_dates(user.dates = c(start, end)))
})

test_that(".check_dates stops with date in future", {
  start <- as.Date("2010-01-01")
  end <- lubridate::today() + 1
  expect_error(.check_dates(user.dates = c(start, end)))
})

test_that(".check_dates stops on invalid date format", {
  start <- as.Date("2010-01-01")
  end <- 21000180
  expect_error(.check_dates(user.dates = c(start, end)))
})
