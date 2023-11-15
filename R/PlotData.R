#' Plot Rainfall and Potential Evapotranspiration Data
#'
#' Plots rainfall and potential evapotranspiration, both Penman-Monteith and
#'   Hargreaves and Samani, amounts using \acronym{NASA} \acronym{POWER} data.
#'
#' @param lon
#' longitude in decimal degrees: (+) Eastern Hemisphere (-) Western Hemisphere.
#' @param lat
#' latitude in decimal degrees: (+) Northern Hemisphere (-) Southern Hemisphere.
#' @param start.date
#' date at which the indices estimates should start ("YYYY-MM-DD").
#' @param end.date
#' date at which the indices estimates should end ("YYYY-MM-DD").
#' @return
#' No return value, called for side effects. Using this will display scatter
#'   plots of rainfall and potential evapotranspiration accumulated at the
#'   1-quart.month time scale in the active \R session.
#' @export
#' @importFrom graphics par
#' @examplesIf interactive()
#'
#' # This example requires an Internet connection to fetch data and so is only
#' # run in interactive sessions
#'
#' PlotData(
#'   lon = -47.3,
#'   lat = -22.87,
#'   start.date = "2021-12-28",
#'   end.date = "2022-12-31"
#' )
#'
PlotData <- function(lon, lat, start.date, end.date) {

  dates <- check.dates(c(start.date, end.date))
  start.date.user <- dates[[1]]
  end.date.user <- dates[[2]]

  if ((end.date.user - start.date.user) < 365) {
    stop(
      "The difference between `start.date` and `end.date` ",
      "must be of at least 1 year. Please select a longer period.",
    call. = FALSE
    )
  }

  start.year <- lubridate::year(start.date.user)
  start.month <- lubridate::month(start.date.user)
  final.year <- lubridate::year(end.date.user)
  final.month <- lubridate::month(end.date.user)
  final.day <- lubridate::day(end.date.user)
  start.week <-
    find.week.int(lubridate::day(start.date.user)) # see internal_functions.R

  message(
    "Just a sec. ",
    "Downloading NASA POWER data and calculating the other parameters.")

  sse_i <- daily.ETP.wrapper(lon,
                             lat,
                             start.date.user,
                             end.date.user)

  # see internal_functions.R for `find.week.int()`
  final.week <- find.week.int(final.day)
  n.years <- 1 + (final.year - start.year)
  total.nweeks <- 48 * n.years
  a <- 1
  b <- 2
  c <- 3
  d <- 4
  data.week <- matrix(NA, total.nweeks, 8)
  month <- start.month
  year <- start.year
  while (year <= final.year || month <= final.month) {
    data.week1 <- colSums(sse_i[which(sse_i$YEAR == year &
                                        sse_i$MM == month &
                                        sse_i$DD <= 7), 14:16])
    data.week2 <- colSums(sse_i[which(sse_i$YEAR == year &
                                        sse_i$MM == month &
                                        sse_i$DD > 7 &
                                        sse_i$DD <= 14), 14:16])
    data.week3 <- colSums(sse_i[which(sse_i$YEAR == year &
                                        sse_i$MM == month &
                                        sse_i$DD > 14 &
                                        sse_i$DD <= 21), 14:16])
    data.week4 <- colSums(sse_i[which(sse_i$YEAR == year &
                                        sse_i$MM == month &
                                        sse_i$DD > 21), 14:16])
    data.week[a, ] <- c(lon, lat, year, month, 1, data.week1)
    data.week[b, ] <- c(lon, lat, year, month, 2, data.week2)
    data.week[c, ] <- c(lon, lat, year, month, 3, data.week3)
    data.week[d, ] <- c(lon, lat, year, month, 4, data.week4)
    month <- month + 1
    if (year == final.year & month > final.month) {
      break
    }
    if (month > 12) {
      year <- year + 1
      month <- 1
    }
    a <- a + 4
    b <- b + 4
    c <- c + 4
    d <- d + 4
  }
  rows <-
    which(data.week[, 3] == final.year &
            data.week[, 4] > final.month)
  n.rows <- length(rows)
  if (n.rows > 0) {
    data.week <- data.week[-c(rows), ]
  }
  rows <-
    which(data.week[, 3] == final.year &
            data.week[, 4] == final.month &
            data.week[, 5] > final.week)
  n.rows <- length(rows)
  if (n.rows > 0) {
    data.week <- data.week[-c(rows), ]
  }
  n <- length(which(data.week[, 3] <= final.year))
  data.week <- data.week[1:n, ]

  # see internal_functions.R for `find.quart.month.int()`
  data.week <-
    cbind(data.week, find.quart.month.int(x = data.week))

  first.row <-
    which(data.week[, 3] == start.year &
            data.week[, 4] == start.month &
            data.week[, 5] == start.week)
  if (first.row > 1) {
    data.week <- data.week[-(1:(first.row - 1)), ]
  }
  # ensure that the user's `par()` settings are honoured on exit from this fn
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  # set up a three row plot with custom margins
  par(mfrow = c(3, 1), mar = c(4, 4, 1.5, 2))
  ##### Rainfall
  plot(
    data.week[, 6] ~ data.week[, 9],
    xlim = c(1, 48),
    main = "Checking suspicious data",
    xlab = "quart.month",
    ylab = "Rainfall (mm)"
  )
  ##### Potential Evapotranspiration (Hargreaves & Samani)
  plot(
    data.week[, 7] ~ data.week[, 9],
    xlim = c(1, 48),
    xlab = "quart.month",
    ylab = "PE (Hargreaves; mm)"
  )
  ##### Potential Evapotranspiration (FAO-56 Penman-Monteith)
  plot(
    data.week[, 8] ~ data.week[, 9],
    xlim = c(1, 48),
    xlab = "quart.month",
    ylab = "PE (FAO-56/PM; mm)"
  )
}
