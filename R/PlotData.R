#' PlotData
#'
#' Plots rainfall and potential evapotranspiration amounts using \acronym{NASA}
#'   \acronym{POWER} data.
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
#' Scatter plots of Rainfall and potential evapotranspiration accumulated at the
#'   1-quart.month time scale.
#' @export
#' @importFrom graphics par
#' @examplesIf interactive()
#' PlotData(
#'   lon = -47.3,
#'   lat = -22.87,
#'   start.date = "2021-12-28",
#'   end.date = "2022-12-31"
#' )
PlotData <- function(lon, lat, start.date, end.date) {

  dates <- check.dates(c(start.date, end.date))

  start.date.user <- dates[[1]]
  end.date.user <- dates[[2]]

  min.period <- end.date.user - start.date.user
  if (min.period < 365) {
    stop(
      "The difference between start.date and end.date must be of at least 1 year.
    Please select a longer period.",
    call. = FALSE
    )
  }

  start.user.day <-
    as.numeric(format(start.date.user, format = "%d"))
  start.month <-
    as.numeric(format(start.date.user, format = "%m"))
  start.year <-
    as.numeric(format(start.date.user, format = "%Y"))

  start.week <- find.week.int(start.user.day) # see internal_functions.R
  dif <- calculate.dif(start.week, start.user.day) # see internal_functions.R

  start.date.protocal <- start.date.user - dif
  message(
    "Just a sec. ",
    "Downloading NASA POWER data and calculating the others parameters.")
  sse_i <-
    get_sdi_power_data(lon, lat, start.date.user, end.date.user)
  decli <-
    23.45 * sin((360 * (sse_i$DOY - 80) / 365) * (0.01745329))
  lat.rad <- lat * (0.01745329)
  decli.rad <- decli * (0.01745329)
  hn.rad <- (acos(tan(decli.rad) * -tan(lat.rad)))
  hn.deg <- hn.rad * (180 / pi)
  N <- (2 * hn.deg) / 15

  dist.terra.sol <-
    1 + (0.033 * cos((0.01745329) * (sse_i$DOY * (0.9863014))))

  Ra <-
    (37.6 * (dist.terra.sol ^ 2)) *
    ((0.01745329) *
       hn.deg * sin(lat.rad) * sin(decli.rad) +
       (cos(lat.rad) * cos(decli.rad) * sin(hn.rad)))

  ####   Hargreaves&Samani
  ETP.harg.daily <-
    0.0023 * (Ra * 0.4081633) * (sse_i$T2M_MAX - sse_i$T2M_MIN) ^ 0.5 *
    (sse_i$T2M + 17.8)

  ###    Penman- Monteith-FAO
  es <- 0.6108 * exp((17.27 * sse_i$T2M) / (sse_i$T2M + 273.3))
  ea <- (sse_i$RH2M * es) / 100
  slope.pressure <- (4098 * es) / ((sse_i$T2M + 237.3) ^ 2)
  Q0.ajust <- 0.75 * Ra

  Rn <-
    (1 - 0.2) * sse_i$ALLSKY_SFC_SW_DWN -
    (1.35 * (sse_i$ALLSKY_SFC_SW_DWN / Q0.ajust) - 0.35) *
    (0.35 - (0.14 * sqrt(ea))) * (0.0000000567) *
    (((sse_i$T2M ^ 4) + (sse_i$T2M_MIN ^ 4)) / 2)

  ETP.pm.daily <-
    (0.408 * slope.pressure * (Rn - 0.8) + 0.063 * (900 / (sse_i$T2M + 273)) *
       sse_i$WS2M * (es - ea)) /
    (slope.pressure + 0.063 * (1 + 0.34 * sse_i$WS2M))

  sse_i <- cbind(sse_i, ETP.harg.daily, ETP.pm.daily)
  n.tot <- length(sse_i[, 1])
  final.year <- sse_i$YEAR[n.tot]
  initial.year <- sse_i$YEAR[1]
  final.month <- sse_i$MM[n.tot]
  final.day <- sse_i$DD[n.tot]

  # see internal_functions.R for `find.week.int()`
  final.week <- find.week.int(final.day)
  n.years <- 1 + (final.year - initial.year)
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
