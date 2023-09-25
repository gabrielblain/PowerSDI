#' Calculate Routine NASA-SPI and NASA-SPEI Estimates
#'
#' Calculates the \acronym{SPI} (Standardized Precipitation Index) and
#'   \acronym{SPEI} (Standardized Precipitation-Evapotranspiration Index) using
#'   \acronym{NASA} \acronym{POWER} data.
#'
#' @param lon longitude in decimal degrees.
#' @param lat latitude in decimal degrees.
#' @param start.date Date at each the calculation must start
#'  (\dQuote{YYYY-MM-DD}).
#' @param end.date Date at each the calculation must end (\dQuote{YYYY-MM-DD}).
#' @param PEMethod A character variable (\dQuote{HS} or \dQuote{PM}) defining
#'   the potential evapotranspiration method. Default is \dQuote{HS}.
#' @param distr A character variable (\dQuote{GEV} or \dQuote{GLO}) defining
#'   which distribution is used to calculate the \acronym{SPEI}. Default is
#'   \dQuote{GEV} (generalized extreme value) with \dQuote{GLO} (generalized
#'   logistic distributions) as an option.
#' @param parms Parameters required for calculating the \acronym{SPI} and
#'   \acronym{SPEI}.  It is provided by the \code{ScientSDI} function (DistPar).
#' @param TS Time scale on the \dQuote{quart.month} basis (integer values
#'   between 1 and 96).
#'
#' @return
#' A data frame with rainfall, potential evapotranspiration (PE),
#'   difference between rainfall and PE (in millimiters), the NASA-SPI and
#'   NASA_SPEI, and the SDI categories corresponding to each indices estimates.
#' @importFrom nasapower get_power
#' @importFrom lmom cdfgam cdfgev cdfglo pelgam pelgev pelglo quagev quagam
#'   quaglo samlmu
#' @importFrom graphics title
#' @importFrom stats cor median na.omit qnorm quantile runif shapiro.test
#' @examplesIf interactive()
#'
#' data("DistPar")
#' OperatSDI(
#'   lon = -47.3,
#'   lat = -22.67,
#'   start.date = "2023-06-01",
#'   end.date = "2023-06-30",
#'   parms = DistPar
#' )

OperatSDI <-
  function(lon,
           lat,
           start.date,
           end.date,
           PEMethod = "HS",
           distr = "GEV",
           parms,
           TS = 4) {

      PEMethod <- toupper(PEMethod)
  distr <- toupper(distr)
  if (PEMethod != "HS" && PEMethod != "PM") {
    stop("PEMethod should be set to either HS or PM.", call. = FALSE)
  }
  if (distr != "GEV" && distr != "GLO") {
    stop("distri should be set to either GEV or GLO.", call. = FALSE)
  }
  if (is.na(as.Date(start.date, "%Y-%m-%d")) ||
      is.na(as.Date(end.date, "%Y-%m-%d"))
      || TS < 1 || TS > 96 || isFALSE(all.equal(TS, as.integer(TS)))) {
    stop("Date format should be YYYY-MM-DD and\n",
         "TS must be an integer value ranging between 1 and 96",
         call. = FALSE)
  }
  start.date.user <- as.Date(start.date, "%Y-%m-%d")
  end.date.user <- as.Date(end.date, "%Y-%m-%d")

  final.year <- as.numeric(format(end.date.user, format = "%Y"))
  final.month <- as.numeric(format(end.date.user, format = "%m"))
  final.day <- as.numeric(format(end.date.user, format = "%d"))

  final.week <- calculate.week(final.day)

  if (final.week == 1 &&
      final.day != 7) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.week == 2 &
      final.day != 14) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.week == 3 &
      final.day != 21) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 1 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 3 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 5 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 7 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 8 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 10 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 12 &
      final.week == 4 &
      final.day != 31) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 4 &
      final.week == 4 &
      final.day != 30) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 6 &
      final.week == 4 &
      final.day != 30) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 9 &
      final.week == 4 &
      final.day != 30) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 11 &
      final.week == 4 &
      final.day != 30) {
    stop("the last day of the period must be 1, 7, 14, 21 or (28/29 feb) or 30/31",
         call. = FALSE)
  }
  if (final.month == 2 & final.week == 4) {

    leap = (final.year %% 4 == 0 &
              (final.year %% 100 != 0 | final.year %% 400 == 0))
    if (isFALSE(leap)) {
      if (final.day != 28) {
        stop("the last day of the period must be 1, 7, 14, 21 or 28",
             call. = FALSE)
      }
    } else {
      if (final.day != 29) {
        stop("Leap year: the last day of the period must be 1, 7, 14, 21 or 29",
             call. = FALSE)
      }
    }
  }
  mim.date.fit <- (end.date.user - start.date.user) + 1

  if (mim.date.fit < 7) {
    stop("Time difference between end.date and start.date must be equal to or longer than 7 days",
         call. = FALSE)

  if (TS > 1) {
    start.date.user <- start.date.user - (10 * TS)
  }

  start.day <- as.numeric(format(start.date.user, format = "%d"))
  start.month <- as.numeric(format(start.date.user, format = "%m"))
  start.year <- as.numeric(format(start.date.user, format = "%Y"))

  }
  else {
    start.week <- calculate.week(start.day) # see internal_functions.R
    dif <- calculate.dif(start.week, start.day) # see internal_functions.R

    start.date.user <- start.date.user - dif
    start.day <- as.numeric(format(start.date.user, format = "%d"))
    start.year <- as.numeric(format(start.date.user, format = "%Y"))
    start.month <- as.numeric(format(start.date.user, format = "%m"))
    message("Calculating...")
    if (PEMethod == "HS") {
      sse_i <- as.data.frame(get_power(
        community = "ag",
        lonlat = c(lon, lat),
        dates = c(start.date.user,
                  end.date.user),
        temporal_api = "daily",
        pars = c("T2M",
                 "T2M_MAX", "T2M_MIN", "PRECTOTCORR")
      ))
      decli <- 23.45 * sin((360 * (sse_i$DOY - 80) / 365) *
                             (0.01745329))
      lat.rad <- lat * (0.01745329)
      decli.rad <- decli * (0.01745329)
      hn.rad <- (acos(tan(decli.rad) * -tan(lat.rad)))
      hn.deg <- hn.rad * (57.29578)
      N <- (2 * hn.deg) / 15
      dist.terra.sol <- 1 + (0.033 * cos((0.01745329) * (sse_i$DOY *
                                                         (0.9863014))))
      Ra <- (37.6 * (dist.terra.sol ^ 2)) * ((0.01745329) *
                                               hn.deg * sin(lat.rad) * sin(decli.rad) + (cos(lat.rad) *
                                                                                           cos(decli.rad) * sin(hn.rad)))
      ETP.harg.daily <- 0.0023 * (Ra * 0.4081633) * (sse_i$T2M_MAX -
                                                       sse_i$T2M_MIN) ^
        0.5 * (sse_i$T2M + 17.8)
      sse_i <- cbind(sse_i, ETP.harg.daily)
      n.tot <- length(sse_i[, 1])
      final.year <- sse_i$YEAR[n.tot]
      final.month <- sse_i$MM[n.tot]
      final.day <- sse_i$DD[n.tot]
      n.years <- 1 + (final.year - start.year)
      total.nweeks <- 48 * n.years
      a <- 1
      b <- 2
      c <- 3
      d <- 4
      data.week <- matrix(NA, total.nweeks, 7)
      month <- start.month
      year <- start.year
      while (year <= final.year || month <= final.month) {
        data.week1 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD <= 7),
                                    11:12])
        data.week2 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 7 &
                                            sse_i$DD <= 14), 11:12])
        data.week3 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 14 &
                                            sse_i$DD <= 21), 11:12])
        data.week4 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 21),
                                    11:12])
        data.week[a, ] <- c(lon, lat, year, month, 1,
                            data.week1)
        data.week[b, ] <- c(lon, lat, year, month, 2,
                            data.week2)
        data.week[c, ] <- c(lon, lat, year, month, 3,
                            data.week3)
        data.week[d, ] <- c(lon, lat, year, month, 4,
                            data.week4)
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
    }
    if (PEMethod == "PM") {
      sse_i <- as.data.frame(get_power(
        community = "ag",
        lonlat = c(lon, lat),
        dates = c(start.date.user,
                  end.date.user),
        temporal_api = "daily",
        pars = c(
          "T2M",
          "T2M_MAX",
          "T2M_MIN",
          "ALLSKY_SFC_SW_DWN",
          "WS2M",
          "RH2M",
          "PRECTOTCORR"
        )
      ))
      decli <- 23.45 * sin((360 * (sse_i$DOY - 80) / 365) *
                             (0.01745329))
      lat.rad <- lat * (0.01745329)
      decli.rad <- decli * (0.01745329)
      hn.rad <- (acos(tan(decli.rad) * -tan(lat.rad)))
      hn.deg <- hn.rad * (57.29578)
      N <- (2 * hn.deg) / 15
      dist.terra.sol <- 1 + (0.033 * cos((0.01745329) * (sse_i$DOY *
                                                         (0.9863014))))
      Ra <- (37.6 * (dist.terra.sol ^ 2)) * ((0.01745329) *
                                               hn.deg * sin(lat.rad) * sin(decli.rad) + (cos(lat.rad) *
                                                                                           cos(decli.rad) * sin(hn.rad)))
      es <- 0.6108 * exp((17.27 * sse_i$T2M) / (sse_i$T2M +
                                                  273.3))
      ea <- (sse_i$RH2M * es) / 100
      slope.pressure <- (4098 * es) / ((sse_i$T2M + 237.3) ^ 2)
      Q0.ajust <- 0.75 * Ra
      Rn <- (1 - 0.2) * sse_i$ALLSKY_SFC_SW_DWN - (1.35 *
                                                     (sse_i$ALLSKY_SFC_SW_DWN /
                                                        Q0.ajust) - 0.35) *
        (0.35 - (0.14 * sqrt(ea))) * (0.0000000567) *
        (((sse_i$T2M ^ 4) + (sse_i$T2M_MIN ^ 4)) / 2)
      ETP.pm.daily <- (0.408 * slope.pressure * (Rn -
                                                   0.8) + 0.063 * (900 /
                                                                     (sse_i$T2M + 273)) * sse_i$WS2M *
                         (es - ea)) / (slope.pressure + 0.063 * (1 + 0.34 *
                                                                   sse_i$WS2M))
      sse_i <- cbind(sse_i, ETP.pm.daily)
      n.tot <- length(sse_i[, 1])
      final.year <- sse_i$YEAR[n.tot]
      final.month <- sse_i$MM[n.tot]
      final.day <- sse_i$DD[n.tot]
      n.years <- 1 + (final.year - start.year)
      total.nweeks <- 48 * n.years
      a <- 1
      b <- 2
      c <- 3
      d <- 4
      data.week <- matrix(NA, total.nweeks, 7)
      month <- start.month
      year <- start.year
      while (year <= final.year || month <= final.month) {
        data.week1 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD <= 7),
                                    14:15])
        data.week2 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 7 &
                                            sse_i$DD <= 14), 14:15])
        data.week3 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 14 &
                                            sse_i$DD <= 21), 14:15])
        data.week4 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year & sse_i$MM == month & sse_i$DD > 21),
                                    14:15])
        data.week[a, ] <- c(lon, lat, year, month, 1,
                            data.week1)
        data.week[b, ] <- c(lon, lat, year, month, 2,
                            data.week2)
        data.week[c, ] <- c(lon, lat, year, month, 3,
                            data.week3)
        data.week[d, ] <- c(lon, lat, year, month, 4,
                            data.week4)
        month <- month + 1
        if (month > 12) {
          year <- year + 1
          month <- 1
        }
        if (year == final.year & month > final.month) {
          break
        }
        a <- a + 4
        b <- b + 4
        c <- c + 4
        d <- d + 4
      }
    }
    data.week <- na.omit(data.week)
    rows <- which(data.week[, 3] == final.year & data.week[,
                                                           4] > final.month)
    n.rows <- length(rows)
    if (n.rows > 0) {
      data.week <- as.matrix(data.week[-c(rows), , drop = FALSE])
    }
    rows <- which(data.week[, 3] == final.year & data.week[,
                                                           4] == final.month & data.week[, 5] > final.week)
    n.rows <- length(rows)
    if (n.rows > 0) {
      data.week <- as.matrix(data.week[-c(rows), , drop = FALSE])
    }
    n <- length(data.week[, 1])
    quart.month <- matrix(NA, n, 1)
    for (i in 1:n) {
      if (data.week[i, 4] == 1 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 1
      }
      if (data.week[i, 4] == 1 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 2
      }
      if (data.week[i, 4] == 1 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 3
      }
      if (data.week[i, 4] == 1 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 4
      }
      if (data.week[i, 4] == 2 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 5
      }
      if (data.week[i, 4] == 2 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 6
      }
      if (data.week[i, 4] == 2 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 7
      }
      if (data.week[i, 4] == 2 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 8
      }
      if (data.week[i, 4] == 3 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 9
      }
      if (data.week[i, 4] == 3 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 10
      }
      if (data.week[i, 4] == 3 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 11
      }
      if (data.week[i, 4] == 3 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 12
      }
      if (data.week[i, 4] == 4 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 13
      }
      if (data.week[i, 4] == 4 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 14
      }
      if (data.week[i, 4] == 4 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 15
      }
      if (data.week[i, 4] == 4 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 16
      }
      if (data.week[i, 4] == 5 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 17
      }
      if (data.week[i, 4] == 5 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 18
      }
      if (data.week[i, 4] == 5 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 19
      }
      if (data.week[i, 4] == 5 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 20
      }
      if (data.week[i, 4] == 6 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 21
      }
      if (data.week[i, 4] == 6 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 22
      }
      if (data.week[i, 4] == 6 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 23
      }
      if (data.week[i, 4] == 6 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 24
      }
      if (data.week[i, 4] == 7 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 25
      }
      if (data.week[i, 4] == 7 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 26
      }
      if (data.week[i, 4] == 7 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 27
      }
      if (data.week[i, 4] == 7 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 28
      }
      if (data.week[i, 4] == 8 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 29
      }
      if (data.week[i, 4] == 8 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 30
      }
      if (data.week[i, 4] == 8 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 31
      }
      if (data.week[i, 4] == 8 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 32
      }
      if (data.week[i, 4] == 9 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 33
      }
      if (data.week[i, 4] == 9 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 34
      }
      if (data.week[i, 4] == 9 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 35
      }
      if (data.week[i, 4] == 9 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 36
      }
      if (data.week[i, 4] == 10 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 37
      }
      if (data.week[i, 4] == 10 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 38
      }
      if (data.week[i, 4] == 10 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 39
      }
      if (data.week[i, 4] == 10 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 40
      }
      if (data.week[i, 4] == 11 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 41
      }
      if (data.week[i, 4] == 11 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 42
      }
      if (data.week[i, 4] == 11 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 43
      }
      if (data.week[i, 4] == 11 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 44
      }
      if (data.week[i, 4] == 12 & data.week[i, 5] == 1) {
        quart.month[i, 1] <- 45
      }
      if (data.week[i, 4] == 12 & data.week[i, 5] == 2) {
        quart.month[i, 1] <- 46
      }
      if (data.week[i, 4] == 12 & data.week[i, 5] == 3) {
        quart.month[i, 1] <- 47
      }
      if (data.week[i, 4] == 12 & data.week[i, 5] == 4) {
        quart.month[i, 1] <- 48
      }
    }
    data.week <- cbind(data.week, quart.month)
    first.row <- which(data.week[, 3] == start.year & data.week[,
                                                                4] == start.month & data.week[, 5] == start.week)
    if (first.row > 1) {
      data.week <- as.matrix(data.week[-c(1:(first.row -
                                               1)), , drop = FALSE])
    }
    n <- length(data.week[, 1])
    data.at.timescale <- matrix(NA, (n - (TS - 1)), 7)
    final.point <- n - (TS - 1)
    if (TS > 1) {
      point <- 1
      a <- 1
      b <- TS
      c <- 1
      data.at.timescale[c, ] <- c(data.week[b, 1:4], data.week[b,
                                                               8], colSums(data.week[a:b, 6:7]))
      point <- point + 1
      a <- a + 1
      b <- b + 1
      c <- c + 1
      while (point <= final.point) {
        data.at.timescale[c, ] <- c(data.week[b, 1:4],
                                    data.week[b, 8], colSums(data.week[a:b, 6:7]))
        point <- point + 1
        a <- a + 1
        b <- b + 1
        c <- c + 1
      }
    }
    else {
      data.at.timescale[, ] <- as.matrix(c(data.week[,
                                                     1:4], data.week[, 8], data.week[, 6:7]))
    }
    data.at.timescale <- as.matrix(cbind(data.at.timescale,
                                         (data.at.timescale[, 6] - data.at.timescale[, 7])))
    n.weeks <- length(data.at.timescale[, 1])
    pos <- 1
    SDI <- matrix(NA, n.weeks, 2)
    parameters <- as.data.frame(parms[which(parms[, 1] ==
                                              lon & parms[, 2] == lat & parms[, 13] == TS), ])
    if (length(parameters[, 1]) == 0) {
      stop(
        "It seems that you don't have the distributions' parameters for this",
        "local and time scale(TS).\n",
        "You must first run `ScientSDI()` function.",
        call. = FALSE)
    }
    else {
      if (distr == "GEV") {
        for (pos in 1:n.weeks) {
          week <- data.at.timescale[pos, 5]
          par <- as.numeric(parameters[week, ])
          prob <- (par[6] + (1 - par[6])) * cdfgam(data.at.timescale[pos,
                                                                     6], c(par[4], par[5]))
          if (!is.na(prob) & prob < 0.001351) {
            prob <- 0.001351
          }
          if (!is.na(prob)  & prob > 0.998649) {
            prob <- 0.998649
          }
          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
          if (PEMethod == "HS") {
            prob <- cdfgev(data.at.timescale[pos, 8],
                           c(par[7], par[8], par[9]))
          }
          if (PEMethod == "PM") {
            prob <- cdfgev(data.at.timescale[pos, 8],
                           c(par[10], par[11], par[12]))
          }
          if (!is.na(prob)  & prob < 0.001351) {
            prob <- 0.001351
          }
          if (!is.na(prob)  & prob > 0.998649) {
            prob <- 0.998649
          }
          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      }
      if (distr == "GLO") {
        for (pos in 1:n.weeks) {
          week <- data.at.timescale[pos, 5]
          par <- as.numeric(parameters[week, ])
          prob <- (par[6] + (1 - par[6])) * cdfgam(data.at.timescale[pos,
                                                                     6], c(par[4], par[5]))
          if (!is.na(prob)  & prob < 0.001351) {
            prob <- 0.001351
          }
          if (!is.na(prob)  & prob > 0.998649) {
            prob <- 0.998649
          }
          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
          if (PEMethod == "HS") {
            prob <- cdfglo(data.at.timescale[pos, 8],
                           c(par[7], par[8], par[9]))
          }
          if (PEMethod == "PM") {
            prob <- cdfglo(data.at.timescale[pos, 8],
                           c(par[10], par[11], par[12]))
          }
          if (!is.na(prob) & prob < 0.001351) {
            prob <- 0.001351
          }
          if (!is.na(prob) & prob > 0.998649) {
            prob <- 0.998649
          }
          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      }
      categories <- matrix(NA, n.weeks, 2)
      for (i in 1:n.weeks) {
        if (SDI[i, 1] <= -2 & !is.na(SDI[i, 1])) {
          categories[i, 1] <- "ext.dry"
        }
        else {
          if (SDI[i, 1] <= -1.5 & !is.na(SDI[i, 1])) {
            categories[i, 1] <- "sev.dry"
          }
          else {
            if (SDI[i, 1] <= -1 & !is.na(SDI[i, 1])) {
              categories[i, 1] <- "mod.dry"
            }
            else {
              if (SDI[i, 1] <= 1 & !is.na(SDI[i, 1])) {
                categories[i, 1] <- "Normal"
              }
              else {
                if (SDI[i, 1] <= 1.5 & !is.na(SDI[i,
                                                  1])) {
                  categories[i, 1] <- "mod.wet"
                }
                else {
                  if (SDI[i, 1] <= 2 & !is.na(SDI[i,
                                                  1])) {
                    categories[i, 1] <- "sev.wet"
                  }
                  else {
                    if (SDI[i, 1] > 2 & !is.na(SDI[i,
                                                   1])) {
                      categories[i, 1] <- "ext.wet"
                    }
                  }
                }
              }
            }
          }
        }
        if (SDI[i, 2] <= -2 & !is.na(SDI[i, 2])) {
          categories[i, 2] <- "ext.dry"
        }
        else {
          if (SDI[i, 2] <= -1.5 & !is.na(SDI[i, 2])) {
            categories[i, 2] <- "sev.dry"
          }
          else {
            if (SDI[i, 2] <= -1 & !is.na(SDI[i, 2])) {
              categories[i, 2] <- "mod.dry"
            }
            else {
              if (SDI[i, 2] <= 1 & !is.na(SDI[i, 2])) {
                categories[i, 2] <- "Normal"
              }
              else {
                if (SDI[i, 2] <= 1.5 & !is.na(SDI[i,
                                                  2])) {
                  categories[i, 2] <- "mod.wet"
                }
                else {
                  if (SDI[i, 2] <= 2 & !is.na(SDI[i,
                                                  2])) {
                    categories[i, 2] <- "sev.wet"
                  }
                  else {
                    if (SDI[i, 2] > 2 & !is.na(SDI[i,
                                                   2])) {
                      categories[i, 2] <- "ext.wet"
                    }
                  }
                }
              }
            }
          }
        }
      }
      SDI <- cbind(data.at.timescale, SDI)
      SDI.final <- data.frame(SDI, categories)
      colnames(SDI.final) <- c("Lon", "Lat", "Year", "Month",
                               "quart.month", "Rain", "PE", "PPE", "SPI", "SPEI",
                               "Categ.SPI", "Categ.SPEI")
      show.date <- as.Date(start.date, "%Y-%m-%d")
      return(SDI.final)
    }
  }
  if (is.na(sum(SDI.final[, 10]))) {
    message("Check the original data, it might have gaps")
  }
  message("Considering the selected TS, the calculations started on:")
  print(start.date.user)
  }
