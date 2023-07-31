#' PlotData
#'
#' Plots Rainfall and potential evapotranspiration amounts using NASA POWER data.
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
#' Scatter plots of Rainfall and potential evapotranspiration accumulated at the 1-quart.month time scale.
#' @export
#' @importFrom nasapower get_power
#' @importFrom graphics par
#' @examplesIf interactive()
#' PlotData(lon = -47.3, lat = -22.87, start.date = "2021-12-28", end.date = "2022-12-31")
PlotData <- function(lon, lat, start.date, end.date) {
  if (is.na(as.Date(end.date, "%Y-%m-%d")) == TRUE || is.na(as.Date(start.date, "%Y-%m-%d")) == TRUE) {
    message("Recall Date format should be YYYY-MM-DD")
  } else {
    end.date.user <- as.Date(end.date, "%Y-%m-%d")
    start.date.user <- as.Date(start.date, "%Y-%m-%d")
    min.period <- end.date.user - start.date.user
    if (min.period < 365) {
      message("The difference between start.date and end.date must be of at least 1 year.
    Please, select a longer period.")
    } else {
      start.user.day <- as.numeric(format(start.date.user, format = "%d"))
      end.user.day <- as.numeric(format(end.date.user, format = "%d"))
      end.user.month <- as.numeric(format(end.date.user, format = "%m"))
      start.year <- as.numeric(format(start.date.user, format = "%Y"))
      start.month <- as.numeric(format(start.date.user, format = "%m"))
      if (start.user.day <= 7) {
        dif <- start.user.day - 1
        start.week <- 1
      }
      if (start.user.day > 7 & start.user.day <= 14) {
        dif <- start.user.day - 8
        start.week <- 2
      }
      if (start.user.day > 14 & start.user.day <= 21) {
        dif <- start.user.day - 15
        start.week <- 3
      }
      if (start.user.day >= 22) {
        dif <- start.user.day - 22
        start.week <- 4
      }
      start.date.protocal <- start.date.user - dif
      message("Just a sec. Downloading NASA POWER data and calculating the others parameters.")
      sse_i <- as.data.frame(get_power(
        community = "ag", lonlat = c(lon, lat),
        dates = c(start.date.protocal, end.date.user), temporal_api = "daily",
        pars = c(
          "T2M", "T2M_MAX", "T2M_MIN",
          "ALLSKY_SFC_SW_DWN", "WS2M", "RH2M", "PRECTOTCORR"
        )
      ))
      decli <- 23.45 * sin((360 * (sse_i$DOY - 80) / 365) * (pi / 180))
      lat.rad <- lat * (pi / 180)
      decli.rad <- decli * (pi / 180)
      hn.rad <- (acos(tan(decli.rad) * -tan(lat.rad)))
      hn.deg <- hn.rad * (180 / pi)
      N <- (2 * hn.deg) / 15
      dist.terra.sol <- 1 + (0.033 * cos((pi / 180) * (sse_i$DOY * (360 / 365))))
      Ra <- (37.6 * (dist.terra.sol^2)) * ((pi / 180) * hn.deg * sin(lat.rad) * sin(decli.rad) +
                                             (cos(lat.rad) * cos(decli.rad) * sin(hn.rad)))
      ####   Hargreaves&Samani
      ETP.harg.daily <- 0.0023 * (Ra * 0.4081633) * (sse_i$T2M_MAX - sse_i$T2M_MIN)^0.5 * (sse_i$T2M + 17.8)
      ###    Penman- Monteith-FAO
      es <- 0.6108 * exp((17.27 * sse_i$T2M) / (sse_i$T2M + 273.3))
      ea <- (sse_i$RH2M * es) / 100
      slope.pressure <- (4098 * es) / ((sse_i$T2M + 237.3)^2)
      Q0.ajust <- 0.75 * Ra
      Rn <- (1 - 0.2) * sse_i$ALLSKY_SFC_SW_DWN - (1.35 * (sse_i$ALLSKY_SFC_SW_DWN / Q0.ajust) - 0.35) * (0.35 - (0.14 * sqrt(ea))) * (5.67 * 10^-8) * (((sse_i$T2M^4) + (sse_i$T2M_MIN^4)) / 2)
      ETP.pm.daily <- (0.408 * slope.pressure * (Rn - 0.8) + 0.063 * (900 / (sse_i$T2M + 273)) * sse_i$WS2M * (es - ea)) / (slope.pressure + 0.063 * (1 + 0.34 * sse_i$WS2M))
      sse_i <- cbind(sse_i, ETP.harg.daily, ETP.pm.daily)
      n.tot <- length(sse_i[, 1])
      final.year <- sse_i$YEAR[n.tot]
      initial.year <- sse_i$YEAR[1]
      final.month <- sse_i$MM[n.tot]
      final.day <- sse_i$DD[n.tot]
      if (final.day <= 7) {
        final.week <- 1
      } else {
        if (final.day <= 14) {
          final.week <- 2
        } else {
          if (final.day <= 21) {
            final.week <- 3
          } else {
            final.week <- 4
          }
        }
      }
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
                                            sse_i$DD > 7 & sse_i$DD <= 14), 14:16])
        data.week3 <- colSums(sse_i[which(sse_i$YEAR == year &
                                            sse_i$MM == month &
                                            sse_i$DD > 14 & sse_i$DD <= 21), 14:16])
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
      rows <- which(data.week[, 3] == final.year & data.week[, 4] > final.month)
      n.rows <- length(rows)
      if (n.rows > 0) {
        data.week <- data.week[-c(rows), ]
      }
      rows <- which(data.week[, 3] == final.year & data.week[, 4] == final.month & data.week[, 5] > final.week)
      n.rows <- length(rows)
      if (n.rows > 0) {
        data.week <- data.week[-c(rows), ]
      }
      n <- length(which(data.week[, 3] <= final.year))
      data.week <- data.week[1:n, ]
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
      first.row <- which(data.week[, 3] == start.year & data.week[, 4] == start.month & data.week[, 5] == start.week)
      if (first.row > 1) {
        data.week <- data.week[-(1:(first.row - 1)), ]
      }
      par(mfrow = c(3, 1), mar = c(4, 4, 1.5, 2))
      ##### Rainfall
      plot(
           data.week[, 6]~data.week[, 9],xlim = c(1, 48),
           main="Checking suspicious data",
           xlab="quart.month",
           ylab="Rainfall (mm)"
           )
      ##### Potential Evapotranspiration (Hargreaves & Samani)
      plot(data.week[, 7]~data.week[, 9],xlim = c(1, 48),
           xlab="quart.month",
           ylab="PE (Hargreaves; mm)"
           )
      ##### Potential Evapotranspiration (FAO-56 Penman-Monteith)
      plot(data.week[, 8]~data.week[, 9],xlim = c(1, 48),
           xlab = "quart.month",
           ylab = "PE (FAO-56/PM; mm)"
           )
    }
  }
}
