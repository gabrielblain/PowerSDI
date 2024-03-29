#' Calculate Routine NASA-SPI and NASA-SPEI Estimates
#'
#' Calculates the \acronym{SPI} (Standardized Precipitation Index) and
#'   \acronym{SPEI} (Standardized Precipitation-Evapotranspiration Index) using
#'   \acronym{NASA} \acronym{POWER} data.
#'
#' @param lon longitude in decimal degrees.
#' @param lat latitude in decimal degrees.
#' @param start.date Date at each time when the calculation must start
#'  (\dQuote{YYYY-MM-DD}).
#' @param end.date Date at each time when the calculation must end
#'  (\dQuote{YYYY-MM-DD}).
#' @param PEMethod A character variable (\dQuote{HS} (Hargreaves & Samani) or
#'   \dQuote{PM} (Penman-Monteith) defining the potential evapotranspiration
#'   method.  Default is \dQuote{HS}.
#' @param distr A character variable (\dQuote{GEV} or \dQuote{GLO}) defining
#'   which distribution is used to calculate the \acronym{SPEI}. Default is
#'   \dQuote{GEV} (generalized extreme value) with \dQuote{GLO} (generalized
#'   logistic distributions) as an option.
#' @param parms Parameters required for calculating the \acronym{SPI} and
#'   \acronym{SPEI}.  It is provided by the \code{\link{ScientSDI}} function's
#'   \code{DistPar}.
#' @param TS Time scale on the \dQuote{quart.month} basis (integer values
#'   between 1 and 96).
#'
#' @return
#' A data frame with six columns
#' \itemize{
#'   \item rainfall,
#'   \item potential evapotranspiration (PE),
#'   \item difference between rainfall and PE (in millimiters),
#'   \item the NASA-SPI,
#'   \item the NASA-SPEI and
#'   \item the SDI categories corresponding to each indices estimates.
#'   }
#'
#' @importFrom lmom cdfgam
#' @importFrom stats na.omit qnorm
#'
#' @examplesIf interactive()
#'
#' # This example is not run as it requires data to be downloaded from an API,
#' #  which may fail.  It also uses data included in this package, "DistPar" for
#' #  `parms` here
#'
#' OperatSDI(
#'   lon = -47.3,
#'   lat = -22.67,
#'   start.date = "2023-06-01",
#'   end.date = "2023-06-30",
#'   PEMethod = "HS",
#'   distr = "GEV",
#'   parms = DistPar,
#'   TS = 4)
#' @export

OperatSDI <-
  function(lon,
           lat,
           start.date,
           end.date,
           PEMethod = "HS",
           distr = "GEV",
           parms,
           TS = 4L) {
    PEMethod <- toupper(PEMethod)
    distr <- toupper(distr)

    if (PEMethod != "HS" && PEMethod != "PM") {
      stop("PEMethod should be set to either `HS` or `PM`.", call. = FALSE)
    }

    if (missing(parms)) {
      stop(
        "It seems that you don't have the distributions' parameters for this ",
        "local and time scale (`TS`).\n",
        "You must first run the `ScientSDI()` function.",
        call. = FALSE
      )
    }

    distr <- check.distr(distr)
    check.TS(TS)

    dates <- check.dates(c(start.date, end.date))
    start.date.user <- dates[[1]]
    end.date.user <- dates[[2]]

    final.year <- lubridate::year(end.date.user)
    final.month <- lubridate::month(end.date.user)
    final.day <- lubridate::day(end.date.user)

    final.week <- find.week.int(final.day)

    # see bottom of this file for this function
    check.final.agreement(final.year, final.month, final.week, final.day)

    if (((end.date.user - start.date.user) + 1) < 7) {
      stop(
        "Time difference between `end.date` and `start.date` ",
        "must be equal to or longer than 7 days.",
        call. = FALSE
      )
    }

    if (TS > 1) {
      start.date.user <- start.date.user - (10 * TS)
    }

    start.year <- lubridate::year(start.date.user)
    start.month <- lubridate::month(start.date.user)
    start.day <- lubridate::day(start.date.user)
    start.week <- find.week.int(start.day)

    dif <- calculate.dif(start.week, start.day)
    start.date.user <- as.Date(start.date.user - dif)

    start.year <- lubridate::year(start.date.user)
    start.month <- lubridate::month(start.date.user)
    start.day <- lubridate::day(start.date.user)

    n.years <- 1 + (final.year - start.year)
    total.nweeks <- 48 * n.years

    message("Calculating...")


    if (PEMethod == "HS") {
      sse_i <-
        get_sdi_power_data(lon,
                           lat,
                           start.date.user,
                           end.date.user,
                           PEMethod = "HS")

      # see calculation functions for the following functions
      ETP.harg.daily <-
        calc.ETP.daily(
          J = sse_i$DOY,
          lat = lat,
          tavg = sse_i$T2M,
          tmax = sse_i$T2M_MAX,
          tmin = sse_i$T2M_MIN,
          method = PEMethod
        )

      sse_i <- cbind(sse_i, ETP.harg.daily)

      a <- 1
      b <- 2
      c <- 3
      d <- 4
      data.week <- matrix(NA, total.nweeks, 7)
      month <- start.month
      year <- start.year
      while (year <= final.year || month <= final.month) {
        data.week1 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD <= 7),
                                    11:12])
        data.week2 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 7 &
                                            sse_i$DD <= 14), 11:12])
        data.week3 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 14 &
                                            sse_i$DD <= 21), 11:12])
        data.week4 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 21),
                                    11:12])
        data.week[a,] <- c(lon, lat, year, month, 1,
                           data.week1)
        data.week[b,] <- c(lon, lat, year, month, 2,
                           data.week2)
        data.week[c,] <- c(lon, lat, year, month, 3,
                           data.week3)
        data.week[d,] <- c(lon, lat, year, month, 4,
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
    } else {
      sse_i <-
        get_sdi_power_data(lon,
                           lat,
                           start.date.user,
                           end.date.user,
                           PEMethod = "PM")
      # see calculation functions for the following functions
      ETP.pm.daily <- calc.ETP.daily(
        J = sse_i$DOY,
        lat = lat,
        tavg = sse_i$T2M,
        tmax = sse_i$T2M_MAX,
        tmin = sse_i$T2M_MIN,
        rh = sse_i$RH2M,
        wind = sse_i$WS2M,
        rad = sse_i$ALLSKY_SFC_SW_DWN,
        method = PEMethod
      )

      sse_i <- cbind(sse_i, ETP.pm.daily)

      a <- 1
      b <- 2
      c <- 3
      d <- 4
      data.week <- matrix(NA, total.nweeks, 7)
      month <- start.month
      year <- start.year
      while (year <= final.year || month <= final.month) {
        data.week1 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD <= 7),
                                    14:15])
        data.week2 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 7 &
                                            sse_i$DD <= 14), 14:15])
        data.week3 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 14 &
                                            sse_i$DD <= 21), 14:15])
        data.week4 <- colSums(sse_i[which(sse_i$YEAR ==
                                            year &
                                            sse_i$MM == month &
                                            sse_i$DD > 21),
                                    14:15])
        data.week[a,] <- c(lon, lat, year, month, 1,
                           data.week1)
        data.week[b,] <- c(lon, lat, year, month, 2,
                           data.week2)
        data.week[c,] <- c(lon, lat, year, month, 3,
                           data.week3)
        data.week[d,] <- c(lon, lat, year, month, 4,
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
    rows <- which(data.week[, 3] == final.year &
                    data.week[, 4] > final.month)
    n.rows <- length(rows)
    if (n.rows > 0) {
      data.week <- as.matrix(data.week[-c(rows), , drop = FALSE])
    }
    rows <- which(data.week[, 3] == final.year &
                    data.week[, 4] == final.month &
                    data.week[, 5] > final.week)
    n.rows <- length(rows)
    if (n.rows > 0) {
      data.week <- as.matrix(data.week[-c(rows), , drop = FALSE])
    }

    # see internal_functions.R for `find.quart.month.int()`
    data.week <-
      cbind(data.week, find.quart.month.int(x = data.week))

    first.row <- which(data.week[, 3] == start.year &
                         data.week[, 4] == start.month &
                         data.week[, 5] == start.week)
    if (first.row > 1) {
      data.week <-
        as.matrix(data.week[-c(1:(first.row - 1)), , drop = FALSE])
    }

    n <- nrow(data.week)
    data.at.timescale <- matrix(NA, (n - (TS - 1)), 7)
    final.point <- n - (TS - 1)
    if (TS > 1) {
      point <- 1
      a <- 1
      b <- TS
      c <- 1
      data.at.timescale[c,] <- c(data.week[b, 1:4],
                                 data.week[b, 8],
                                 colSums(data.week[a:b, 6:7]))
      point <- point + 1
      a <- a + 1
      b <- b + 1
      c <- c + 1
      while (point <= final.point) {
        data.at.timescale[c,] <- c(data.week[b, 1:4],
                                   data.week[b, 8],
                                   colSums(data.week[a:b, 6:7]))
        point <- point + 1
        a <- a + 1
        b <- b + 1
        c <- c + 1
      }
    } else {
      data.at.timescale[,] <- as.matrix(c(data.week[, 1:4],
                                          data.week[, 8],
                                          data.week[, 6:7]))
    }
    data.at.timescale <- as.matrix(cbind(
      data.at.timescale,
      (data.at.timescale[, 6] - data.at.timescale[, 7])
    ))
    n.weeks <- nrow(data.at.timescale)
    pos <- 1
    SDI <- matrix(NA, n.weeks, 2)
    parameters <- as.data.frame(parms[which(parms[, 1] == lon &
                                              parms[, 2] == lat &
                                              parms[, 13] == TS), ])

    # calc.qnorm() is in this file, below
    SDI <- calc.qnorm(distr,
                      data.at.timescale,
                      parameters,
                      n.weeks,
                      SDI,
                      PEMethod)

    categories <- matrix(NA, n.weeks, 2)

    # see internal_functions.R for find.category()
    categories[, 1] <- find.category(x = SDI[, 1])
    categories[, 2] <- find.category(x = SDI[, 2])

    SDI <- cbind(data.at.timescale, SDI)
    SDI.final <- data.frame(SDI, categories)
    colnames(SDI.final) <- c(
      "Lon",
      "Lat",
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
    if (anyNA(SDI.final[, 10])) {
      message("Check the original data, it might have gaps.")
    }
    message("Considering the selected `TS`, '", TS,
            "', the calculations started on: ",
            start.date.user)
    return(SDI.final)
  }

#' Calculate Quantile Norm, qnorm, Values
#'
#' Calculates quantile values for SDI.
#'
#' @return A matrix, SDI, with a new column of qnorm values
#' @keywords Internal
#' @noRd

calc.qnorm <-
  function(distr,
           data.at.timescale,
           parameters,
           n.weeks,
           SDI,
           PEMethod) {
    if (distr == "GEV") {
      for (pos in seq_len(n.weeks)) {
        week <- data.at.timescale[pos, 5]
        par <- as.numeric(parameters[week, ])
        # see internal_functions.R for `adjust.prob()` and `set.PEMethod.prob`
        prob <-
          adjust.prob((par[6] + (1 - par[6])) *
                        cdfgam(data.at.timescale[pos, 6], c(par[4], par[5])))

        SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)

        prob <-
          set.PEMethod.prob(
            distr,
            PEMethod,
            data.at.timescale,
            dat_row = pos,
            dat_col = 8,
            par = par,
            p1 = 7,
            p2 = 8,
            p3 = 9,
            p4 = 10,
            p5 = 11,
            p6 = 12
          )

        prob <- adjust.prob(prob)

        SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
        pos <- pos + 1
      }
    } else {
      for (pos in seq_len(n.weeks)) {
        week <- data.at.timescale[pos, 5]
        par <- as.numeric(parameters[week, ])
        prob <-
          (par[6] + (1 - par[6])) *
          cdfgam(data.at.timescale[pos, 6], c(par[4], par[5]))

        # see internal_functions.R for `adjust.prob()` and `set.PEMethod.prob`
        prob <- adjust.prob(prob)
        SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
        prob <-
          set.PEMethod.prob(
            distr,
            PEMethod,
            data.at.timescale,
            dat_row = pos,
            dat_col = 8,
            par = par,
            p1 = 7,
            p2 = 8,
            p3 = 9,
            p4 = 10,
            p5 = 11,
            p6 = 12
          )
        prob <- adjust.prob(prob)

        SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
        pos <- pos + 1
      }
    }
    return(SDI)
  }

#' Check for Final Day and Week Agreement
#'
#' Ensures that the final day of a period is an acceptable value for the last
#' day of a period for the purposes of this package.
#'
#' @param final.week The last week of the period as calculated by
#'   find.week.int()
#' @param final.day The last day of the period as provided by the user.
#'
#' @examples
#' # passes
#' final.week <- 1
#' final.day <- 7
#'
#' # doesn't pass
#' final.week <- 2
#' final.day <- 4
#'
#' @keywords Internal
#' @noRd

check.final.agreement <-
  function(final.year,
           final.month,
           final.week,
           final.day) {
    msg <-
      "The last day of the period must be 7, 14, 21 or (28/29 Feb) or 30/31."

    if (final.week == 1 & final.day != 7) {
      stop(msg,
           call. = FALSE)
    } else if (final.week == 2 & final.day != 14) {
      stop(msg,
           call. = FALSE)
    } else if (final.week == 3 & final.day != 21) {
      stop(msg,
           call. = FALSE)
    } else if (final.month %in% c(1, 3, 5, 7, 8, 10, 12) &
               final.week == 4 & final.day != 31) {
      stop(msg,
           call. = FALSE)
    } else if (final.month %in% c(4, 6, 9, 11) &
               final.week == 4 & final.day != 30) {
      stop(msg,
           call. = FALSE)
    } else if (final.month == 2) {
      if (isFALSE(lubridate::leap_year(final.year)) &
          final.week == 4 & final.day != 28) {
        stop(msg,
             call. = FALSE)
      } else if (lubridate::leap_year(final.year) &
                 final.week == 4 & final.day != 29) {
        stop(msg,
             call. = FALSE)
      }
    }
    return(invisible(NULL))
  }
