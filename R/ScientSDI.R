#' Estimate parameters of Gamma, Generalized Extreme Value, or Generalized Logistic Distributions
#'
#' Verifies concepts expected from SDI.  The first step of the \acronym{SPI} and
#'   \acronym{SPEI} algorithms is to calculate the cumulative probabilities of
#'   their input variables (Guttman 1999).  Function estimates the parameters of
#'   the gamma, generalized extreme value (GEV), or generalized logistic
#'   distributions (GLO) through the L-moments method are provided.  This
#'   function also allows users to remove suspicious values from the data
#'   sample.
#'
#' @param lon
#' longitude in decimal degrees: (+) Eastern Hemisphere, (-) Western Hemisphere.
#' @param lat
#' latitude in decimal degrees: (+) Northern hemisphere, (-) Southern
#'   Hemisphere.
#' @param start.date
#' date at which the indices estimates should start. Format: "YYYY-MM-DD".
#' @param end.date
#' date at which the indices estimates should end. Format: "YYYY-MM-DD".
#' @param distr
#' A character variable ("GEV" or "GLO") defining the distribution to calculate
#'   the \acronym{SPEI}. Default is "GEV".
#' @param TS
#' Time scale on the quart.month basis (integer values between 1 and 96).
#'   Default is 4.
#' @param Good
#' A character variable ("Yes" or "No") to calculate or not the goodness-of-fit
#'   and normality tests. Default is "No".
#' @param sig.level
#' A numeric variable (between 0.90 and 0.95) defining the significance level
#'   for parameter Good. Default is "0.95".
#' @param RainUplim
#' Optional. Upper limit in millimeters from which rainfall values larger than
#'   it will be removed. Default is \code{NULL}.
#' @param RainLowlim
#' Optional. Lower limit in millimeters from which rainfall values smaller than
#'   it will be removed. Default is \code{NULL}.
#' @param PEUplim
#' Optional. Upper limit in millimeters from which evapotranspiration values
#'   larger than it will be removed. Default is \code{NULL}.
#' @param PELowlim
#' Optional. Lower limit in millimeters from which evapotranspiration values
#'   smaller than it will be removed. Default is \code{NULL}.
#' @return
#' A list with data calculated at the time scale selected by the user.
#' If \code{Good="Yes"}, this list includes:
#' \describe{
#'   \item{SDI}{The NASA-SPI, NASA-SPEI.HS and NASA-SPEI.PM.}
#'   \item{DistPar}{The parameters of the distributions (gamma and GEV) used to
#'   calculate the indices.}
#'   \item{GoodFit}{The Lilliefors and Anderson-Darling tests goodness-of-fit
#'   tests.}
#'   \item{Normality}{The outcomes of the two normality checking procedures (Wu
#'   et al., 2007 and Stagge et al., 2015).}
#'  }
#'
#' If \code{Good="No"}, this list includes \acronym{SDI} and DistPar.
#'
#' This function also presents other data (in millimiters) calculated from the
#'   \acronym{NASA} \acronym{POWER} project:
#' \itemize{
#'   \item Rainfall amounts (Rain).
#'   \item Potential evapotranspiration values estimated through the Hargreaves
#'    and Samani method (PEHS).
#'   \item Potential evapotranspiration values estimated through the FAO-56
#'    Penman-Monteith method (PEPM).
#'   \item The difference between rainfall and potential evapotranspiration
#'   (PPEHS and PPEPM).
#'   }
#' @export
#' @importFrom nasapower get_power
#' @importFrom stats cor median na.omit qnorm quantile runif shapiro.test
#' @importFrom utils install.packages menu write.table
#' @examplesIf interactive()
#' ScientSDI(
#'   lon = -47.3,
#'   lat = -22.87,
#'   start.date = "2015-01-01",
#'   end.date = "2022-12-31",
#'   TS = 1,
#'   Good = "no"
#' )
#'
#' @references
#'  Guttman, N.B., 1999. Accepting the standardized precipitation
#'    index: a calculation algorithm 1. JAWRA Journal of the American Water
#'    Resources Association, 35(2), pp.311-322.
#'
#'  Stagge, J.H., Tallaksen, L.M., Gudmundsson, L., Van Loon, A.F. and Stahl,
#'    K., 2015. Candidate distributions for climatological drought indices (SPI
#'    and SPEI). International Journal of Climatology, 35(13), pp.4027-4040.
#'
#'  Wu, H., Svoboda, M.D., Hayes, M.J., Wilhite, D.A. and Wen, F., 2007.
#'   Appropriate application of the standardized precipitation index in arid
#'   locations and dry seasons. International Journal of Climatology: A Journal
#'   of the Royal Meteorological Society, 27(1), pp.65-79.

ScientSDI <-
  function(lon,
           lat,
           start.date,
           end.date,
           distr = "GEV",
           TS = 4,
           Good = "No",
           sig.level = 0.95,
           RainUplim = NULL,
           RainLowlim = NULL,
           PEUplim = NULL,
           PELowlim = NULL) {
    Good <- tolower(Good)
    distr <- toupper(distr)
    end.date.user <- as.Date(end.date, "%Y-%m-%d")
    start.date.user <- as.Date(start.date, "%Y-%m-%d")

    if (distr == "GEV" || distr == "GLO") {

      if (Good == "Yes" || Good == "YES" || Good == "YeS" ||
          Good == "YEs" || Good == "yes" || Good == "NO" ||
          Good == "No" || Good == "nO" || Good == "no") {

        if (is.na(as.Date(end.date, "%Y-%m-%d")) ||
            is.na(as.Date(start.date, "%Y-%m-%d")) ||
            TS < 1 ||
            TS > 96 || all.equal(TS, as.integer(TS))) {

          mim.date.fit <-
            as.numeric((end.date.user - start.date.user) / 365)
          if (mim.date.fit < 8) {
            stop("Please select a longer period between start.date and end.date.",
                 call. = FALSE)
          }

          mim.date.fit <- end.date.user - start.date.user
          start.user.day <-
            as.numeric(format(start.date.user, format = "%d"))
          end.user.day <-
            as.numeric(format(end.date.user, format = "%d"))
          end.user.month <-
            as.numeric(format(end.date.user, format = "%m"))
          start.year <-
            as.numeric(format(start.date.user, format = "%Y"))
          start.month <-
            as.numeric(format(start.date.user, format = "%m"))
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
            community = "ag",
            lonlat = c(lon, lat),
            dates = c(start.date.protocal, end.date.user),
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
          decli <-
            23.45 * sin((360 * (sse_i$DOY - 80) / 365) * (pi / 180))
          lat.rad <- lat * (pi / 180)
          decli.rad <- decli * (pi / 180)
          hn.rad <- (acos(tan(decli.rad) * -tan(lat.rad)))
          hn.deg <- hn.rad * (180 / pi)
          N <- (2 * hn.deg) / 15
          dist.terra.sol <-
            1 + (0.033 * cos((pi / 180) * (sse_i$DOY * (360 / 365))))
          Ra <-
            (37.6 * (dist.terra.sol ^ 2)) *
            ((pi / 180) * hn.deg * sin(lat.rad) * sin(decli.rad) +
               (cos(lat.rad) * cos(decli.rad) * sin(hn.rad)))
          ####   Hargreaves&Samani
          ETP.harg.daily <-
            0.0023 * (Ra * 0.4081633) *
            (sse_i$T2M_MAX - sse_i$T2M_MIN) ^ 0.5 * (sse_i$T2M + 17.8)
          ####    Penman- Monteith-FAO
          es <-
            0.6108 * exp((17.27 * sse_i$T2M) / (sse_i$T2M + 273.3))
          ea <- (sse_i$RH2M * es) / 100
          slope.pressure <-
            (4098 * es) / ((sse_i$T2M + 237.3) ^ 2)
          Q0.ajust <- 0.75 * Ra
          Rn <-
            (1 - 0.2) * sse_i$ALLSKY_SFC_SW_DWN -
            (1.35 * (sse_i$ALLSKY_SFC_SW_DWN / Q0.ajust) - 0.35) *
            (0.35 - (0.14 * sqrt(ea))) * (5.67 * 10 ^
                                            -8) * (((sse_i$T2M ^ 4) + (sse_i$T2M_MIN ^ 4)) / 2)
          ETP.pm.daily <-
            (0.408 * slope.pressure * (Rn - 0.8) + 0.063 *
               (900 / (sse_i$T2M + 273)) * sse_i$WS2M * (es - ea)) /
            (slope.pressure + 0.063 * (1 + 0.34 * sse_i$WS2M))
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
                                                sse_i$DD > 7 &
                                                sse_i$DD <= 14), 14:16])
            data.week3 <- colSums(sse_i[which(sse_i$YEAR == year &
                                                sse_i$MM == month &
                                                sse_i$DD > 14 &
                                                sse_i$DD <= 21), 14:16])
            data.week4 <- colSums(sse_i[which(sse_i$YEAR == year &
                                                sse_i$MM == month &
                                                sse_i$DD > 21), 14:16])
            data.week[a,] <-
              c(lon, lat, year, month, 1, data.week1)
            data.week[b,] <-
              c(lon, lat, year, month, 2, data.week2)
            data.week[c,] <-
              c(lon, lat, year, month, 3, data.week3)
            data.week[d,] <-
              c(lon, lat, year, month, 4, data.week4)
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
            data.week <- data.week[-c(rows),]
          }
          rows <-
            which(data.week[, 3] == final.year &
                    data.week[, 4] == final.month &
                    data.week[, 5] >
                    final.week)
          n.rows <- length(rows)
          if (n.rows > 0) {
            data.week <- data.week[-c(rows),]
          }
          n <- length(which(data.week[, 3] <= final.year))
          data.week <- data.week[1:n,]
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
          first.row <-
            which(data.week[, 3] == start.year &
                    data.week[, 4] == start.month &
                    data.week[, 5] == start.week)
          if (first.row > 1) {
            data.week <- data.week[-(1:(first.row - 1)),]
          }
          ######## removing suspicions data
          ##### Rainfall
          if (!is.numeric(RainUplim) &
              !is.null(RainUplim) ||
              !is.numeric(RainLowlim) &
              !is.null(RainLowlim) ||
              !is.numeric(PEUplim) &
              !is.null(PEUplim) ||
              !is.numeric(PELowlim) &
              !is.null(PELowlim)) {
            stop(
              "Please, provide appropriate numerical values for RainUplim or
                RainLowlim (mm) or PEUplim or PELowlim (Celsious degrees).
                If there is no suspicions data to be removed set them to NULL.",
              call. = FALSE
            )
          }
          Uplim <- RainUplim
          Lowlim <- RainLowlim
          upremov <- which(data.week[, 6] > Uplim)
          lowremov <- which(data.week[, 6] < Lowlim)
          if (length(upremov) > 0) {
            data.week[c(upremov),6]=NA
            message("removed rowns:")
            print(upremov)
          }
          if (length(lowremov) > 0) {
            data.week[c(lowremov),6]=NA
            message("removed rowns:")
            print(lowremov)
          }
          ##### Potential Evapotranspiration (Hargreaves & Samani)
          Uplim <- PEUplim
          Lowlim <- PELowlim
          upremov <- which(data.week[, 7] > Uplim)
          lowremov <- which(data.week[, 7] < Lowlim)
          if (length(upremov) > 0) {
            data.week[c(upremov),7]=NA
            message("removed rowns:")
            print(upremov)
          }
          if (length(lowremov) > 0) {
            data.week[c(lowremov),7]=NA
            message("removed rowns:")
            print(lowremov)
          }
          #########
          n <- length(data.week[, 1])
          data.at.timescale <- matrix(NA, (n - (TS - 1)), 6)
          final.point <- n - (TS - 1)
          if (TS > 1) {
            point <- 1
            a <- 1
            b <- TS
            c <- 1
            data.at.timescale[c,] <-
              c(data.week[b, 3:4], data.week[b, 9],
                colSums(data.week[a:b, 6:8]))
            point <- point + 1
            a <- a + 1
            b <- b + 1
            c <- c + 1
            while (point <= final.point) {
              data.at.timescale[c,] <-
                c(data.week[b, 3:4], data.week[b, 9],
                  colSums(data.week[a:b, 6:8]))
              if (is.na(data.at.timescale[c, 4]) ||
                  is.na(data.at.timescale[c, 5]) ||
                  is.na(data.at.timescale[c, 6] )) {
                message("Gaps in the original data.")
              }
              point <- point + 1
              a <- a + 1
              b <- b + 1
              c <- c + 1
            }
          } else {
            data.at.timescale <-
              cbind(data.week[, 3:4], data.week[, 9], data.week[, 6:8])
          }
          data.at.timescale <-
            cbind(
              data.at.timescale,
              (data.at.timescale[, 4] - data.at.timescale[, 5]),
              (data.at.timescale[, 4] - data.at.timescale[, 6])
            )
          parameters <- matrix(NA, 48, 11)
          if (Good == "yes") {
            if (!is.numeric(sig.level) ||
                sig.level < 0.90 || sig.level > 0.95) {
              stop(
                "Please provide an appropriate significance level, that is:
          sig.level may only assume values between 0.9 and 0.95."
              )
            }
            message("Calculating the goodness-of-fit tests. This might take a while.")
            Goodness <- matrix(NA, 48, 12)
            for (i in 1:48) {
              month.par <- data.at.timescale[i, 3]
              rain <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 4])
              rain.nozero <- na.omit(rain[rain > 0])
              n.rain <- length(na.omit(rain))
              n.nonzero <- length(rain.nozero)
              n.z <- n.rain - n.nonzero
              if (n.z == 0) {
                probzero <- 0
              } else {
                probzero <- (n.z + 1) / (2 * (n.rain + 1))
              }
              soma.rain <- matrix(NA, n.nonzero, 1)
              parameters[i, 1:4] <-
                c(data.at.timescale[i, 3], pelgam(samlmu(rain.nozero)), probzero)
              prob.rain <-
                sort(cdfgam(rain.nozero, c(parameters[i, 2], parameters[i, 3])))
              prob.rain[prob.rain < 0.001351] <- 0.001351
              prob.rain[prob.rain > 0.998649] <- 0.998649
              prob.emp <-
                sort(rank(
                  rain.nozero,
                  na.last = NA,
                  ties.method = c("first")
                )) / n.nonzero
              Goodness[i, 1] <- max(abs(prob.emp - prob.rain))
              petp.harg <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 7])
              petp.pm <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 8])
              if (distr == "GEV") {
                parameters[i, 5:10] <-
                  c(pelgev(samlmu(petp.harg)), pelgev(samlmu(petp.pm)))
                prob.harg <-
                  sort(cdfgev(petp.harg,
                              c(
                                parameters[i, 5], parameters[i, 6], parameters[i, 7]
                              )))
                prob.pm <-
                  sort(cdfgev(petp.pm,
                              c(
                                parameters[i, 8], parameters[i, 9], parameters[i, 10]
                              )))
              }
              if (distr == "GLO") {
                parameters[i, 5:10] <-
                  c(pelglo(samlmu(petp.harg)), pelglo(samlmu(petp.pm)))
                prob.harg <-
                  sort(cdfglo(petp.harg,
                              c(
                                parameters[i, 5], parameters[i, 6], parameters[i, 7]
                              )))
                prob.pm <-
                  sort(cdfglo(petp.pm,
                              c(
                                parameters[i, 8], parameters[i, 9], parameters[i, 10]
                              )))
              }
              prob.harg[prob.harg < 0.001351] <- 0.001351
              prob.harg[prob.harg > 0.998649] <- 0.998649
              prob.pm[prob.pm < 0.001351] <- 0.001351
              prob.pm[prob.pm > 0.998649] <- 0.998649
              n.harg <- length(na.omit(petp.harg))
              soma.harg <- matrix(NA, n.harg, 1)
              prob.emp <-
                sort(rank(
                  petp.harg,
                  na.last = NA,
                  ties.method = c("first")
                )) / n.harg
              Goodness[i, 3] <- max(abs(prob.emp - prob.harg))
              n.pm <- length(na.omit(petp.pm))
              soma.pm <- matrix(NA, n.pm, 1)
              prob.emp <-
                sort(rank(
                  petp.pm,
                  na.last = NA,
                  ties.method = c("first")
                )) / n.pm
              Goodness[i, 5] <- max(abs(prob.emp - prob.pm))
              for (ad in 1:n.nonzero) {
                soma.rain[ad, 1] <-
                  ((2 * ad) - 1) * ((log(prob.rain[ad])) + log(1 - prob.rain[n.nonzero + 1 - ad]))
              }
              Goodness[i, 7] <-
                -n.nonzero - ((1 / n.nonzero) * sum(soma.rain, na.rm = TRUE))
              for (ad in 1:n.harg) {
                soma.harg[ad, 1] <-
                  ((2 * ad) - 1) * ((log(prob.harg[ad])) + log(1 - prob.harg[n.harg + 1 - ad]))
              }
              Goodness[i, 9] <-
                -n.harg - ((1 / n.harg) * sum(soma.harg, na.rm = TRUE))
              for (ad in 1:n.pm) {
                soma.pm[ad, 1] <-
                  ((2 * ad) - 1) * ((log(prob.pm[ad])) + log(1 - prob.pm[n.pm + 1 - ad]))
              }
              Goodness[i, 11] <-
                -n.pm - ((1 / n.pm) * sum(soma.pm, na.rm = TRUE))
              #### Critical values
              null.dist <- matrix(NA, 2000, 6)
              for (j in 1:2000) {
                x <-
                  sort(quagam(runif(n.nonzero),
                              c(parameters[i, 2], parameters[i, 3])))
                prob.synt <- try(cdfgam(x, pelgam(samlmu(x))))
                if (length(prob.synt) != n.nonzero){
                  prob.synt <- try(cdfgam(x, c(parameters[i,2],parameters[i,3])))
                  message("Using original parameters")
                }
                prob.synt[prob.synt < 0.001351] <- 0.001351
                prob.synt[prob.synt > 0.998649] <- 0.998649
                prob.emp <- sort(rank(x)) / n.nonzero
                null.dist[j, 1] <- max(abs(prob.emp - prob.synt))
                for (ad in 1:n.nonzero) {
                  soma.rain[ad, 1] <-
                    ((2 * ad) - 1) * ((log(prob.synt[ad])) + log(1 - prob.synt[n.nonzero + 1 - ad]))
                }
                null.dist[j, 4] <-
                  -n.nonzero - ((1 / n.nonzero) * sum(soma.rain, na.rm = TRUE))
                if (distr == "GEV") {
                  y <-
                    sort(quagev(
                      runif(n.harg),
                      c(parameters[i, 5], parameters[i, 6], parameters[i, 7])
                    ))
                  prob.synt <- cdfgev(y, pelgev(samlmu(y)))
                  prob.synt[prob.synt < 0.001351] <- 0.001351
                  prob.synt[prob.synt > 0.998649] <- 0.998649
                  prob.emp <- sort(rank(y)) / n.harg
                  null.dist[j, 2] <-
                    max(abs(prob.emp - prob.synt))
                  for (ad in 1:n.harg) {
                    soma.harg[ad, 1] <-
                      ((2 * ad) - 1) * ((log(prob.synt[ad])) + log(1 - prob.synt[n.harg + 1 - ad]))
                  }
                  null.dist[j, 5] <-
                    -n.harg - ((1 / n.harg) * sum(soma.harg, na.rm = TRUE))
                  z <-
                    sort(quagev(
                      runif(n.pm),
                      c(parameters[i, 8], parameters[i, 9], parameters[i, 10])
                    ))
                  prob.synt <- cdfgev(z, pelgev(samlmu(z)))
                  prob.synt[prob.synt < 0.001351] <- 0.001351
                  prob.synt[prob.synt > 0.998649] <- 0.998649
                  prob.emp <- sort(rank(z)) / n.pm
                  null.dist[j, 3] <-
                    max(abs(prob.emp - prob.synt))
                  for (ad in 1:n.pm) {
                    soma.pm[ad, 1] <-
                      ((2 * ad) - 1) * ((log(prob.synt[ad])) + log(1 - prob.synt[n.pm + 1 - ad]))
                  }
                  null.dist[j, 6] <-
                    -n.pm - ((1 / n.pm) * sum(soma.pm, na.rm = TRUE))
                }
                if (distr == "GLO") {
                  y <-
                    sort(quaglo(
                      runif(n.harg),
                      c(parameters[i, 5], parameters[i, 6], parameters[i, 7])
                    ))
                  prob.synt <- cdfglo(y, pelglo(samlmu(y)))
                  prob.synt[prob.synt < 0.001351] <- 0.001351
                  prob.synt[prob.synt > 0.998649] <- 0.998649
                  prob.emp <- sort(rank(y)) / n.harg
                  null.dist[j, 2] <-
                    max(abs(prob.emp - prob.synt))
                  for (ad in 1:n.harg) {
                    soma.harg[ad, 1] <-
                      ((2 * ad) - 1) * ((log(prob.synt[ad])) + log(1 - prob.synt[n.harg + 1 - ad]))
                  }
                  null.dist[j, 5] <-
                    -n.harg - ((1 / n.harg) * sum(soma.harg, na.rm = TRUE))
                  z <-
                    sort(quaglo(
                      runif(n.pm),
                      c(parameters[i, 8], parameters[i, 9], parameters[i, 10])
                    ))
                  prob.synt <- cdfglo(z, pelglo(samlmu(z)))
                  prob.synt[prob.synt < 0.001351] <- 0.001351
                  prob.synt[prob.synt > 0.998649] <- 0.998649
                  prob.emp <- sort(rank(z)) / n.pm
                  null.dist[j, 3] <-
                    max(abs(prob.emp - prob.synt))
                  for (ad in 1:n.pm) {
                    soma.pm[ad, 1] <-
                      ((2 * ad) - 1) * ((log(prob.synt[ad])) + log(1 - prob.synt[n.pm + 1 - ad]))
                  }
                  null.dist[j, 6] <-
                    -n.pm - ((1 / n.pm) * sum(soma.pm, na.rm = TRUE))
                }
              }
              Goodness[i, 2] <-
                quantile(null.dist[, 1], sig.level)
              Goodness[i, 4] <-
                quantile(null.dist[, 2], sig.level)
              Goodness[i, 6] <-
                quantile(null.dist[, 3], sig.level)
              Goodness[i, 8] <-
                quantile(null.dist[, 4], sig.level)
              Goodness[i, 10] <-
                quantile(null.dist[, 5], sig.level)
              Goodness[i, 12] <-
                quantile(null.dist[, 6], sig.level)
            }
            parameters[, 11] <- TS
            parameters <- cbind(lon, lat, parameters)
            parameters <- parameters[order(parameters[, 3]),]
            colnames(parameters) <- c(
              "lon",
              "lat",
              "quart.month",
              "alfa.rain",
              "beta.rain",
              "probzero.rain",
              "loc.harg",
              "sc.harg",
              "sh.harg",
              "loc.pm",
              "sc.pm",
              "sh.pm",
              "TS"
            )
            colnames(Goodness) <- c(
              "Lili.Rain",
              "Crit",
              "Lili.PPEHarg",
              "Crit",
              "Lili.PPEPM",
              "Crit",
              "AD.Rain",
              "Crit",
              "AD.PPEHarg",
              "Crit",
              "AD.PPEPM",
              "Crit"
            )
            ########
            n.weeks <- length(data.at.timescale[, 1])
            pos <- 1
            SDI <- matrix(NA, n.weeks, 3)
            if (distr == "GEV") {
              while (pos <= n.weeks) {
                i <- data.at.timescale[pos, 3]
                prob <-
                  parameters[i, 6] + (1 - parameters[i, 6]) * cdfgam(data.at.timescale[pos, 4],
                                                                     c(parameters[i, 4], parameters[i, 5]))
                if (!is.na(prob) & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfgev(data.at.timescale[pos, 7],
                         c(parameters[i, 7], parameters[i, 8], parameters[i, 9]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfgev(data.at.timescale[pos, 8],
                         c(parameters[i, 10], parameters[i, 11], parameters[i, 12]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
                pos <- pos + 1
              }
            }
            if (distr == "GLO") {
              while (pos <= n.weeks) {
                i <- data.at.timescale[pos, 3]
                prob <-
                  parameters[i, 6] + (1 - parameters[i, 6]) * cdfgam(data.at.timescale[pos, 4],
                                                                     c(parameters[i, 4], parameters[i, 5]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfglo(data.at.timescale[pos, 7],
                         c(parameters[i, 7], parameters[i, 8], parameters[i, 9]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfglo(data.at.timescale[pos, 8],
                         c(parameters[i, 10], parameters[i, 11], parameters[i, 12]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
                pos <- pos + 1
              }
            }
            categories <- matrix(NA, n.weeks, 3)
            for (i in 1:n.weeks) {
              if (SDI[i, 1] <= -2.0 & !is.na(SDI[i, 1])) {
                categories[i, 1] <- "ext.dry"
              } else {
                if (SDI[i, 1] <= -1.5 & !is.na(SDI[i, 1])) {
                  categories[i, 1] <- "sev.dry"
                } else {
                  if (SDI[i, 1] <= -1.0 & !is.na(SDI[i, 1])) {
                    categories[i, 1] <- "mod.dry"
                  } else {
                    if (SDI[i, 1] <= 1.0 & !is.na(SDI[i, 1])) {
                      categories[i, 1] <- "Normal"
                    } else {
                      if (SDI[i, 1] <= 1.5 & !is.na(SDI[i, 1])) {
                        categories[i, 1] <- "mod.wet"
                      } else {
                        if (SDI[i, 1] <= 2.0 & !is.na(SDI[i, 1])) {
                          categories[i, 1] <- "sev.wet"
                        } else {
                          if (SDI[i, 1] > 2.0 & !is.na(SDI[i, 1])) {
                            categories[i, 1] <- "ext.wet"
                          }
                        }
                      }
                    }
                  }
                }
              }
              if (SDI[i, 2] <= -2.0 & !is.na(SDI[i, 2])) {
                categories[i, 2] <- "ext.dry"
              } else {
                if (SDI[i, 2] <= -1.5 & !is.na(SDI[i, 2])) {
                  categories[i, 2] <- "sev.dry"
                } else {
                  if (SDI[i, 2] <= -1.0 & !is.na(SDI[i, 2])) {
                    categories[i, 2] <- "mod.dry"
                  } else {
                    if (SDI[i, 2] <= 1.0 & !is.na(SDI[i, 2])) {
                      categories[i, 2] <- "Normal"
                    } else {
                      if (SDI[i, 2] <= 1.5 & !is.na(SDI[i, 2])) {
                        categories[i, 2] <- "mod.wet"
                      } else {
                        if (SDI[i, 2] <= 2.0 & !is.na(SDI[i, 2])) {
                          categories[i, 2] <- "sev.wet"
                        } else {
                          if (SDI[i, 2] > 2.0 & !is.na(SDI[i, 2])) {
                            categories[i, 2] <- "ext.wet"
                          }
                        }
                      }
                    }
                  }
                }
              }
              if (SDI[i, 3] <= -2.0 & !is.na(SDI[i, 3])) {
                categories[i, 3] <- "ext.dry"
              } else {
                if (SDI[i, 3] <= -1.5 & !is.na(SDI[i, 3])) {
                  categories[i, 3] <- "sev.dry"
                } else {
                  if (SDI[i, 3] <= -1.0 & !is.na(SDI[i, 3])) {
                    categories[i, 3] <- "mod.dry"
                  } else {
                    if (SDI[i, 3] <= 1.0 & !is.na(SDI[i, 3])) {
                      categories[i, 3] <- "Normal"
                    } else {
                      if (SDI[i, 3] <= 1.5 & !is.na(SDI[i, 3])) {
                        categories[i, 3] <- "mod.wet"
                      } else {
                        if (SDI[i, 3] <= 2.0 & !is.na(SDI[i, 3])) {
                          categories[i, 3] <- "sev.wet"
                        } else {
                          if (SDI[i, 3] > 2.0 & !is.na(SDI[i, 3])) {
                            categories[i, 3] <- "ext.wet"
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            SDI <- cbind(data.at.timescale, SDI)
            ##### Normality checking procedures
            Norn.check <- matrix(NA, 48, 15)
            for (j in 1:48) {
              SDI.week <- as.matrix(SDI[which(SDI[, 3] == j), 9:11])
              w <- shapiro.test(SDI.week[, 1])
              if (w$p.value<0.01){w$p.value <- 0.01}
              Norn.check[j, 1:3] <-
                c(w$statistic, w$p.value, abs(median((SDI.week[, 1]), na.rm = TRUE)))
              w <- shapiro.test(SDI.week[, 2])
              if (w$p.value<0.01){w$p.value <- 0.01}
              Norn.check[j, 4:6] <-
                c(w$statistic, w$p.value, abs(median((SDI.week[, 2]), na.rm = TRUE)))
              w <- shapiro.test(SDI.week[, 3])
              if (w$p.value<0.01){w$p.value <- 0.01}
              Norn.check[j, 7:9] <-
                c(w$statistic, w$p.value, abs(median((SDI.week[, 3]), na.rm = TRUE)))
              ###### As proposed in Wu et al. (2007)
              if (Norn.check[j, 1] < 0.960 &&
                  Norn.check[j, 2] < 0.10 &&
                  Norn.check[j, 3] > 0.05) {
                Norn.check[j, 10] <- "NoNormal"
              } else {
                Norn.check[j, 10] <- "Normal"
              }
              if (Norn.check[j, 4] < 0.960 &&
                  Norn.check[j, 5] < 0.10 &&
                  Norn.check[j, 6] > 0.05) {
                Norn.check[j, 11] <- "NoNormal"
              } else {
                Norn.check[j, 11] <- "Normal"
              }
              if (Norn.check[j, 7] < 0.960 &&
                  Norn.check[j, 8] < 0.10 &&
                  Norn.check[j, 9] > 0.05) {
                Norn.check[j, 12] <- "NoNormal"
              } else {
                Norn.check[j, 12] <- "Normal"
              }
              ###### As proposed in Stagge et al. (2015)
              if (Norn.check[j, 2] < 0.05) {
                Norn.check[j, 13] <- "NoNormal"
              } else {
                Norn.check[j, 13] <- "Normal"
              }
              if (Norn.check[j, 5] < 0.05) {
                Norn.check[j, 14] <- "NoNormal"
              } else {
                Norn.check[j, 14] <- "Normal"
              }
              if (Norn.check[j, 8] < 0.05) {
                Norn.check[j, 15] <- "NoNormal"
              } else {
                Norn.check[j, 15] <- "Normal"
              }
            }
            ##########
            colnames(Norn.check) <- c(
              "SPI.Shap",
              "SPI.Shap.p",
              "SPI.AbsMed",
              "SPEI.Harg.Shap",
              "SPEI.Harg.Shap.p",
              "SPEI.Harg.AbsMed",
              "SPEI.PM.Shap",
              "SPEI.PM.Shap.p",
              "SPEI.PM.AbsMed",
              "SPI.testI",
              "SPEI.Harg.testI",
              "SPEI.PM.testI",
              "SPI.testII",
              "SPEI.Harg.testII",
              "SPEI.PM.testII"
            )
            SDI.final <- data.frame(SDI, categories)
            colnames(SDI.final) <- c(
              "Year",
              "Month",
              "quart.month",
              "Rain",
              "PE.Harg",
              "PE.PM",
              "PPE.Harg",
              "PPE.PM",
              "SPI",
              "SPEI.Harg",
              "SPEI.PM",
              "Categ.SPI",
              "Categ.SPEI.Harg",
              "Categ.SPEI.PM"
            )
            if (end.user.month == 1 ||
                end.user.month == 3 || end.user.month == 5 ||
                end.user.month == 7 ||
                end.user.month == 8 ||
                end.user.month == 10 || end.user.month == 12) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 ||
                  end.user.day > 22 & end.user.day < 31) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            if (end.user.month == 4 ||
                end.user.month == 6 ||
                end.user.month == 9 || end.user.month == 11) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 ||
                  end.user.day > 22 & end.user.day < 30) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            if (end.user.month == 2) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 ||
                  end.user.day > 22 & end.user.day < 28) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            whichTS <- paste("TS is", as.character(TS))
            row.names(SDI.final[1,]) <- whichTS
            Result <-
              list(SDI.final, parameters, Goodness, Norn.check)
            Result <- list(
              SDI = SDI.final,
              DistPar = parameters,
              GoodFit = Goodness,
              Normality = Norn.check
            )
            return(Result)
            message("The calculations started on:")
            print(start.date.protocal)
          }
          if (Good == "no") {
            for (i in 1:48) {
              month.par <- data.at.timescale[i, 3]
              rain <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 4])
              rain.nozero <- na.omit(rain[rain > 0])
              n.rain <- length(rain)
              n.nonzero <- length(rain.nozero)
              n.z <- n.rain - n.nonzero
              if (n.z == 0) {
                probzero <- 0
              } else {
                probzero <- (n.z + 1) / (2 * (n.rain + 1))
              }
              parameters[i, 1:4] <-
                c(data.at.timescale[i, 3], pelgam(samlmu(rain.nozero)), probzero)
              petp.harg <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 7])
              petp.pm <-
                (data.at.timescale[which(data.at.timescale[, 3] == month.par), 8])
              if (distr == "GEV") {
                parameters[i, 5:10] <-
                  c(pelgev(samlmu(petp.harg)), pelgev(samlmu(petp.pm)))
                prob.harg <-
                  sort(cdfgev(petp.harg,
                              c(
                                parameters[i, 5], parameters[i, 6], parameters[i, 7]
                              )))
                prob.pm <-
                  sort(cdfgev(petp.pm,
                              c(
                                parameters[i, 8], parameters[i, 9], parameters[i, 10]
                              )))
              }
              if (distr == "GLO") {
                parameters[i, 5:10] <-
                  c(pelglo(samlmu(petp.harg)), pelglo(samlmu(petp.pm)))
                prob.harg <-
                  sort(cdfglo(petp.harg,
                              c(
                                parameters[i, 5], parameters[i, 6], parameters[i, 7]
                              )))
                prob.pm <-
                  sort(cdfglo(petp.pm,
                              c(
                                parameters[i, 8], parameters[i, 9], parameters[i, 10]
                              )))
              }
            }
            parameters[, 11] <- TS
            parameters <- cbind(lon, lat, parameters)
            parameters <- parameters[order(parameters[, 3]),]
            colnames(parameters) <- c(
              "lon",
              "lat",
              "quart.month",
              "alfa.rain",
              "beta.rain",
              "probzero.rain",
              "loc.harg",
              "sc.harg",
              "sh.harg",
              "loc.pm",
              "sc.pm",
              "sh.pm",
              "TS"
            )
            n.weeks <- length(data.at.timescale[, 1])
            pos <- 1
            SDI <- matrix(NA, n.weeks, 3)
            if (distr == "GEV") {
              while (pos <= n.weeks) {
                i <- data.at.timescale[pos, 3]
                prob <-
                  parameters[i, 6] + (1 - parameters[i, 6]) *
                  cdfgam(data.at.timescale[pos, 4],
                         c(parameters[i, 4], parameters[i, 5]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfgev(data.at.timescale[pos, 7],
                         c(parameters[i, 7], parameters[i, 8],
                           parameters[i, 9]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfgev(data.at.timescale[pos, 8],
                         c(parameters[i, 10], parameters[i, 11],
                           parameters[i, 12]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
                pos <- pos + 1
              }
            }
            if (distr == "GLO") {
              while (pos <= n.weeks) {
                i <- data.at.timescale[pos, 3]
                prob <-
                  parameters[i, 6] + (1 - parameters[i, 6]) *
                  cdfgam(data.at.timescale[pos, 4],
                         c(parameters[i, 4], parameters[i, 5]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfglo(data.at.timescale[pos, 7],
                         c(parameters[i, 7], parameters[i, 8],
                           parameters[i, 9]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
                prob <-
                  cdfglo(data.at.timescale[pos, 8],
                         c(parameters[i, 10], parameters[i, 11],
                           parameters[i, 12]))
                if (!is.na(prob)  & prob < 0.001351) {
                  prob <- 0.001351
                }
                if (!is.na(prob)  & prob > 0.998649) {
                  prob <- 0.998649
                }
                SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
                pos <- pos + 1
              }
            }
            categories <- matrix(NA, n.weeks, 3)
            for (i in 1:n.weeks) {
              if (SDI[i, 1] <= -2.0 & !is.na(SDI[i, 1])) {
                categories[i, 1] <- "ext.dry"
              } else {
                if (SDI[i, 1] <= -1.5 & !is.na(SDI[i, 1])) {
                  categories[i, 1] <- "sev.dry"
                } else {
                  if (SDI[i, 1] <= -1.0 & !is.na(SDI[i, 1])) {
                    categories[i, 1] <- "mod.dry"
                  } else {
                    if (SDI[i, 1] <= 1.0 & !is.na(SDI[i, 1])) {
                      categories[i, 1] <- "Normal"
                    } else {
                      if (SDI[i, 1] <= 1.5 & !is.na(SDI[i, 1])) {
                        categories[i, 1] <- "mod.wet"
                      } else {
                        if (SDI[i, 1] <= 2.0 & !is.na(SDI[i, 1])) {
                          categories[i, 1] <- "sev.wet"
                        } else {
                          if (SDI[i, 1] > 2.0 & !is.na(SDI[i, 1])) {
                            categories[i, 1] <- "ext.wet"
                          }
                        }
                      }
                    }
                  }
                }
              }
              if (SDI[i, 2] <= -2.0 & !is.na(SDI[i, 2])) {
                categories[i, 2] <- "ext.dry"
              } else {
                if (SDI[i, 2] <= -1.5 & !is.na(SDI[i, 2])) {
                  categories[i, 2] <- "sev.dry"
                } else {
                  if (SDI[i, 2] <= -1.0 & !is.na(SDI[i, 2])) {
                    categories[i, 2] <- "mod.dry"
                  } else {
                    if (SDI[i, 2] <= 1.0 & !is.na(SDI[i, 2])) {
                      categories[i, 2] <- "Normal"
                    } else {
                      if (SDI[i, 2] <= 1.5 & !is.na(SDI[i, 2])) {
                        categories[i, 2] <- "mod.wet"
                      } else {
                        if (SDI[i, 2] <= 2.0 & !is.na(SDI[i, 2])) {
                          categories[i, 2] <- "sev.wet"
                        } else {
                          if (SDI[i, 2] > 2.0 & !is.na(SDI[i, 2])) {
                            categories[i, 2] <- "ext.wet"
                          }
                        }
                      }
                    }
                  }
                }
              }
              if (SDI[i, 3] <= -2.0 & !is.na(SDI[i, 3])) {
                categories[i, 3] <- "ext.dry"
              } else {
                if (SDI[i, 3] <= -1.5 & !is.na(SDI[i, 3])) {
                  categories[i, 3] <- "sev.dry"
                } else {
                  if (SDI[i, 3] <= -1.0 & !is.na(SDI[i, 3])) {
                    categories[i, 3] <- "mod.dry"
                  } else {
                    if (SDI[i, 3] <= 1.0 & !is.na(SDI[i, 3])) {
                      categories[i, 3] <- "Normal"
                    } else {
                      if (SDI[i, 3] <= 1.5 & !is.na(SDI[i, 3])) {
                        categories[i, 3] <- "mod.wet"
                      } else {
                        if (SDI[i, 3] <= 2.0 & !is.na(SDI[i, 3])) {
                          categories[i, 3] <- "sev.wet"
                        } else {
                          if (SDI[i, 3] > 2.0 & !is.na(SDI[i, 3])) {
                            categories[i, 3] <- "ext.wet"
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
            colnames(SDI.final) <- c(
              "Year",
              "Month",
              "quart.month",
              "Rain",
              "PE.Harg",
              "PE.PM",
              "PPE.Harg",
              "PPE.PM",
              "SPI",
              "SPEI.Harg",
              "SPEI.PM",
              "Categ.SPI",
              "Categ.SPEI.Harg",
              "Categ.SPEI.PM"
            )
            if (end.user.month == 1 || end.user.month == 3 ||
                end.user.month == 5 ||
                end.user.month == 7 ||
                end.user.month == 8 ||
                end.user.month == 10 || end.user.month == 12) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 || end.user.day > 22 &
                  end.user.day < 31) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            if (end.user.month == 4 || end.user.month == 6 ||
                end.user.month == 9 || end.user.month == 11) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 || end.user.day > 22 &
                  end.user.day < 30) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            if (end.user.month == 2) {
              if (end.user.day < 7 || end.user.day > 7 & end.user.day < 14 ||
                  end.user.day > 14 &
                  end.user.day < 22 || end.user.day > 22 &
                  end.user.day < 28) {
                message("The latest quart.month period is not complete")
                SDI.final <- SDI.final[-c(n.weeks),]
              }
            }
            whichTS <- paste("TS is", as.character(TS))
            row.names(SDI.final[1,]) <- whichTS
            Result <- list(SDI.final, parameters)
            Result <- list(SDI = SDI.final, DistPar = parameters)
            return(Result)
            message("The calculations started on:")
            print(start.date.protocal)
          }
        } else {stop("`distr` should be set to either 'GEV' or 'GLO.'",
                     call = FALSE)}

      } else {stop("`Good` should be set to either 'Yes' or 'No'.",
                   call. = FALSE)}
    } else{
      stop(
        "Recall Date format should be YYYY-MM-DD and TS must be an",
        "interger value ranging between 1 and 96",
        call. = FALSE
      )
    }
  }
