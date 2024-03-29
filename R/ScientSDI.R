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
#' date at which the indices estimates should start. Format:
#'   \dQuote{YYYY-MM-DD}.
#' @param end.date
#' date at which the indices estimates should end. Format: \dQuote{YYYY-MM-DD}.
#' @param distr
#' A character variable (\dQuote{GEV} or \dQuote{GLO}) defining the distribution
#'   to calculate the \acronym{SPEI}.  Default is \code{GEV}.
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
#' Optional. Upper limit in millimetres from which rainfall values larger than
#'   it will be removed.  Default is \code{NULL}.
#' @param RainLowlim
#' Optional. Lower limit in millimetres from which rainfall values smaller than
#'   it will be removed.  Default is \code{NULL}.
#' @param PEUplim
#' Optional. Upper limit in millimetres from which evapotranspiration values
#'   larger than it will be removed. Valid for Hargreaves and Samani method
#'   Default is \code{NULL}.
#' @param PELowlim
#' Optional. Lower limit in millimetres from which evapotranspiration values
#'   smaller than it will be removed. Valid for Hargreaves and Samani method
#'   Default is \code{NULL}.
#' @return
#' A \code{list} object with data calculated at the time scale selected by the
#'    user.  If \code{Good = "Yes"}, this \code{list} object includes:
#' \describe{
#'   \item{SDI}{The \dQuote{NASA-SPI}, \dQuote{NASA-SPEI.HS} and
#'     \dQuote{NASA-SPEI.PM.}}
#'   \item{DistPar}{The parameters of the distributions (gamma and
#'     \acronym{GEV}) used to calculate the indices.}
#'   \item{GoodFit}{The Lilliefors and Anderson-Darling tests goodness-of-fit
#'     tests.}
#'   \item{Normality}{The outcomes of the two normality checking procedures (Wu
#'     \emph{et al}., 2006 and Stagge \emph{et al}., 2015).}
#'  }
#'
#' If \code{Good = "No"}, this \code{list} object includes \acronym{SDI} and
#'   DistPar.
#'
#' This function also presents other data (in millimiters) calculated from the
#'   \acronym{NASA} \acronym{POWER} project:
#' \itemize{
#'   \item Rainfall amounts (Rain),
#'   \item potential evapotranspiration values estimated through the Hargreaves
#'    and Samani method (\acronym{PEHS}),
#'   \item potential evapotranspiration values estimated through the FAO-56
#'    Penman-Monteith method (\acronym{PEPM}), and
#'   \item the difference between rainfall and potential evapotranspiration
#'   (\acronym{PPEHS} and \acronym{PPEPM}).
#'   }
#'
#' @export
#' @importFrom stats cor median na.omit qnorm quantile runif shapiro.test
#' @importFrom lmom pelgam pelgev pelglo quagam quagev quaglo samlmu
#' @examplesIf interactive()
#'
#' # This example requires an Internet connection to fetch data and takes >5s
#' #  to run, and so is only run in interactive sessions
#'
#' ScientSDI(
#'   lon = -47.3,
#'   lat = -22.87,
#'   start.date = "1993-01-01",
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
#'  Wu, H., Svoboda, M.D., Hayes, M.J., Wilhite, D.A. and Wen, F., 2006.
#'   Appropriate application of the standardized precipitation index in arid
#'   locations and dry seasons. International Journal of Climatology: A Journal
#'   of the Royal Meteorological Society, 27(1), pp.65-79.

ScientSDI <-
  function(lon,
           lat,
           start.date,
           end.date,
           distr = "GEV",
           TS = 4L,
           Good = "No",
           sig.level = 0.95,
           RainUplim = NULL,
           RainLowlim = NULL,
           PEUplim = NULL,
           PELowlim = NULL) {
    Good <- tolower(Good)
    distr <- toupper(distr)

    check.distr(distr)
    check.TS(TS)

    if (Good != "yes" && Good != "no") {
      stop("`Good` should be set to either 'Yes' or 'No'.",
           call. = FALSE)
    }

    if (!is.numeric(RainUplim) &
        !is.null(RainUplim) ||
        !is.numeric(RainLowlim) &
        !is.null(RainLowlim) ||
        !is.numeric(PEUplim) &
        !is.null(PEUplim) ||
        !is.numeric(PELowlim) &
        !is.null(PELowlim)) {
      stop(
        "Please, provide appropriate numerical values for `RainUplim` or ",
        "`RainLowlim` (mm) or `PEUplim` or `PELowlim` (Celsius degrees). ",
        "If there are no suspicious data to be removed, leave them set them to",
        " `NULL`.",
        call. = FALSE
      )
    }

    dates <- check.dates(c(start.date, end.date))
    start.date.user <- dates[[1]]
    end.date.user <- dates[[2]]

    mim.date.fit <-
      as.numeric((end.date.user - start.date.user) / 365)
    if (mim.date.fit < 8) {
      stop("Please select a longer period between start.date and end.date.",
           call. = FALSE)
    } else if (mim.date.fit < 30) {
      warning("A period of 30 years is normal for calibrating standardised ",
              "drought indices. However, these indices will be calculated ",
              "with a shorter period, ", round(mim.date.fit, 1), ", years.",
           call. = FALSE)
    }

    start.year <- lubridate::year(start.date.user)
    start.month <- lubridate::month(start.date.user)
    start.day <- lubridate::day(start.date.user)
    final.year <- lubridate::year(end.date.user)
    final.month <- lubridate::month(end.date.user)
    final.day <- lubridate::day(end.date.user)
    start.week <-
      find.week.int(lubridate::day(start.date.user)) # see internal_functions.R
    dif <-
      calculate.dif(start.week, lubridate::day(start.date.user)) # see internal_functions.R

    start.date.protocal <- start.date.user - dif

    sse_i <- daily.ETP.wrapper(
      lon = lon,
      lat = lat,
      start.date.user = start.date.user,
      end.date.user = end.date.user
    )

    final.week <- find.week.int(final.year)
    n.years <- 1 + (final.year - start.year)
    total.nweeks <- 48 * n.years
    a <- 1
    b <- 2
    c <- 3
    d <- 4
    data.week <- matrix(NA, total.nweeks, 8)
    month <- start.month
    year <- start.year
    while (year <= final.year ||
           month <= final.month) {
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
      if (year == final.year &
          month > final.month) {
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
      which(
        data.week[, 3] == final.year &
          data.week[, 4] == final.month &
          data.week[, 5] >
          final.week
      )
    n.rows <- length(rows)
    if (n.rows > 0) {
      data.week <- data.week[-c(rows),]
    }
    n <- length(which(data.week[, 3] <= final.year))
    data.week <- data.week[1:n,]

    # see internal_functions.R for `find.quart.month.int()`
    data.week <-
      cbind(data.week, find.quart.month.int(x = data.week))

    first.row <-
      which(data.week[, 3] == start.year &
              data.week[, 4] == start.month &
              data.week[, 5] == start.week)
    if (first.row > 1) {
      data.week <- data.week[-(1:(first.row - 1)),]
    }
    ######## removing suspicious data ----

    data.week <-
      check.remove.lims(data.week, 6L, RainUplim, RainLowlim, "rain")
    data.week <-
      check.remove.lims(data.week, 7L, PEUplim, PELowlim, "PE")

    #########
    n <- nrow(data.week)
    data.at.timescale <- matrix(NA, (n - (TS - 1)), 6)
    final.point <- n - (TS - 1)
    if (TS > 1) {
      point <- 1
      a <- 1
      b <- TS
      c <- 1
      data.at.timescale[c, ] <-
        c(data.week[b, 3:4], data.week[b, 9],
          colSums(data.week[a:b, 6:8],na.rm = TRUE))
      point <- point + 1
      a <- a + 1
      b <- b + 1
      c <- c + 1
      while (point <= final.point) {
        data.at.timescale[c, ] <-
          c(data.week[b, 3:4], data.week[b, 9],
            colSums(data.week[a:b, 6:8],na.rm = TRUE))
        if (is.na(data.at.timescale[c, 4]) ||
            is.na(data.at.timescale[c, 5]) ||
            is.na(data.at.timescale[c, 6])) {
          message("Gaps in the original data.")
        }
        point <- point + 1
        a <- a + 1
        b <- b + 1
        c <- c + 1
      }
    } else {
      data.at.timescale <- cbind(data.week[, c(3:4, 9, 6:8)])
    }
    data.at.timescale <-
      cbind(
        data.at.timescale,
        (data.at.timescale[, 4] - data.at.timescale[, 5]),
        (data.at.timescale[, 4] - data.at.timescale[, 6])
      )
    parameters <- matrix(NA, 48, 11)
    if (Good == "yes") {
      check.sig.level(sig.level)

      Goodness <- matrix(NA, 48, 12)
      for (i in 1:48) {
        month.par <- data.at.timescale[i, 3]
        rain <-
          (data.at.timescale[which(data.at.timescale[, 3] == month.par), 4])
        rain.nozero <- na.omit(rain[rain > 0])
        n.rain <- length(na.omit(rain))
        n.nonzero <- length(rain.nozero)
        n.z <- n.rain - n.nonzero
        probzero <- calc.probzero(n.z, n.rain)
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
        } else {
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
            ((2 * ad) - 1) *
            ((log(prob.rain[ad])) + log(1 - prob.rain[n.nonzero + 1 - ad]))
        }
        Goodness[i, 7] <-
          -n.nonzero - ((1 / n.nonzero) * sum(soma.rain, na.rm = TRUE))
        for (ad in 1:n.harg) {
          soma.harg[ad, 1] <-
            ((2 * ad) - 1) *
            ((log(prob.harg[ad])) + log(1 - prob.harg[n.harg + 1 - ad]))
        }
        Goodness[i, 9] <-
          -n.harg - ((1 / n.harg) * sum(soma.harg, na.rm = TRUE))
        for (ad in 1:n.pm) {
          soma.pm[ad, 1] <-
            ((2 * ad) - 1) *
            ((log(prob.pm[ad])) + log(1 - prob.pm[n.pm + 1 - ad]))
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
          if (length(prob.synt) != n.nonzero) {
            prob.synt <- try(cdfgam(x, c(parameters[i, 2], parameters[i, 3])))
          }
          prob.synt[prob.synt < 0.001351] <- 0.001351
          prob.synt[prob.synt > 0.998649] <- 0.998649
          prob.emp <- sort(rank(x)) / n.nonzero
          null.dist[j, 1] <- max(abs(prob.emp - prob.synt))
          for (ad in 1:n.nonzero) {
            soma.rain[ad, 1] <-
              ((2 * ad) - 1) * ((log(prob.synt[ad])) +
                                  log(1 - prob.synt[n.nonzero + 1 - ad]))
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
                ((2 * ad) - 1) * ((log(prob.synt[ad])) +
                                    log(1 - prob.synt[n.harg + 1 - ad]))
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
                ((2 * ad) - 1) * ((log(prob.synt[ad])) +
                                    log(1 - prob.synt[n.pm + 1 - ad]))
            }
            null.dist[j, 6] <-
              -n.pm - ((1 / n.pm) * sum(soma.pm, na.rm = TRUE))
          } else {
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
                ((2 * ad) - 1) * ((log(prob.synt[ad])) +
                                    log(1 - prob.synt[n.harg + 1 - ad]))
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
                ((2 * ad) - 1) * ((log(prob.synt[ad])) +
                                    log(1 - prob.synt[n.pm + 1 - ad]))
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
      parameters <- parameters[order(parameters[, 3]), ]
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
      n.weeks <- nrow(data.at.timescale)
      pos <- 1
      SDI <- matrix(NA, n.weeks, 3)
      if (distr == "GEV") {
        while (pos <= n.weeks) {
          i <- data.at.timescale[pos, 3]
          prob <-
            adjust.prob(parameters[i, 6] + (1 - parameters[i, 6]) *
                          cdfgam(data.at.timescale[pos, 4],
                                 c(parameters[i, 4], parameters[i, 5])))

          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfgev(
              data.at.timescale[pos, 7],
              c(parameters[i, 7], parameters[i, 8], parameters[i, 9])
            ))

          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfgev(
              data.at.timescale[pos, 8],
              c(parameters[i, 10], parameters[i, 11], parameters[i, 12])
            ))

          SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      } else {
        while (pos <= n.weeks) {
          i <- data.at.timescale[pos, 3]
          prob <-
            adjust.prob(parameters[i, 6] + (1 - parameters[i, 6]) *
                          cdfgam(data.at.timescale[pos, 4],
                                 c(parameters[i, 4], parameters[i, 5])))

          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfglo(
              data.at.timescale[pos, 7],
              c(parameters[i, 7], parameters[i, 8], parameters[i, 9])
            ))

          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfglo(
              data.at.timescale[pos, 8],
              c(parameters[i, 10], parameters[i, 11], parameters[i, 12])
            ))

          SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      }
      categories <- matrix(NA, n.weeks, 3)
      # see internal_functions.R for find.category()
      categories[, 1] <- find.category(x = SDI[, 1])
      categories[, 2] <- find.category(x = SDI[, 2])
      categories[, 3] <- find.category(x = SDI[, 3])

      SDI <- cbind(data.at.timescale, SDI)
      ##### Normality checking procedures
      Norn.check <- matrix(NA, 48, 15)
      for (j in 1:48) {
        SDI.week <- as.matrix(SDI[which(SDI[, 3] == j), 9:11])
        w <- shapiro.test(SDI.week[, 1])
        w$p.value[w$p.value < 0.01] <- 0.01
        Norn.check[j, 1:3] <-
          c(w$statistic, w$p.value, abs(median((SDI.week[, 1]), na.rm = TRUE)))
        w <- shapiro.test(SDI.week[, 2])
        w$p.value[w$p.value < 0.01] <- 0.01
        Norn.check[j, 4:6] <-
          c(w$statistic, w$p.value, abs(median((SDI.week[, 2]), na.rm = TRUE)))
        w <- shapiro.test(SDI.week[, 3])
        w$p.value[w$p.value < 0.01] <- 0.01
        Norn.check[j, 7:9] <-
          c(w$statistic, w$p.value, abs(median((SDI.week[, 3]), na.rm = TRUE)))
        ###### As proposed in Wu et al. (2006)

        Norn.check[j, 10] <- ifelse((Norn.check[j, 1] < 0.960 &&
                                       Norn.check[j, 2] < 0.10 &&
                                       Norn.check[j, 3] > 0.05),
                                    "NoNormal",
                                    "Normal")

        Norn.check[j, 11] <- ifelse((Norn.check[j, 4] < 0.960 &&
                                       Norn.check[j, 5] < 0.10 &&
                                       Norn.check[j, 6] > 0.05),
                                    "NoNormal",
                                    "Normal")

        Norn.check[j, 12] <- ifelse((Norn.check[j, 7] < 0.960 &&
                                       Norn.check[j, 8] < 0.10 &&
                                       Norn.check[j, 9] > 0.05),
                                    "NoNormal",
                                    "Normal")

        ###### As proposed in Stagge et al. (2015)

        Norn.check[j, 13] <- ifelse(Norn.check[j, 2] < 0.05,
                                    "NoNormal",
                                    "Normal")

        Norn.check[j, 14] <- ifelse(Norn.check[j, 5] < 0.05,
                                    "NoNormal",
                                    "Normal")

        Norn.check[j, 15] <- ifelse(Norn.check[j, 8] < 0.05,
                                    "NoNormal",
                                    "Normal")
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
      check.quart.month.complete(final.year,
                                 final.month,
                                 final.day)
      SDI.final <- SDI.final[-c(n.weeks), ]

      row.names(SDI.final[1, ]) <- paste("TS is ", as.character(TS))
      Result <-
        list(SDI.final, parameters, Goodness, Norn.check)
      Result <- list(
        SDI = SDI.final,
        DistPar = parameters,
        GoodFit = Goodness,
        Normality = Norn.check
      )
      message("The calculations started on: ", start.date.protocal)
      return(Result)
    } else {
      for (i in 1:48) {
        month.par <- data.at.timescale[i, 3]
        rain <-
          (data.at.timescale[which(data.at.timescale[, 3] == month.par), 4])
        rain.nozero <- na.omit(rain[rain > 0])
        n.rain <- length(na.omit(rain))
        n.nonzero <- length(rain.nozero)
        n.z <- n.rain - n.nonzero
        probzero <- calc.probzero(n.z, n.rain)
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
        } else {
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
      parameters <- parameters[order(parameters[, 3]), ]
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
      n.weeks <- nrow(data.at.timescale)
      pos <- 1
      SDI <- matrix(NA, n.weeks, 3)
      if (distr == "GEV") {
        while (pos <= n.weeks) {
          i <- data.at.timescale[pos, 3]
          prob <-
            adjust.prob((parameters[i, 6] + (1 - parameters[i, 6])) *
                          cdfgam(data.at.timescale[pos, 4],
                                 c(parameters[i, 4], parameters[i, 5])))

          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfgev(data.at.timescale[pos, 7],
                   c(parameters[i, 7], parameters[i, 8],
                     parameters[i, 9])))

          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfgev(data.at.timescale[pos, 8],
                   c(parameters[i, 10], parameters[i, 11],
                     parameters[i, 12])))

          SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      } else {
        # TODO: could this just be an apply() rather than a while loop?
        while (pos <= n.weeks) {
          i <- data.at.timescale[pos, 3]
          prob <-
            adjust.prob(parameters[i, 6] + (1 - parameters[i, 6]) *
            cdfgam(data.at.timescale[pos, 4],
                   c(parameters[i, 4], parameters[i, 5])))
          SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)

          prob <-
            adjust.prob(cdfglo(data.at.timescale[pos, 7],
                   c(parameters[i, 7], parameters[i, 8],
                     parameters[i, 9])))
          SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
          prob <-
            adjust.prob(cdfglo(data.at.timescale[pos, 8],
                   c(parameters[i, 10], parameters[i, 11],
                     parameters[i, 12])))
          SDI[pos, 3] <- qnorm(prob, mean = 0, sd = 1)
          pos <- pos + 1
        }
      }
      categories <- matrix(NA, n.weeks, 3)

      categories[, 1] <- find.category(x = SDI[, 1])
      categories[, 2] <- find.category(x = SDI[, 2])
      categories[, 3] <- find.category(x = SDI[, 3])

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
      check.quart.month.complete(final.year,
                                 final.month,
                                 final.day)
      SDI.final <- SDI.final[-c(n.weeks), ]

      row.names(SDI.final[1, ]) <- paste("TS is", as.character(TS))
      Result <- list(SDI.final, parameters)
      Result <- list(SDI = SDI.final, DistPar = parameters)
      message("The calculations started on: ", start.date.protocal)
      return(Result)
    }
  }

#' Check and Remove Values Outside of Set Limits
#'
#' Takes a user-provided value, checks if values are within the boundaries,
#' emits a message with number of removed rows and set to 'na' the data falling
#' outside the boundaries.
#' @param data.week A matrix of values
#' @param col.position An integer value indicating which column should be
#'   checked
#' @param Uplim The user-defined upper limit
#' @param Lowlim The user-defined lower limit
#' @param which.lim A string indicating whether the limits are applied to rain
#'   or PE values for the message that is emitted
#'
#' @examples
#' data.week <-
#' structure(
#'   c(
#'     -47.3,
#'     -47.3,
#'     -22.87,
#'     -22.87,
#'     2015,
#'     2015,
#'     1,
#'     1,
#'     1,
#'     2,
#'     34.88,
#'     22.72,
#'     35.4347478184492,
#'     42.1418688289752,
#'     34.3122432933585,
#'     44.6334180288311
#'   ),
#'   dim = c(2L, 8L)
#' )
#'
#' # removes second row
#' check.remove.lims(data.week, 6, 45, 34, "rain")
#' @noRd
#' @keywords Internal

check.remove.lims <-
  function(data.week,
           col.position,
           Uplim,
           Lowlim,
           which.lim) {
    if (!is.null(Uplim)) {
      upremov <- which(data.week[, col.position] > Uplim)
      if (length(upremov) > 384) {
        stop(call. = FALSE,
             "There are not enough rows in the data with the limits that you ",
             "have set. Please use lower or higher limits to allow enough data ",
             "to remain.")
      }
      if (length(upremov) > 0) {
        message("Row(s) with data above limit: ",
                paste(upremov, collapse = ", "),
                " for ",
                which.lim)
        data.week[which(data.week[, col.position] > Uplim), col.position] <- Uplim
      }
    }

    if (!is.null(Lowlim)) {
      lowremov <- which(data.week[, col.position] < Lowlim)
      if (length(lowremov) > 384) {
        stop(call. = FALSE,
             "There are not enough rows in the data with the limits that you ",
             "have set. Please use lower or higher limits to allow enough data ",
             "to remain.")
      }
      if (length(lowremov) > 0) {
        message("Row(s) with data below limit: ",
                paste(lowremov, collapse = ", "),
                " for ",
                which.lim)
        data.week[which(data.week[, col.position] < Lowlim), col.position] <- Lowlim
      }
    }

    if (nrow(data.week) < 2) {
      stop(call. = FALSE,
           "There are not enough rows in the data with the limits that you ",
           "have set. Please use lower or higher limits to allow enough data ",
           "to remain.")
    }
    return(data.week)
  }
