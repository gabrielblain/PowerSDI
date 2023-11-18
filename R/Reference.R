#' Calculate the SPI and SPEI Using a Reference Data Source
#'
#' Calculates the Standardised Precipitation Index (\acronym{SPI}) and
#'   Standardised Precipitation-Evapotranspiration Index (\acronym{SPEI}) using
#'   a reference data source.
#'
#' @param ref
#' A data frame with the variables required for calculating the \acronym{SDI}s.
#'   See \code{refHS} or \code{refPM} as examples.
#' @param distr
#' A character variable (\dQuote{GEV} or \dQuote{GLO}) defining which
#'   distribution is used to calculate the \acronym{SPEI}. Default is
#'   \dQuote{GEV}.
#' @param PEMethod
#' A character variable (\dQuote{HS} or \dQuote{PM}) defining the potential
#'   evapotranspiration method.  Default is \dQuote{HS}.
#' @param TS
#' Time scale on the \dQuote{quart.month} basis (whole values between 1 and
#'   96).  Default is 4.
#'
#' @return
#' A data frame with five columns
#' \itemize{
#'   \item rain,
#'   \item potential evapotranspiration,
#'   \item difference between rainfall and potential evapotranspiration,
#'   \item \acronym{SPI} calculated at the time scale selected by the user, and
#'   \item \acronym{SPIE} calculated at the time scale selected by the user
#'   }
#'
#' @export
#'
#' @examples
#'
#' Reference(ref = refHS, distr = "GEV", PEMethod = "HS", TS = 4)
#'
Reference <- function(ref,
                      distr = "GEV",
                      PEMethod = "HS",
                      TS = 4L) {

  distr <- check.distr(distr)
  PEMethod <- check.PEMethod(PEMethod)
  check.TS(TS)

  if (PEMethod == "HS" && ncol(ref) != 8) {
    stop(
      "It seems that your input file (ref) has the wrong number of ",
      "columns. It should be 8.",
      call. = FALSE
    )
  } else if (PEMethod == "PM" && ncol(ref) != 11) {
    stop("It seems that your input file (ref) has the wrong number of ",
         "columns. It should be 11.")
  }

  n.tot <- nrow(ref)
  end.year <- ref$YEAR[n.tot]
  end.month <- ref$MM[n.tot]
  end.day <- ref$DD[n.tot]
  start.year <- ref$YEAR[1]
  start.month <- ref$MM[1]
  start.day <- ref$DD[1]

  start.week <- find.week.int(start.day)

  if (PEMethod == "HS") {
    tavg <- ref$tavg
    tmax <- ref$tmax
    tmin <- ref$tmin
    Ra <- ref$Ra
    Rain <- ref$Rain

    ETP.harg.daily <-
      calc.ETP.daily(
        tavg = tavg,
        tmax = tmax,
        tmin = tmin,
        Ra = Ra,
        method = PEMethod
      )

    message("Calculating. Please wait.")

    ref <- cbind(ref, ETP.harg.daily)

    end.week <- find.week.int(end.day)
    n.years <- 1 + (end.year - start.year)
    total.nweeks <- 48 * n.years
    a <- 1
    b <- 2
    c <- 3
    d <- 4
    data.week <- matrix(NA, total.nweeks, 5)
    for (year in start.year:end.year) {
      for (month in seq_along(1:12)) {
        data.week1 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD <= 7),
                                  8:9])
        data.week2 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 7 &
                                          ref$DD <= 14), 8:9])
        data.week3 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 14 &
                                          ref$DD <= 21), 8:9])
        data.week4 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 21),
                                  8:9])
        data.week[a, ] <- c(year, month, 1, data.week1)
        data.week[b, ] <- c(year, month, 2, data.week2)
        data.week[c, ] <- c(year, month, 3, data.week3)
        data.week[d, ] <- c(year, month, 4, data.week4)
        a <- a + 4
        b <- b + 4
        c <- c + 4
        d <- d + 4
      }
    }
  }
  if (PEMethod == "PM") {
    tavg <- ref$tavg
    tmax <- ref$tmax
    tmin <- ref$tmin
    Ra <- ref$Ra
    Rs <- ref$Rs
    W <- ref$W
    RH <- ref$RH
    Rain <- ref$Rain

    ETP.pm.daily <-
      calc.ETP.daily(
        tavg = tavg,
        tmax = tmax,
        tmin = tmin,
        wind = W,
        rh = RH,
        Ra = Ra,
        rad = Rs,
        method = PEMethod
      )

    message("Calculating. Please wait.")

    ref <- cbind(ref, ETP.pm.daily)
    n.tot <- nrow(ref)
    end.year <- ref$YEAR[n.tot]
    end.month <- ref$MM[n.tot]
    end.day <- ref$DD[n.tot]

    end.week <- find.week.int(end.day)

    n.years <- 1 + (end.year - start.year)
    total.nweeks <- 48 * n.years
    a <- 1
    b <- 2
    c <- 3
    d <- 4
    data.week <- matrix(NA, total.nweeks, 5)
    for (year in start.year:end.year) {
      for (month in seq_along(1:12)) {
        data.week1 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD <= 7),
                                  11:12])
        data.week2 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 7 &
                                          ref$DD <= 14), 11:12])
        data.week3 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 14 &
                                          ref$DD <= 21), 11:12])
        data.week4 <- colSums(ref[which(ref$YEAR ==
                                          year &
                                          ref$MM == month & ref$DD > 21),
                                  11:12])
        data.week[a, ] <- c(year, month, 1, data.week1)
        data.week[b, ] <- c(year, month, 2, data.week2)
        data.week[c, ] <- c(year, month, 3, data.week3)
        data.week[d, ] <- c(year, month, 4, data.week4)
        a <- a + 4
        b <- b + 4
        c <- c + 4
        d <- d + 4
      }
    }
  }
  rows <-
    which(data.week[, 1] == end.year & data.week[, 2] > end.month)
  n.rows <- length(rows)
  if (n.rows > 0) {
    data.week <- data.week[-c(rows), ]
  }
  rows <-
    which(data.week[, 1] == end.year & data.week[, 2] ==
            end.month & data.week[, 3] > end.week)
  n.rows <- length(rows)
  if (n.rows > 0) {
    data.week <- data.week[-c(rows), ]
  }
  data.week <- cbind(data.week, rep(1:48, n.years))
  first.row <- which(data.week[, 1] == start.year &
                       data.week[, 2] == start.month &
                       data.week[, 3] == start.week)


  if (first.row > 1) {
    data.week <- data.week[-(1:(first.row - 1)), ]
  } else {
    data.week <- data.week[c(-1), ]
  }

  n <- nrow(data.week)
  data.at.timescale <- matrix(NA, (n - (TS - 1)),
                              5)
  end.point <- n - (TS - 1)
  if (TS > 1) {
    point <- 1
    a <- 1
    b <- TS
    c <- 1
    data.at.timescale[c, ] <- c(data.week[b, 1:2],
                                data.week[b, 6],
                                colSums(data.week[a:b, 4:5]))
    point <- point + 1
    a <- a + 1
    b <- b + 1
    c <- c + 1
    while (point <= end.point) {
      data.at.timescale[c, ] <- c(data.week[b, 1:2],
                                  data.week[b, 6],
                                  colSums(data.week[a:b, 4:5]))
      point <- point + 1
      a <- a + 1
      b <- b + 1
      c <- c + 1
    }
  } else {
    data.at.timescale <- cbind(data.week[, 1:2],
                               data.week[, 6],
                               data.week[, 4:5])
  }
  data.at.timescale <- cbind(data.at.timescale,
                             (data.at.timescale[, 4] -
                                data.at.timescale[, 5]))
  parameters <- matrix(NA, 48, 7)

  for (i in seq_along(1:48)) {
    rain <- data.at.timescale[which(data.at.timescale[, 3] == i), 4]
    rain.nozero <- rain[rain > 0]
    n.rain <- length(rain)
    n.nonzero <- length(rain.nozero)
    n.z <- n.rain - n.nonzero
    probzero <- calc.probzero(n.z, n.rain)
    parameters[i, 1:4] <- c(i, pelgam(samlmu(rain.nozero)),
                            probzero)
    pep <- data.at.timescale[which(data.at.timescale[, 3] == i), 6]

    parameters[i, 5:7] <- switch(
        distr,
        "GEV" = c(pelgev(samlmu(pep))),
        c(pelglo(samlmu(pep)))
    )
  }

  colnames(parameters) <- c("lastweek",
                            "alfa.gam",
                            "beta.gam",
                            "probzero.rain",
                            "loc.gev",
                            "sc.gev",
                            "sh.gev")
  n.weeks <- nrow(data.at.timescale)
  pos <- 1
  SDI <- matrix(NA, n.weeks, 2)
  while (pos <= n.weeks) {
    i <- data.at.timescale[pos, 3]
    prob <- parameters[i, 4] +
      (1 - parameters[i, 4]) *
      cdfgam(data.at.timescale[pos, 4], c(parameters[i, 2], parameters[i, 3]))
    prob <- adjust.prob(prob)
    SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)

    prob <-
      set.PEMethod.prob(
        distr,
        PEMethod,
        data.at.timescale,
        dat_row = pos,
        dat_col = 6,
        par = parameters,
        p1 = 5,
        p2 = 6,
        p3 = 7,
        p4 = 5,
        p5 = 6,
        p6 = 7,
      i = i
      )

    prob <- adjust.prob(prob)

    SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
    pos <- pos + 1
  }
  categories <- matrix(NA, n.weeks, 2)

  # see internal_functions.R for find.category()
  categories[, 1] <- find.category(x = SDI[, 1])
  categories[, 2] <- find.category(x = SDI[, 2])

  SDI <- cbind(data.at.timescale, SDI)
  SDI.final <- data.frame(SDI, categories)
  colnames(SDI.final) <- c(
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

  check.quart.month.complete(end.year, end.month, end.day)
  SDI.final <- SDI.final[-c(n.weeks), ]
  return(SDI.final)
}
