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
#' @return
#' A \code{data.frame} with:
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
#' data("refHS")
#' Reference(ref = refHS, distr = "GEV", PEMethod = "HS", TS = 4)
Reference <- function(ref,
                      distr = "GEV",
                      PEMethod = "HS",
                      TS = 4) {

  distr <- check.distr(distr)
  PEMethod <- check.PEMethod(PEMethod)
  check.TS(TS)

  if (PEMethod == "HS" && length(ref[1,]) != 8) {
    stop(
      "It seems that your input file (ref) has the wrong number of
                   columns. It should be 8",
      call. = FALSE
    )
  }
  if (PEMethod == "PM" && length(ref[1,]) != 11) {
    stop("It seems that your input file (ref) has the wrong number of
                   columns. It should be 11")
  }
  colnames(ref) <- switch(
    PEMethod,
    "HS" = c("YEAR",
             "MM",
             "DD",
             "tmed",
             "tmax",
             "tmin",
             "Ra",
             "Rain"),
    "PM" = c(
      "YEAR",
      "MM",
      "DD",
      "tmed",
      "tmax",
      "tmin",
      "Ra",
      "Rs",
      "W",
      "RH",
      "Rain"
    )
  )

  n.tot <- length(ref[, 1])
  end.year <- ref$YEAR[n.tot]
  end.month <- ref$MM[n.tot]
  end.day <- ref$DD[n.tot]
  start.year <- ref$YEAR[1]
  start.month <- ref$MM[1]
  start.day <- ref$DD[1]

  start.week <- calculate.week(start.day)

  if (PEMethod == "HS") {
    tmed <- ref$tmed
    tmax <- ref$tmax
    tmin <- ref$tmin
    Ra <- ref$Ra
    Rain <- ref$Rain
    ETP.harg.daily <- 0.0023 * (Ra * 0.4081633) *
      (tmax - tmin) ^ 0.5 * (tmed + 17.8)
    message("Calculating. Please wait.")
    ref <- cbind(ref, ETP.harg.daily)
    if (end.day <= 7) {
      end.week <- 1
    } else {
      if (end.day <= 14) {
        end.week <- 2
      } else {
        if (end.day <= 21) {
          end.week <- 3
        } else {
          end.week <- 4
        }
      }
    }
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
    tmed <- ref$tmed
    tmax <- ref$tmax
    tmin <- ref$tmin
    Ra <- ref$Ra
    Rs <- ref$Rs
    W <- ref$W
    RH <- ref$RH
    Rain <- ref$Rain
    es <- 0.6108 * exp((17.27 * tmed) / (tmed + 273.3))
    ea <- (RH * es) / 100
    slope.pressure <- (4098 * es) / ((tmed + 237.3) ^ 2)
    Q0.ajust <- 0.75 * Ra
    Rn <- (1 - 0.2) * Rs -
      (1.35 * (Rs / Q0.ajust) - 0.35) *
      (0.35 - (0.14 * sqrt(ea))) *
      (5.67 * 10 ^ -8) * (((tmed ^ 4) + (tmin ^ 4)) / 2)
    ETP.pm.daily <- (0.408 * slope.pressure * (Rn - 0.8) +
                       0.063 * (900 / (tmed + 273)) * W *
                       (es - ea)) / (slope.pressure + 0.063 *
                                       (1 + 0.34 * W))
    message("Calculating. Please wait.")
    ref <- cbind(ref, ETP.pm.daily)
    n.tot <- length(ref[, 1])
    end.year <- ref$YEAR[n.tot]
    end.month <- ref$MM[n.tot]
    end.day <- ref$DD[n.tot]

    end.week <- calculate.week(end.day)

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
  }
  if (start.day > 1 & start.day <= 7) {
    data.week <- data.week[c(-1), ]
  }
  if (start.day > 8 & start.day <= 14) {
    data.week <- data.week[c(-1), ]
  }
  if (start.day > 15 & start.day <= 21) {
    data.week <- data.week[c(-1), ]
  }
  if (start.day > 22) {
    data.week <- data.week[c(-1), ]
  }
  n <- length(data.week[, 1])
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
    probzero <- (n.z + 1) / (2 * (n.rain + 1))
    parameters[i, 1:4] <- c(i, pelgam(samlmu(rain.nozero)),
                            probzero)
    pep <- data.at.timescale[which(data.at.timescale[, 3] == i), 6]
    if (distr == "GEV") {
      parameters[i, 5:7] <- c(pelgev(samlmu(pep)))
    } else {
      parameters[i, 5:7] <- c(pelglo(samlmu(pep)))
    }
  }
  colnames(parameters) <- c("lastweek",
                            "alfa.gam",
                            "beta.gam",
                            "probzero.rain",
                            "loc.gev",
                            "sc.gev",
                            "sh.gev")
  n.weeks <- length(data.at.timescale[, 1])
  pos <- 1
  SDI <- matrix(NA, n.weeks, 2)
  while (pos <= n.weeks) {
    i <- data.at.timescale[pos, 3]
    prob <- parameters[i, 4] +
      (1 - parameters[i, 4]) *
      cdfgam(data.at.timescale[pos, 4], c(parameters[i, 2], parameters[i, 3]))
    if (!is.na(prob) && prob < 0.001351) {
      prob <- 0.001351
    }
    if (!is.na(prob) && prob > 0.998649) {
      prob <- 0.998649
    }
    SDI[pos, 1] <- qnorm(prob, mean = 0, sd = 1)
    if (distr == "GEV") {
      prob <- cdfgev(data.at.timescale[pos, 6],
                     c(parameters[i, 5], parameters[i, 6],
                       parameters[i, 7]))
    } else {
      prob <- cdfglo(data.at.timescale[pos, 6],
                     c(parameters[i, 5], parameters[i, 6],
                       parameters[i, 7]))
    }
    if (!is.na(prob) && prob < 0.001351) {
      prob <- 0.001351
    }
    if (!is.na(prob) && prob > 0.998649) {
      prob <- 0.998649
    }
    SDI[pos, 2] <- qnorm(prob, mean = 0, sd = 1)
    pos <- pos + 1
  }
  categories <- matrix(NA, n.weeks, 2)
  for (i in seq_along(1:n.weeks)) {
    if (SDI[i, 1] <= -2 && !is.na(SDI[i, 1])) {
      categories[i, 1] <- "ext.dry"
    } else {
      if (SDI[i, 1] <= -1.5 && !is.na(SDI[i, 1])) {
        categories[i, 1] <- "sev.dry"
      } else {
        if (SDI[i, 1] <= -1 && !is.na(SDI[i, 1])) {
          categories[i, 1] <- "mod.dry"
        } else {
          if (SDI[i, 1] <= 1 && !is.na(SDI[i, 1])) {
            categories[i, 1] <- "Normal"
          } else {
            if (SDI[i, 1] <= 1.5 && !is.na(SDI[i,
                                              1])) {
              categories[i, 1] <- "mod.wet"
            } else {
              if (SDI[i, 1] <= 2 && !is.na(SDI[i, 1])) {
                categories[i, 1] <- "sev.wet"
              } else {
                if (SDI[i, 1] > 2 && !is.na(SDI[i, 1])) {
                  categories[i, 1] <- "ext.wet"
                }
              }
            }
          }
        }
      }
    }
    if (SDI[i, 2] <= -2 && !is.na(SDI[i, 2])) {
      categories[i, 2] <- "ext.dry"
    } else {
      if (SDI[i, 2] <= -1.5 && !is.na(SDI[i, 2])) {
        categories[i, 2] <- "sev.dry"
      } else {
        if (SDI[i, 2] <= -1 && !is.na(SDI[i, 2])) {
          categories[i, 2] <- "mod.dry"
        } else {
          if (SDI[i, 2] <= 1 && !is.na(SDI[i, 2])) {
            categories[i, 2] <- "Normal"
          } else {
            if (SDI[i, 2] <= 1.5 && !is.na(SDI[i, 2])) {
              categories[i, 2] <- "mod.wet"
            } else {
              if (SDI[i, 2] <= 2 && !is.na(SDI[i, 2])) {
                categories[i, 2] <- "sev.wet"
              } else {
                if (SDI[i, 2] > 2 && !is.na(SDI[i, 2])) {
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
  if (end.month == 1 || end.month == 3 || end.month ==
      5 || end.month == 7 || end.month == 8 || end.month ==
      10 || end.month == 12) {
    if (end.day < 7 || end.day > 7 && end.day <
        14 || end.day > 14 & end.day < 22 || end.day >
        22 && end.day < 31) {
      message("The latest quart.month period is not complete")
      SDI.final <- SDI.final[-c(n.weeks), ]
    }
  }
  if (end.month == 4 || end.month == 6 || end.month ==
      9 || end.month == 11) {
    if (end.day < 7 || end.day > 7 && end.day <
        14 || end.day > 14 && end.day < 22 || end.day >
        22 && end.day < 30) {
      message("The latest quart.month period is not complete")
      SDI.final <- SDI.final[-c(n.weeks), ]
    }
  }
  if (end.month == 2) {
    if (end.day < 7 || end.day > 7 && end.day <
        14 || end.day > 14 && end.day < 22 || end.day >
        22 && end.day < 28) {
      message("The latest quart.month period is not complete")
      SDI.final <- SDI.final[-c(n.weeks), ]
    }
  }
  return(SDI.final)
}
