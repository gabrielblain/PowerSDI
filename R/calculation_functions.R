
#' Calculate Daily Evapotranspiration
#' @param DOY Day of year as an integer
#' @param lat User provided latitude value
#' @param tavg Average daily temperature
#' @param tmax Daily maximum temperature
#' @param tmin Daily minimum temperature
#' @param rh Daily relative humidity
#' @param wind Daily average wind speed
#' @param rad Solar radiation
#' @param method A string indicating whether the method should be
#'   Penman-Monteith (\dQuote{PM}) or Hargreaves & Samani \dQuote{HS}.
#' @keywords Internal
#' @noRd
calc.ETP.daily <-
  function(J = NULL,
           lat = NULL,
           tavg,
           tmax,
           tmin,
           rh = NULL,
           wind = NULL,
           rad = NULL,
           Ra = NULL,
           method) {
    # the Reference() provides pre-calculated data so there is no need to
    # calculate all of these values
    if (!is.null(J) && !is.null(lat)) {
      decli <- calc.decli(J)
      lat.rad <- calc.decli.rad(lat)
      decli.rad <- calc.decli.rad(decli)
      hn.rad <- calc.hn.rad(decli.rad, lat.rad)
      hn.deg <- calc.hn.deg(hn.rad)
      dist.terra.sol <- calc.dist.terra.sol(J)
      Ra <-
        calc.Ra(dist.terra.sol, hn.deg, hn.rad, lat.rad, decli.rad)
    }

    if (method == "PM") {
      es <- calc.es(tavg)
      ea <- calc.ea(rh, es)
      slope.pressure <- calc.slope.pressure(es, tavg)
      Q0.ajust <- calc.Q0.ajust(Ra)
      Rn <-
        calc.Rn(rad,
                Q0.ajust,
                ea,
                tavg,
                tmin)
    }

    return(switch(
      method,
      "PM" =  (0.408 * slope.pressure *
                 (Rn - 0.8) + 0.063 *
                 (900 / (tavg + 273)) *
                 wind *
                 (es - ea)) / (slope.pressure + 0.063 *
                                 (1 + 0.34 * wind)),
      "HS" = 0.0023 *
        (Ra * 0.4081633) *
        (tmax - tmin) ^ 0.5 *
        (tavg + 17.8)
    ))
  }

#' Calculate Mean Daylight Hours (N) for Different Latitudes for the 15th of the Month
#' @keywords Internal
#' @noRd
calc.N <- function(hn.deg) {
  (2 * hn.deg) / 15
}

#' Calculate decli
#' @keywords Internal
#' @noRd
calc.decli <- function(DOY) {
  23.45 * sin((360 * (DOY - 80) / 365) * 0.01745329)
}

#' Calculate lat.rad
#' @keywords Internal
#' @noRd
calc.lat.rad <- function(lat) {
  lat * 0.01745329
}

#' Calculate decli.rad
#' @keywords Internal
#' @noRd
calc.decli.rad <- function(decli) {
  decli * 0.01745329
}

#' Calculate hn.rad
#' @keywords Internal
#' @noRd
calc.hn.rad <- function(decli.rad, lat.rad) {
  acos(tan(decli.rad) * -tan(lat.rad))
}

#' Calculate hn.deg
#' @keywords Internal
#' @noRd
calc.hn.deg <- function(hn.rad) {
  hn.rad * 57.29578
}

#' Calculate dist.terra.sol
#' @keywords Internal
#' @noRd
calc.dist.terra.sol <- function(DOY) {
  1 + (0.033 * cos(0.01745329 * (DOY * 0.9863014)))
}

#' Calculate Ra
#' @keywords Internal
#' @noRd
calc.Ra <- function(dist.terra.sol,
                    hn.deg,
                    hn.rad,
                    lat.rad,
                    decli.rad) {
  (37.6 * (dist.terra.sol ^ 2)) *
    (0.01745329 * hn.deg * sin(lat.rad) * sin(decli.rad) +
       (cos(lat.rad) * cos(decli.rad) * sin(hn.rad)))
}

#' Calculate es
#' @keywords Internal
#' @noRd
calc.es <- function(temp) {
  0.6108 *
    exp((17.27 * temp) / (temp + 273.3))
}

#' Calculate ea
#' @keywords Internal
#' @noRd
calc.ea <- function(rh, es) {
  (rh * es) / 100
}

#' Calculate slope.pressure
#' @keywords Internal
#' @noRd
calc.slope.pressure <- function(es, temp) {
  (4098 * es) / ((temp + 237.3) ^ 2)
}

#' Calculate Q0.ajust
#' @keywords Internal
#' @noRd
calc.Q0.ajust <- function(Ra) {
  0.75 * Ra
}

#' Calculate Rn, net radiation at the crop surface [MJ m-2 day-1]
#' @keywords Internal
#' @noRd
calc.Rn <- function(rad, Q0.ajust, ea, temp, tmin) {
  0.8 * rad -
    (1.35 * (rad / Q0.ajust) - 0.35) *
    (0.35 - (0.14 * sqrt(ea))) *
    0.0000000567 *
    (((temp ^ 4) + (tmin ^ 4)) / 2)
}

#' Calculate probzero, the Probability of Zero Rain
#'
#' @param n.z numeric value
#' @param n.rain numeric value
#'
#' @noRd
#' @keywords Internal

calc.probzero <- function(n.z, n.rain) {
  ifelse(n.z == 0, 0, (n.z + 1) / (2 * (n.rain + 1)))
}
