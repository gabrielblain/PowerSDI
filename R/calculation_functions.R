#' Calculate decli
#' @keywords Internal
#' @noRd
calc.decli <- function(sse_i) {
  23.45 * sin((360 * (sse_i$DOY - 80) / 365) * 0.01745329)
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

#' Calculate N
#' @keywords Internal
#' @noRd
calc.N <- function(hn.deg) {
  (2 * hn.deg) / 15
}

#' Calculate dist.terra.sol
#' @keywords Internal
#' @noRd
calc.dist.terra.sol <- function(sse_i) {
  1 + (0.033 * cos((0.01745329) * (sse_i$DOY * 0.9863014)))
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

#' Calculate ETP.harg.daily
#' @keywords Internal
#' @noRd
calc.ETP.harg.daily <- function(Ra, sse_i) {
  0.0023 *
    (Ra * 0.4081633) *
    (sse_i$T2M_MAX - sse_i$T2M_MIN) ^ 0.5 *
    (sse_i$T2M + 17.8)
}

#' Calculate es
#' @keywords Internal
#' @noRd
calc.es <- function(sse_i) {
  0.6108 *
    exp((17.27 * sse_i$T2M) / (sse_i$T2M + 273.3))
}

#' Calculate ea
#' @keywords Internal
#' @noRd
calc.ea <- function(sse_i, es) {
  (sse_i$RH2M * es) / 100
}

#' Calculate slope.pressure
#' @keywords Internal
#' @noRd
calc.slope.pressure <- function(es, sse_i) {
  (4098 * es) / ((sse_i$T2M + 237.3) ^ 2)
}

#' Calculate Q0.ajust
#' @keywords Internal
#' @noRd
calc.Q0.ajust <- function(Ra) {
  0.75 * Ra
}

#' Calculate Rn
#' @keywords Internal
#' @noRd
calc.Rn <- function(sse_i, Q0.ajust, ea) {
  (1 - 0.2) *
    sse_i$ALLSKY_SFC_SW_DWN -
    (1.35 * (sse_i$ALLSKY_SFC_SW_DWN / Q0.ajust) - 0.35) *
    (0.35 - (0.14 * sqrt(ea))) *
    (5.67 * 10 ^ -8) *
    (((sse_i$T2M ^ 4) + (sse_i$T2M_MIN ^ 4)) / 2)
}

#' Calculate ETP.pm.daily
#' @keywords Internal
#' @noRd
calc.ETP.pm.daily <- function(slope.pressure, sse_i, es, ea) {
  (0.408 * slope.pressure *
     (Rn - 0.8) + 0.063 *
     (900 / (sse_i$T2M + 273)) *
     sse_i$WS2M *
     (es - ea)) / (slope.pressure + 0.063 *
                     (1 + 0.34 * sse_i$WS2M))
}
