

#' Adjust prob Values
#'
#' Takes a probability and if <0.001351 or >0.998649 sets it to those values, if
#'   between, returns the `prob` value unchanged
#'
#' @param prob a numeric value of probability
#'
#' @examples
#' adjust.prob(0.997)
#' adjust.prob(0.0001)
#' adjust.prob(0.99999)
#'
#' @return A single numeric value.
#'
#' @noRd
#' @keywords Internal

adjust.prob <- function(prob) {
  p <- cut(
    x = prob,
    breaks = c(-Inf, 0.001351, 0.998649, Inf),
    labels = c(0.001351, prob, 0.998649)
  )
  return(as.numeric(as.character(p)))
}

#' Check User-Provided Dates for Validity
#'
#' Validates user entered dates for format
#'
#' @param dates A vector of two user entered `dates` values.
#'
#' @return Two validated \code{date} objects in a \code{list} object.
#' @keywords internal
#' @noRd
check.dates <- function(dates) {
  # put dates in list to use lapply
  dates <- as.list(dates)

  # check dates as entered by user
  date_format <- function(x) {
    tryCatch(
      # try to parse the date format using lubridate
      x <- lubridate::parse_date_time(x,
                                      c(
                                        "Ymd",
                                        "dmY",
                                        "mdY",
                                        "BdY",
                                        "Bdy",
                                        "bdY",
                                        "bdy"
                                      )),
      warning = function(c) {
        stop(call. = FALSE,
             x,
             " is not a valid entry for date. Enter as YYYY-MM-DD.\n")
      }
    )
    as.Date(x)
  }

  # apply function to reformat/check dates
  dates <- lapply(X = dates, FUN = date_format)

  if (dates[[2]] < dates[[1]]) {
    message("Your start and end dates were reversed. ",
            "They have been reordered.\n")
    dates <- c(dates[2], dates[1])
  }

  # check end date to be sure it's not in the future
  if (dates[[2]] > Sys.Date()) {
    stop(call. = FALSE,
         "The weather data cannot possibly extend beyond this day.\n")
  }

  if (dates[[2]] < dates[[1]]) {
    message("Your start and end dates were reversed. ",
            "They have been reordered.\n")
    dates <- c(dates[2], dates[1])
  }

  return(dates)
}

#' Calculate dif Value
#'
#' Given a user-supplied \code{user.start.day} value and a \code{start.week}
#'   value from \code{find.week.int}, calculate the \code{dif} value.
#' @param x A value calculated from \code{find.week.int}.
#' @param y User provided value for the start date of the day of
#'   month.
#'
#' @examples
#' # start.week 1
#' start.user.day <- 7
#' start.week <- find.week.int(start.user.day)
#' calculate.dif(start.week, start.user.day)
#'
#' # start.week 3
#' find.week.int(17)
#'
#' @keywords Internal
#' @noRd
calculate.dif <- function(x, y) {
  if (x == 1) {
    dif <- y - 1
  } else if (x == 2) {
    dif <- y - 8
  } else if (x == 3) {
    dif <- y - 15
  } else {
    dif <- y - 22
  }
  return(dif)
}

#' Check User Provided distr for Validity
#'
#' @param distr User-supplied value
#'
#' @return An upper-case string that has been validated
#' @noRd
#'
#' @examples
#' # Passes
#' check.distr(distr = "GLO")
#'
#' # Doesn't pass
#' check.distr(distr = "GLAM")
check.distr <- function(distr) {
  distr <- toupper(distr)
  if (distr != "GEV" && distr != "GLO") {
    stop("`distr` should be set to either 'GEV' or 'GLO'.",
         call. = FALSE)
  }
  return(distr)
}

#' Check That the quart.month Object Has Complete Dates
#'
#' @param end.year integer
#' @param end.month integer
#' @param end.day integer
#'
#' @examples
#' check.quart.month.complete(2002, 2, 28)
#' check.quart.month.complete(2002, 2, 21)
#'
#'
#' @noRd
#' @keywords Internal

check.quart.month.complete <-
  function(end.year,
           end.month,
           end.day) {
    shared.days <- c(7, 14, 22)

    if (end.month %in% c(4, 6, 9, 11) &
        end.day %notin% c(shared.days, 30) ||
        end.month %in% c(1, 3, 5, 7, 8, 10, 12) &
        end.day %notin% c(shared.days, 31) ||
        isFALSE(lubridate::leap_year(end.year)) && end.month == 2 &
        end.day %notin% c(shared.days, 28) ||
        lubridate::leap_year(end.year) && end.month == 2 &
        end.day %notin% c(shared.days, 29)) {
      warning("The latest quart.month period is not complete",
              call. = FALSE)
    }
  }

#' Check User Provided PEMethod for Validity
#'
#' @param PEMethod User-supplied value
#'
#' @return An upper-case string that has been validated
#' @noRd
#'
#' @examples
#' # Passes
#' check.PEMethod(PEMethod = "HS")
#'
#' # Doesn't pass
#' check.PEMethod(PEMethod = "GLO")
check.PEMethod <- function(PEMethod) {
  PEMethod <- toupper(PEMethod)
  if (PEMethod != "HS" && PEMethod != "PM") {
    stop("`PEMethod` should be set to either 'HS' or 'PM'.",
         call. = FALSE)
  }
  return(PEMethod)
}

#' Check User Provided TS for Validity
#'
#' @param PEMethod User-supplied value
#'
#' @return Called for its side-effects, an invisible `NULL`.
#' @noRd
#'
#' @examples
#' # Passes
#' check.TS(TS = 1)
#'
#' # Doesn't pass
#' check.TS(TS = 4.1)
check.TS <- function(TS) {
  if (isFALSE(TS == as.integer(TS)) || TS < 1 || TS > 96) {
    stop(
      "TS must be a whole value ranging between 1 and 96.
               Please choose another TS between 1 and 96.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

#' Check User-Input sig.level
#' sig.level User provided value
#'
#' @examples
#' # passes
#' check.sig.level(sig.level = 0.9)
#'
#' # doesn't pass
#' check.sig.level(sig.level = 0.89)
#'
#' @return Invisible NULL, called for it's side-effects
#' @keywords Internal
#' @noRd
#'
check.sig.level <- function(sig.level) {
  if (isFALSE(is.numeric(sig.level)) ||
      sig.level < 0.90 || sig.level > 0.95) {
    stop(
      "Please provide an appropriate significance level, that is:
        `sig.level` may only assume numeric values between 0.9 and 0.95.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

#' Find Wetness Category
#'
#' @param x a vector of values
#'
#' @examples
#' find.category(x = -3:3)
#'
#' @return A character string classification scheme for wetness
#' @noRd
#' @keywords Internal

find.category <- function(x) {
  as.character(cut(
    x = x,
    breaks = c(-Inf, -2, -1.5, -1, 1, 1.5, 2, Inf),
    include.lowest = TRUE,
    labels = c(
      "ext.dry",
      "sev.dry",
      "mod.dry",
      "Normal",
      "mod.wet",
      "sev.wet",
      "ext.wet"
    )
  ))
}

#' Find the quart.month Integer Value
#'
#' @param x A data.frame, usually called \dQuote{data.week}
#'
#' @examples
#' # example code
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
#'   find.quart.month.int(data.week)
#'
#' @keywords Internal
#' @noRd
#'

find.quart.month.int <- function(x) {
  M1 <-
    cbind(rep(1:12, each = 4), rep(1:4, length.out = 48))
  M2 <- cbind(x[, 4], x[, 5])

  apply(M2, 1, function(a)
    which(apply(M1, 1, function(b)
      all(b == a))))
}

#' Find a Corresponding Integer Value for a Week's Position in a Month
#'
#' Given a user-supplied \code{user.start.day} value, calculate the start.week
#'   value.
#'
#' @param x User provided value for the start date of the day of
#'   month.
#'
#' @examples
#' # start.week 1
#' find.week.int(7)
#'
#' # start.week 3
#' find.week.int(17)
#'
#' @keywords Internal
#' @noRd

find.week.int <- function(x) {
  return(cut(
    x = x,
    breaks = c(1, 7, 14, 21, 31),
    include.lowest = TRUE,
    labels = FALSE
  ))
}

#' Adds a %notin% Function
#'
#' Negates `%in%` for easier (mis)matching.
#'
#' @param x A character string to match.
#' @param table A table containing values to match `x` against.
#'
#' @return A logical vector, indicating if a mismatch was located for any
#'  element of x: thus the values are TRUE or FALSE and never NA.
#' @keywords internal
#' @noRd
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}


#' Get POWER data from NASA POWER API for PowerSDI
#'
#' @param lon user-supplied longitude
#' @param lat user-supplied latitude
#' @param start.date.user user-supplied start date
#' @param end.date.user user-supplied end date
#' @param PEMethod Method for calculating potential evapotranspiration to be
#'   used. Defaults to \code{PM}.
#'
#' @return a `data.frame`
#' @noRd
#' @keywords Internal
#'
#' @examples
#' get_sdi_power_data(lon = -47.3,
#'   lat = -22.67,
#'   start.date = "2023-06-01",
#'   end.date = "2023-06-30",
#'   PEMethod = "PM")
#'
#' @importFrom nasapower get_power

get_sdi_power_data <-
  function(lon,
           lat,
           start.date.user,
           end.date.user,
           PEMethod = "PM") {
    power_pars <- switch(
      PEMethod,
      "HS" = c("T2M",
               "T2M_MAX",
               "T2M_MIN",
               "PRECTOTCORR"),
      "PM" = c(
        "T2M",
        "T2M_MAX",
        "T2M_MIN",
        "ALLSKY_SFC_SW_DWN",
        "WS2M",
        "RH2M",
        "PRECTOTCORR"
      )
    )

    cache_file <- file.path(tempdir(), "sse_i.Rda")

    if (file.exists(cache_file)) {
      sse_i <- readRDS(cache_file)

      if (start.date.user >= min(sse_i$YYYYMMDD) &&
          end.date.user <= max(sse_i$YYYYMMDD) &&
          lon == sse_i$LON[1] &&
          lat == sse_i$LAT[1] &&
          PEMethod == sse_i$PEMethod[1]) {
        return(as.data.frame(sse_i[, !(names(sse_i) %in% "PEMethod")]))
      } else {
        # it exists but doesn't meet our needs, download new data
        return(
          PSDI_get_power(
            lon,
            lat,
            start.date.user,
            end.date.user,
            PEMethod,
            power_pars
          )
        )
      }
    }  else {
      # cache doesn't exist so we download fresh data
      return(PSDI_get_power(
        lon,
        lat,
        start.date.user,
        end.date.user,
        PEMethod,
        power_pars
      ))
    }
  }

#' Aggregates Data Download and Calculates ETP Daily Values
#'
#' Wraps functions that fetch data using `get_sdi_power_data()` and calculates
#'   the daily evapotranspiration values using `calc.ETP.daily()` for both
#'   Penman-Monteith and Hargreves and Samani methods.
#'
#' @param lon User provided longitude value
#' @param lat User provided latitude value
#' @param start.date.user Start date provided by user
#' @param end.date.user End date provided by User
#' @return A \code{list} with the POWER data, `sse_i`; evapotranspiration for
#'    PM, `ETP.pm.daily`; and HS, `ETP.hs.daily`, values.
#' @keywords Internal
#' @noRd

daily.ETP.wrapper <-
  function(lon,
           lat,
           start.date.user,
           end.date.user) {

    sse_i <-
      get_sdi_power_data(
        lon = lon,
        lat = lat,
        start.date.user = start.date.user,
        end.date.user = end.date.user
      )

    #### Hargreaves & Samani ----
    ETP.harg.daily <-
      calc.ETP.daily(
        J = sse_i$DOY,
        lat = lat,
        tavg = sse_i$T2M,
        tmax = sse_i$T2M_MAX,
        tmin = sse_i$T2M_MIN,
        method = "HS"
      )

    #### Penman- Monteith-FAO ----
    ETP.pm.daily <- calc.ETP.daily(
      J = sse_i$DOY,
      lat = lat,
      tavg = sse_i$T2M,
      tmax = sse_i$T2M_MAX,
      tmin = sse_i$T2M_MIN,
      rh = sse_i$RH2M,
      wind = sse_i$WS2M,
      rad = sse_i$ALLSKY_SFC_SW_DWN,
      method = "PM"
    )

    return(cbind(sse_i,
                 ETP.harg.daily,
                 ETP.pm.daily))
  }

#' Wrap get_power() from {nasaspower} for this package
#'
#' @param lon user provided longitude
#' @param lat user provided latitude
#' @param start.date.user user provided start date
#' @param end.date.user user provided end date
#' @param PEMethod \code{PEMethod} selected by the user
#' @param power_pars The POWER parameters to download in line with the selected
#'   \code{PEMethod}
#'
#' @return a \code{data.frame} of NASA POWER data that is also cached in tempdir
#' @noRd
#' @keywords Internal
#'

PSDI_get_power <-
  function(lon,
           lat,
           start.date.user,
           end.date.user,
           PEMethod,
           power_pars) {
    sse_i <-
      get_power(
        community = "ag",
        lonlat = c(lon, lat),
        dates = c(start.date.user,
                  end.date.user),
        temporal_api = "daily",
        pars = power_pars
      )

    # add a col for PEMethod when caching for easier checking when reading
    # cached versions against what's newly requested
    sse_i$PEMethod <- PEMethod

    saveRDS(sse_i,
            file = file.path(tempdir(), "sse_i.Rda"),
            compress = FALSE)

    # upon return drop PEMethod col that's not used except in this internal
    # fn for validation
    return(as.data.frame(sse_i[, !(names(sse_i) %in% "PEMethod")]))
  }

#' Find prob Value for the Potential Evapotranspiration Method
#' @param distr A character value of either `GLO` or `GEV`
#' @param PEMethod A character value of either `HS` or `PM`
#' @param data.at.timescale A matrix of numeric values derived from POWER data
#' @param pos_row The row to draw from in data.at.timescale
#' @param pos_col The col to draw fropm in data.at.timescale
#' @param par a `matrix` or `vector` containing the parameters
#' @param p1 The first `par` object index to draw from for `cdfgev()`
#' @param p2 The second `par` object index to draw from for `cdfgev()`
#' @param p3 The third `par` object index to draw from for `cdfgev()`
#' @param p4 The fourth `par` object index to draw from for `cdfglo()`
#' @param p5 The fifth `par` object index to draw from for `cdfglo()`
#' @param p6 The sixth `par` object index to draw from for `cdfglo()`
#' @param i if par is a `matrix`, `i` is the loop iterator and is the indexed
#'   row to select for use in calculations
#' @noRd
#' @keywords Internal
#' @importFrom lmom cdfgev cdfglo
set.PEMethod.prob <-
  function(distr,
           PEMethod,
           data.at.timescale,
           dat_row,
           dat_col,
           par,
           p1,
           p2,
           p3,
           p4,
           p5,
           p6,
           i = NULL) {
    if (is.matrix(par)) {
      if (is.null(i)) {
        stop("`par` is a matrix object, you must supply `i`.",
             call. = FALSE)
      }
      p1 <- par[i, p1]
      p2 <- par[i, p2]
      p3 <- par[i, p3]
      p4 <- par[i, p4]
      p5 <- par[i, p5]
      p6 <- par[i, p6]
    } else {
      p1 <- par[p1]
      p2 <- par[p2]
      p3 <- par[p3]
      p4 <- par[p4]
      p5 <- par[p5]
      p6 <- par[p6]
    }
    # Since there is only `GEV` and `GLO`, use `ifelse, GEV == TRUE` to test
    ifelse(distr == "GEV",
           return(switch(
             PEMethod,
             "HS" = cdfgev(data.at.timescale[dat_row, dat_col],
                           c(p1, p2, p3)),
             "PM" = cdfgev(data.at.timescale[dat_row, dat_col],
                           c(p4, p5, p6))
           )),
           return(switch(
             PEMethod,
             "HS" = cdfglo(data.at.timescale[dat_row, dat_col],
                           c(p1, p2, p3)),
             "PM" = cdfglo(data.at.timescale[dat_row, dat_col],
                           c(p4, p5, p6))
           )))
  }
