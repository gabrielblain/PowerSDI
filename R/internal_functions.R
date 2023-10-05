
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
#' @return Validated dates in a list
#' @keywords internal
#' @noRd
check.dates <- function(dates) {
  if (is.null(dates)) {
    stop(call. = FALSE,
         "You have not entered dates for the query.\n")
  }
  if (length(unique(dates)) < 2) {
    stop(
      call. = FALSE,
      "For `temporal_api = monthly`, at least two (2) years ",
      "are required to be provided.\n"
    )
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
             "",
             x,
             " is not a valid entry for date. Enter as YYYY-MM-DD.\n")
      }
    )
    as.Date(x)
  }
  # apply function to reformat/check dates
  dates <- lapply(X = dates, FUN = date_format)
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
  if (isFALSE(is.numeric(sig.level)) || sig.level < 0.90 || sig.level > 0.95) {
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
