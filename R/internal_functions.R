
#' Calculate start.week Value
#'
#' Given a user-supplied \code{user.start.day} value, calculate the start.week
#'   value.
#'
#' @param x User provided value for the start date of the day of
#'   month.
#'
#' @examples
#' # start.week 1
#' calculate.week(7)
#'
#' # start.week 3
#' calculate.week(17)
#'
#' @keywords Internal
#' @noRd

calculate.week <- function(x) {
  return(cut(
    x = x,
    breaks = c(1, 7, 14, 21, 31),
    include.lowest = TRUE,
    labels = FALSE
  ))
}

#' Calculate dif Value
#'
#' Given a user-supplied \code{user.start.day} value and a \code{start.week}
#'   value from \code{calculate.week}, calculate the \code{dif} value.
#' @param x A value calculated from \code{calculate.week}.
#' @param y User provided value for the start date of the day of
#'   month.
#'
#' @examples
#' # start.week 1
#' start.user.day <- 7
#' start.week <- calculate.week(start.user.day)
#' calculate.dif(start.week, start.user.day)
#'
#' # start.week 3
#' calculate.week(17)
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
}

#' Check Dates for Validity
#'
#' Validates user entered date values for format and order
#'
#' @param user.dates A vector of user entered date values.
#'
#' @return A list object of validated date values
#' @keywords internal
#' @noRd
check.dates <- function(user.dates) {
  tryCatch(
    # check dates as entered by user
    # set up function to use in lapply() below
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
      return(as.Date(x))
    }
  )

  # apply function to reformat/check dates
  dates <- lapply(X = user.dates, FUN = date_format)

  # if the stdate is > endate, flip order
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

  mim.date.fit <-
    as.numeric((dates[[2]] - dates[[1]]) / 365)
  if (mim.date.fit < 8) {
    stop(
      "Please select a longer period between the `start.date` and
             `end.date` values.",
      call. = FALSE
    )
  }
  return(dates)
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
}
