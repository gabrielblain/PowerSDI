#' Verify how well NASA-POWER Data Represent Observed Data
#'
#' Calculates scalar measures of accuracy.
#'
#' @param obs_est
#' A 2-column matrix. The reference or observed and the estimated or predicted
#'   data. See \code{ObsEst} object as an example.
#' @param conf.int
#' A character variable (\code{Yes} or \code{No}) defining if the function must
#'   calculate confidence intervals.  Default is \dQuote{Yes}.
#' @param sig.level
#' A numeric variable (between 0.90 and 0.95) defining the significance level
#'   for parameter the confidence intervals.  Default is 0.95.
#' @return
#' An object of \code{PowerSDI.Accuracy}, a \code{list}, which reports:
#'
#' \itemize{
#'  \item Absolute mean error (AME)
#'  \item Square root of the mean squared error (RMSE)
#'  \item Willmott's indices of agreement: original (dorig)
#'  \item Modified (dmod) and refined (dref)
#'  \item Pearson determination coefficient (R2), and
#'  \item If \code{conf.int="Yes"}, confidence intervals are calculated
#'  }
#'
#' @export
#'
#' @examples
#' data("ObsEst")
#' a <- Accuracy(obs_est = ObsEst, conf.int = "No")
#' a
#'
#' # A generic plotting method is also supplied
#' plot(a)
Accuracy <- function(obs_est,
                     conf.int = "Yes",
                     sig.level = 0.95) {
  # convert conf.int to lowercase for easy checking
  conf.int <- tolower(conf.int)

  check.confint(conf.int, sig.level)

  o <- (obs_est[, 1])
  p <- obs_est[, 2]
  N <- length(o)

  if (any(is.na(o)) || (any(is.na(p)))) {
    stop("Missing data are not allowed.",
         "Please, remove them from the input file object, `obs_est`.",
         call. = FALSE)
  }

  min.length <- length(as.matrix(obs_est)) / 2

  if (min.length < 10) {
    stop(
      "You must have at least 10 pairs of ObsVsEst data.
    Please, provide a longer period.",
    call. = FALSE
    )
  }

    databoot <- matrix(NA, N, 2)
    Nboots <- 10000
    dorigboot <- matrix(NA, Nboots, 1)
    drefboot <- matrix(NA, Nboots, 1)
    dmodboot <- matrix(NA, Nboots, 1)
    AMEboot <- matrix(NA, Nboots, 1)
    RMSEboot <- matrix(NA, Nboots, 1)
    RQuadboot <- matrix(NA, Nboots, 1)
    # AME
    AME <- sum((abs(p - o))) / N
    # RMSE
    RMSE <- sqrt(sum(((p - o) ^ 2)) / N)
    # Original Willmott index
    Num.orig <- sum((o - p) ^ 2)
    Den.orig <- sum((abs(p - mean(o)) + abs(o - mean(o))) ^ 2)
    dorig <- 1 - (Num.orig / Den.orig)
    # Modified Willmott index
    Num.mod <- sum(abs(o - p))
    Den.mod <- sum(abs(p - mean(o)) + abs(o - mean(o)))
    dmod <- 1 - (Num.mod / Den.mod)
    # Refined Willmott's index
    if (abs(sum(p - o)) <= 2 * sum(abs(o - mean(o)))) {
      dref <- 1 - ((sum(abs(p - o))) / (2 * sum(abs(o - mean(
        o
      )))))
    } else {
      (dref <- ((2 * sum(abs(
        o - mean(o)
      ))) / sum(abs(p - o))) - 1)
    }
    # Rquad
    RQuad <- (cor(o, p, method = "pearson"))^2

    if (conf.int == "yes") {
    message("Just a sec")
    # Bootstraping
    sig.level1 <- sig.level / 2
    for (i in 1:Nboots) {
      databoot <- obs_est[sample(nrow(obs_est), replace = TRUE), ]
      oboot <- as.matrix(databoot[, 1])
      pboot <- as.matrix(databoot[, 2])
      # AME
      AMEboot[i, 1] <- sum((abs(pboot - oboot))) / N
      RMSEboot[i, 1] <- sqrt(sum(((pboot - oboot) ^ 2)) / N)
      # Original Willmott index
      Numboot <- sum((oboot - pboot) ^ 2)
      Denboot <-
        sum((abs(pboot - mean(oboot)) + abs(oboot - mean(oboot))) ^ 2)
      dorigboot[i, 1] <- 1 - ((Numboot) / Denboot)
      # Modified Willmott index
      Numboot.mod <- sum(abs(oboot - pboot))
      Denboot.mod <-
        sum(abs(pboot - mean(oboot)) + abs(oboot - mean(oboot)))
      dmodboot[i, 1] <- 1 - (Numboot.mod / Denboot.mod)
      # Refined Willmott index
      if (abs(sum(pboot - oboot)) <= 2 * sum(abs(oboot - mean(oboot)))) {
        drefboot[i, 1] <-
          1 - ((sum(abs(
            pboot - oboot
          ))) / (2 * sum(abs(
            oboot - mean(oboot)
          ))))
      } else {
        (drefboot[i, 1] <-
           ((2 * sum(
             abs(oboot - mean(oboot))
           )) / sum(abs(
             pboot - oboot
           ))) - 1)
      }
      # Rquad
      RQuadboot[i, 1] <-
        (cor(oboot, pboot, method = "pearson"))^2
    }
    # Defining confidence intervals
    AME_CIinf <-
      quantile(AMEboot, probs = sig.level1, na.rm = TRUE)
    AME_CIsup <-
      quantile(AMEboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    RMSE_CIinf <-
      quantile(RMSEboot, probs = sig.level1, na.rm = TRUE)
    RMSE_CIsup <-
      quantile(RMSEboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    dorig_CIinf <-
      quantile(dorigboot, probs = sig.level1, na.rm = TRUE)
    dorig_CIsup <-
      quantile(dorigboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    dmod_CIinf <-
      quantile(dmodboot, probs = sig.level1, na.rm = TRUE)
    dmod_CIsup <-
      quantile(dmodboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    dref_CIinf <-
      quantile(drefboot, probs = sig.level1, na.rm = TRUE)
    dref_CIsup <-
      quantile(drefboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    RQuad_CIinf <-
      quantile(RQuadboot, probs = sig.level1, na.rm = TRUE)
    RQuad_CIsup <-
      quantile(RQuadboot,
               probs = (1 - sig.level1),
               na.rm = TRUE)
    ModelAccuracy <- cbind(
      AME_CIinf,
      AME,
      AME_CIsup,
      RMSE_CIinf,
      RMSE,
      RMSE_CIsup,
      dorig_CIinf,
      dorig,
      dorig_CIsup,
      dmod_CIinf,
      dmod,
      dmod_CIsup,
      dref_CIinf,
      dref,
      dref_CIsup,
      RQuad_CIinf,
      RQuad,
      RQuad_CIsup
    )
    colnames(ModelAccuracy) <- c(
      "AMECIinf",
      "AME",
      "AMECIsup",
      "RMSECIinf",
      "RMSE",
      "RMSECIsup",
      "dorig_CIinf",
      "dorig",
      "dorigCIsup",
      "dmodCIinf",
      "dmod",
      "dmodCIsup",
      "dref_CIinf",
      "dref",
      "dref_CIsup",
      "RQuad_CIinf",
      "RQuad",
      "RQuad_CIsup"
    )
    row.names(ModelAccuracy) <- c("")

  } else {
    ModelAccuracy <- cbind(AME, RMSE, dorig, dmod, dref, RQuad)
    colnames(ModelAccuracy) <-
      c("AME", "RMSE", "dorig", "dmod", "dref", "RQuad")
    row.names(ModelAccuracy) <- c("")
  }

  out <-
   list(o, p, ModelAccuracy, class = "PowerSDI.Accuracy")
  names(out) <- c("o", "p", "ModelAccuracy")
  class(out) <- union("PowerSDI.Accuracy", class(out))

  return(out)
}

#' Prints PowerSDI.Accuracy Objects
#'
#' Custom \code{print()} method for \code{PowerSDI.Accuracy} objects.
#'
#' @param x a \code{PowerSDI.Accuracy} object
#' @param digits The number of digits to be used after the decimal when
#'   displaying accuracy values.
#' @param ... ignored
#' @export
#'
print.PowerSDI.Accuracy <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {
  print(x$ModelAccuracy)
  invisible(x)
}

#' Plots PowerSDI.Accuracy Objects
#'
#' Custom \code{plot()} method for \code{PowerSDI.Accuracy} objects.
#'
#' @param x a `PowerSDI.Accuracy` object
#' @param ... Other parameters as passed to \code{plot()}
#' @return Nothing. Side-effect: plots graphs.
#' @export
#'
plot.PowerSDI.Accuracy <- function(x, ...) {
  plot(
    x = x$o,
    y = x$p,
    main = "",
    xlab = "Reference",
    ylab = "Estimated"
  )
}

check.confint <- function(conf.int, sig.level) {
  # fail early
  if (conf.int != "yes" && conf.int != "no") {
    stop(
      "`conf.int` should be set to either 'Yes' or 'No'.\n
  If 'Yes', the `sig.level` must be defined (a value between 0.9 and 0.95;
  default is 0.95).",
  call. = FALSE
    )
  }

  if (conf.int == "yes") {
    check.sig.level(sig.level)
  }
}
