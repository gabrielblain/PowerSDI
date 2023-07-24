#' Accuracy
#'
#' Calculates scalar measures of accuracy.
#'
#' @param obs_est
#' A 2-column matrix. The reference or observed and the estimated or predicted data.
#' See ObsEst as example.
#' @param conf.int
#' A character variable ("Yes" or "No") defining if the function must calculate confidence intervals.
#' Default is "Yes".
#' @param sig.level
#' A numeric variable (between 0.90 and 0.95) defining the significance level for parameter the confidence intervals.
#' Default is 0.95.
#' @return
#' Absolute mean error (AME)
#' Square root of the mean squared error (RMSE)
#' Willmott's indices of agreement: original (dorig)
#' Modified (dmod) and refined (dref)
#' Pearson determination coefficient (R2).
#' If conf.int="Yes", confidence intervals are calculated.
#' @export
#'
#' @examples
#' data("ObsEst")
#' Accuracy(obs_est = ObsEst, conf.int = "No")
Accuracy <- function(obs_est, conf.int = "Yes", sig.level = 0.95) {
  if (conf.int == "Yes" || conf.int == "YES" || conf.int == "YeS" || conf.int == "YEs" || conf.int == "yes" ||
    conf.int == "NO" || conf.int == "No" || conf.int == "nO" || conf.int == "no") {
    obs_est <- na.omit(obs_est)
    min.length <- length(as.matrix(obs_est))/2
    if (min.length >= 10) {
      o <- (obs_est[, 1])
      p <- obs_est[, 2]
      N <- length(o)
      N1 <- length(p)
      if (N != N1) {
        print("Observed/reference and estimated/predicted data must have the same length")
      } else {
        if (any(is.na(o)) == TRUE || (any(is.na(p) == TRUE))) {
          print("Missing data is not allowed. Please, remove them from the input file")
        } else {
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
          RMSE <- sqrt(sum(((p - o)^2)) / N)
          # Original Willmott index
          Num.orig <- sum((o - p)^2)
          Den.orig <- sum((abs(p - mean(o)) + abs(o - mean(o)))^2)
          dorig <- 1 - (Num.orig / Den.orig)
          # Modified Willmott index
          Num.mod <- sum(abs(o - p))
          Den.mod <- sum(abs(p - mean(o)) + abs(o - mean(o)))
          dmod <- 1 - (Num.mod / Den.mod)
          # Refined Willmott's index
          if (abs(sum(p - o)) <= 2 * sum(abs(o - mean(o)))) {
            dref <- 1 - ((sum(abs(p - o))) / (2 * sum(abs(o - mean(o)))))
          } else {
            (dref <- ((2 * sum(abs(o - mean(o)))) / sum(abs(p - o))) - 1)
          }
          # Rquad
          RQuad <- cor(o, p, method = "pearson")
          if (conf.int == "Yes" || conf.int == "YES" || conf.int == "YeS" || conf.int == "YEs" || conf.int == "yes") {
            if (is.numeric(sig.level) == FALSE ||
              sig.level < 0.90 || sig.level > 0.95) {
              stop("Please provide an appropriate significance level, that is:
          sig.level may only assume values between 0.9 and 0.95.")
            }
            message("Just a sec")
            # Bootstraping
            sig.level1 <- sig.level / 2
            for (i in 1:Nboots) {
              databoot <- obs_est[sample(nrow(obs_est), replace = TRUE), ]
              oboot <- as.matrix(databoot[, 1])
              pboot <- as.matrix(databoot[, 2])
              # AME
              AMEboot[i, 1] <- sum((abs(pboot - oboot))) / N
              RMSEboot[i, 1] <- sqrt(sum(((pboot - oboot)^2)) / N)
              # Original Willmott index
              Numboot <- sum((oboot - pboot)^2)
              Denboot <- sum((abs(pboot - mean(oboot)) + abs(oboot - mean(oboot)))^2)
              dorigboot[i, 1] <- 1 - ((Numboot) / Denboot)
              # Modified Willmott index
              Numboot.mod <- sum(abs(oboot - pboot))
              Denboot.mod <- sum(abs(pboot - mean(oboot)) + abs(oboot - mean(oboot)))
              dmodboot[i, 1] <- 1 - (Numboot.mod / Denboot.mod)
              # Refined Willmott index
              if (abs(sum(pboot - oboot)) <= 2 * sum(abs(oboot - mean(oboot)))) {
                drefboot[i, 1] <- 1 - ((sum(abs(pboot - oboot))) / (2 * sum(abs(oboot - mean(oboot)))))
              } else {
                (drefboot[i, 1] <- ((2 * sum(abs(oboot - mean(oboot)))) / sum(abs(pboot - oboot))) - 1)
              }
              # Rquad
              RQuadboot[i, 1] <- cor(oboot, pboot, method = "pearson")
            }
            # Defining confidence intervals
            AME_CIinf <- quantile(AMEboot, probs = sig.level1, na.rm = T)
            AME_CIsup <- quantile(AMEboot, probs = (1 - sig.level1), na.rm = T)
            RMSE_CIinf <- quantile(RMSEboot, probs = sig.level1, na.rm = T)
            RMSE_CIsup <- quantile(RMSEboot, probs = (1 - sig.level1), na.rm = T)
            dorig_CIinf <- quantile(dorigboot, probs = sig.level1, na.rm = T)
            dorig_CIsup <- quantile(dorigboot, probs = (1 - sig.level1), na.rm = T)
            dmod_CIinf <- quantile(dmodboot, probs = sig.level1, na.rm = T)
            dmod_CIsup <- quantile(dmodboot, probs = (1 - sig.level1), na.rm = T)
            dref_CIinf <- quantile(drefboot, probs = sig.level1, na.rm = T)
            dref_CIsup <- quantile(drefboot, probs = (1 - sig.level1), na.rm = T)
            RQuad_CIinf <- quantile(RQuadboot, probs = sig.level1, na.rm = T)
            RQuad_CIsup <- quantile(RQuadboot, probs = (1 - sig.level1), na.rm = T)
            ModelAcuracy <- cbind(
              AME_CIinf, AME, AME_CIsup, RMSE_CIinf, RMSE, RMSE_CIsup,
              dorig_CIinf, dorig, dorig_CIsup,
              dmod_CIinf, dmod, dmod_CIsup, dref_CIinf, dref, dref_CIsup,
              RQuad_CIinf, RQuad, RQuad_CIsup
            )
            colnames(ModelAcuracy) <- c(
              "AMECIinf", "AME", "AMECIsup", "RMSECIinf", "RMSE", "RMSECIsup",
              "dorig_CIinf", "dorig", "dorigCIsup",
              "dmodCIinf", "dmod", "dmodCIsup", "dref_CIinf", "dref", "dref_CIsup",
              "RQuad_CIinf", "RQuad", "RQuad_CIsup"
            )
            row.names(ModelAcuracy) <- c("")

          } else {
            ModelAcuracy <- cbind(AME, RMSE, dorig, dmod, dref, RQuad)
            colnames(ModelAcuracy) <- c("AME", "RMSE", "dorig", "dmod", "dref", "RQuad")
            row.names(ModelAcuracy) <- c("")

          }
          return(ModelAcuracy)
          plot(o, p)
          title("", xlab = "Reference", ylab = "Estimated")
        }
      }
    } else {
      message("You must have at least 10 pairs of ObsVsEst data.
    Please, provide a longer period.")
    }
  } else {
    message("conf.int should be set to either Yes or No.
  If Yes, the sig.level must be defined (avalue between 0.9 and 0.95; default is 0.95).")
  }
}