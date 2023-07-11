#' Parameters of the gamma and GEV distribution for calculating the SPI and SPEI
#'
#' This data can be provided by the ScientSDI function.
#'
#' @format A matrix with 48 rows and 13 variables:
#' \describe{
#' \item{lon}{Longitute of the weather station of Campinas-SP}
#' \item{lat}{Latitude of the weather station of Campinas-SP}
#' \item{quart.month}{The quasi-weekly period}
#' \item{alfa.rain}{Shape parameter of the gamma distribution}
#' \item{beta.rain}{Scale parameter of the gamma distribution}
#' \item{probzero.rain}{Probability of rain equal to zero}
#' \item{loc.harg}{Location parameter of the GEV distribution. PE calculated by HS method}
#' \item{sc.harg}{Scale parameter of the GEV distribution. PE calculated by HS method}
#' \item{sh.harg}{Shape parameter of the GEV distribution. PE calculated by HS method}
#' \item{loc.pm}{Location parameter of the GEV distribution. PE calculated by PM method}
#' \item{sc.pm}{Scale parameter of the GEV distribution. PE calculated by PM method}
#' \item{sh.pm}{Shape parameter of the GEV distribution. PE calculated by PM method}
#' }
#'
#' @source Calculated using the ScientSDI function
#'
#' @examples
#' data(DistPar)
"DistPar"
