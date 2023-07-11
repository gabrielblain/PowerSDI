#' Example of the input required by the Reference.R function.
#'
#' Contains data for calculating the SPI and SPEI.
#'
#' @format A 8-column matrix with 10950 rows and 8 variables
#' \describe{
#' \item{YEAR}{Year}
#' \item{MM}{Month}
#' \item{DD}{Day}
#' \item{tmed}{Daily average air temperature at 2 meters above the ground (ºC)}
#' \item{tmax}{Daily maximum air temperature at 2 meters above the ground (ºC)}
#' \item{tmin}{Daily minimum air temperature at 2 meters above the ground (ºC)}
#' \item{Ra}{Daily top of the atmosphere radiation (MJ/m^2/day)}
#' \item{Rain}{Daily rainfall amounts (mm)}
#' }
#'
#' @source {Agronomic Institute and NASAPOWER}
#'
#' @examples
#' data(refHS)
"refHS"
