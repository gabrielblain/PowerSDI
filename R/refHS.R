#' Example of the Input Required by the Reference Function
#'
#' Contains data for calculating the SPI and SPEI.
#'
#' @format A \code{data.frame} with 10950 rows and 8 variables.
#'
#' \describe{
#' \item{YEAR}{Year}
#' \item{MM}{Month}
#' \item{DD}{Day}
#' \item{tmed}{Daily average air temperature at 2 meters above the ground (degrees C)}
#' \item{tmax}{Daily maximum air temperature at 2 meters above the ground (degrees C)}
#' \item{tmin}{Daily minimum air temperature at 2 meters above the ground (degrees C)}
#' \item{Ra}{Daily top of the atmosphere radiation (MJ/m^2/day)}
#' \item{Rain}{Daily rainfall amounts (mm)}
#' }
#'
#' @source Agronomic Institute and \acronym{NASA} \acronym{POWER}.
#'
#' @examples
#' data(refHS)
"refHS"
