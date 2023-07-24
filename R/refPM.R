#' Example of the input required by the Reference function.
#'
#' Contains data for calculating the SPI and SPEI.
#'
#' @format A 11-column matrix with 10958 rows and 11 variables
#'
#' \describe{
#' \item{YEAR}{Year}
#' \item{MM}{Month}
#' \item{DD}{Day}
#' \item{tmed}{Daily average air temperature at 2 meters above the ground (degrees C)}
#' \item{tmax}{Daily maximum air temperature at 2 meters above the ground (degrees C)}
#' \item{tmin}{Daily minimum air temperature at 2 meters above the ground (degrees C)}
#' \item{Ra}{Daily top of the atmosphere radiation (MJ/m^2/day)}
#' \item{Rs}{Daily global horizontal irradiance (MJ/m^2/day)}
#' \item{W}{Daily average wind speed at 2 meters above the ground (m/s)}
#' \item{RH}{Daily average relative humidity at 2 meters above the ground (in percentage)}
#' \item{Rain}{Daily rainfall amounts (mm)}
#' }
#'
#' @source {Agronomic Institute and NASAPOWER}
#'
#' @examples
#' data(refPM)
"refPM"
