#' Read ISO 8601 standard character string to datetime object (POSIXct, POSIXt)
#'
#' @description
#'     Convert a character string in the ISO 8601 format to a datetime object
#'     (POSIXct, POSIXt). NOTE: Time zones are not currently supported.
#'
#' @usage iso8601_read(x)
#'
#' @param x
#'     (character) A vector of dates and times in the ISO8601 format:
#'     \itemize{
#'         \item{YYYY-MM-DD}
#'         \item{YYYY-MM-DDThh}
#'         \item{YYYY-MM-DDThh:mm}
#'         \item{YYYY-MM-DDThh:mm:ss}
#'     }
#'
#' @return
#'     A vector of datetime object (POSIXct, POSIXt).
#'
#' @export
#'

iso8601_read <- function(x){

  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }

  # Parse datetime strings ----------------------------------------------------

  x_converted <- lubridate::parse_date_time(
    x = x,
    orders = 'ymd_HMS',
    truncated = 3
  )

  # Output --------------------------------------------------------------------

  x_converted

}
