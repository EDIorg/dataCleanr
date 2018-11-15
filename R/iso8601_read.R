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
  x <- x[!is.na(x)]
  
  # Separate timezones if present ---------------------------------------------
  
  if (sum(stringr::str_detect(x, 'T')) > 0){
    x_sample <- x[stringr::str_detect(x, 'T')]
    x_sample <- stringr::str_remove_all(x_sample[1], pattern = '.+[T]')
    if ((sum(stringr::str_detect(x_sample, '\\+')) > 0)){
      tz <- stringr::str_extract(x_sample, pattern = '[\\+].+')
      x <- substr(x, start = 1, stop = (nchar(x)-3))
    } else if ((sum(stringr::str_detect(x_sample, '\\-')) > 0)){
      tz <- stringr::str_extract(x_sample, pattern = '[\\-].+')
      x <- substr(x, start = 1, stop = (nchar(x)-3))
    } else {
      tz <- NULL
    }
  } else {
    tz <- NULL
  }
  
  # Get timezone name ---------------------------------------------------------
  
  onames <- utils::read.table(
    system.file('time_zones.txt', package = 'dataCleanr'),
    sep = '\t', 
    header = TRUE, 
    as.is = TRUE
    )
  
  tz_name <- onames$name[onames$offset == as.numeric(tz)] # make character

  # Parse datetime strings ----------------------------------------------------

  if (!is.null(tz)){
    x_converted <- lubridate::parse_date_time(
      x = x,
      orders = 'ymd_HMS',
      truncated = 3, 
      tz = tz_name
    )
  } else {
    x_converted <- lubridate::parse_date_time(
      x = x,
      orders = 'ymd_HMS',
      truncated = 3
    )
  }

  # Output --------------------------------------------------------------------

  x_converted

}
