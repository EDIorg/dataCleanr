#' Read ISO 8601 formatted strings into POSIXct, POSIXt
#'
#' @description
#'     Read ISO 8601 formatted dates and times into POSIXct, POSIXt.
#'
#' @details
#'     `iso8601_read` provides a lightweight option for reading ISO 8601 data 
#'     in the POSIXct POXIXt class. This function uses regular expressions to 
#'     extract the orders and time zone offset arguments and then passess the 
#'     info to `lubridate::parse_date_time` for parsing. Use 
#'     `lubridate::parse_date_time` for more options on handling time zone 
#'     offsets and locales.
#'
#' @usage iso8601_read(x)
#'
#' @param x
#'     (character) A vector of ISO 8601 date times created with 
#'     `dataCleanr::iso8601_convert`.
#'
#' @return
#'     (POSIXct, POSIXt) representation of date times, unless the format
#'     is 'YYYY', in which case integer class data is returned.
#'
#' @examples 
#' # Read date time strings
#' datetimes <- iso8601_read(c('2012-05-01T13:23:00', '2012-05-02T13:23:00'))
#' class(datetimes)
#' 
#' # Read date time strings with specified time zone offset
#' datetimes <- iso8601_read(c('2012-05-01T13:23:00-03', '2012-05-02T13:23:00-03'))
#' class(datetimes)
#'  
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
    
    format_str <- iso8601_get_format_string(x)
    
    if (format_str == 'YYYY-MM-DDThh:mm:ss'){
      
      x_converted <- lubridate::parse_date_time(
        x = x,
        orders = 'ymd_HMS'
      )
      
    } else if (format_str == 'YYYY-MM-DDThh:mm'){
      
      x_converted <- lubridate::parse_date_time(
        x = x,
        orders = 'ymd_HM'
      )
      
    } else if (format_str == 'YYYY-MM-DDThh'){
      
      x_converted <- lubridate::parse_date_time(
        x = x,
        orders = 'ymd_H'
      )
      
    } else if (format_str == 'YYYY-MM-DD'){
      
      x_converted <- lubridate::parse_date_time(
        x = x,
        orders = 'ymd'
      )
      
    } else if (format_str == 'YYYY'){
      
      x_converted <- as.integer(x)
      
    } else {
      
      x_converted <- NULL
      warning('Time only data is not yet supported. Please use lubridate functions to handle these data.')
      
    }
    
    
  }

  # Output --------------------------------------------------------------------

  x_converted

}
