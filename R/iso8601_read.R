#' Read ISO 8601 strings into POSIXct, POSIXt
#'
#' @description
#'     Read ISO 8601 strings into POSIXct, POSIXt.
#'
#' @details
#'     `iso8601_read` provides a lightweight option for reading ISO 8601 
#'     strings into the POSIXct POXIXt class. This function uses regular 
#'     expressions to extract the orders and time zone offset arguments and 
#'     passess this info to `lubridate::parse_date_time` for parsing.
#'
#' @usage iso8601_read(x)
#'
#' @param x
#'     (character) A vector of ISO 8601 strings created with `iso8601_convert`.
#'     Vector contents must be one of dates and times, dates, or times. A mix 
#'     of more than one type is not supported. A mix of input temporal 
#'     resolution is supported for date and time data.
#'
#' @return
#'     (POSIXct, POSIXt) A vector of POSIXct POSIXt, unless the format is 
#'     'YYYY', in which case integer class data is returned.
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
#' @export
#'

iso8601_read <- function(x){

  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  
  vfr <- validate_format_rules(x)
  
  x <- x[!is.na(x)]
  
  # Separate timezone(s) if present -------------------------------------------
  
  if (sum(stringr::str_detect(x, 'T')) > 0){
    x_sample <- x[stringr::str_detect(x, 'T')]
    x_sample <- stringr::str_remove_all(x_sample, pattern = '.+[T]')
    if ((sum(stringr::str_detect(x_sample, '(\\+)|(\\-)')) > 0)){
      tz <- stringr::str_extract(x_sample, pattern = '([\\+].+)|([\\-].+)')
      x <- substr(x, start = 1, stop = (nchar(x)-3))
      if (length(unique(tz)) > 1){
        stop(
          paste(
            'Input "x" contains more than one time zone. iso8601_read() does',
            'not support data with multiple time zones.'
          )
        )
      }
      tz <- unique(tz)
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
    
    format_str <- suppressWarnings(
      iso8601_get_format_string(x)
    )
    
    message(
      paste0(
        'Parsing data with format specifier "',
        format_str,
        '"'
      )
    )
    
    if ((format_str == 'YYYY-MM-DDThh:mm:ss') |
        (format_str == 'YYYY-MM-DDThh:mm') |
        (format_str == 'YYYY-MM-DDThh')){
      
      x_converted <- lubridate::parse_date_time(
        x = x,
        orders = 'ymd_HMS',
        truncated = 3
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
      stop('Time only data is not yet supported.')
      
    }
    
  }

  # Output --------------------------------------------------------------------

  x_converted

}
