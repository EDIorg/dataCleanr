#' Read ISO 8601 strings into POSIXct, POSIXt
#'
#' @description
#'     Read ISO 8601 strings into POSIXct, POSIXt.
#'
#' @details
#'     `iso8601_read` provides a lightweight option for reading ISO 8601 
#'     strings created with `iso8601_convert` into the POSIXct POXIXt class. 
#'     This function uses regular expressions to extract the orders and time 
#'     zone offset arguments and passess this info to 
#'     `lubridate::parse_date_time` for parsing. 
#'     
#'     Time zone offsets are parsed "as is", i.e. using Olson Names that do not
#'     recognize Day Light Savings time. For example a time zone offset of
#'     "+06" is parsed using the Olson Name "Etc/GMT-6". Date times without
#'     a time zone offset default to "UTC". Use the argument `tz.name` to 
#'     override the input data time zone.
#'
#' @usage iso8601_read(x, tz.name = NULL)
#'
#' @param x
#'     (character) A vector of ISO 8601 strings created with `iso8601_convert`.
#'     Vector contents must be one of dates and times, dates, or times. A mix 
#'     of more than one type is not supported. A mix of input temporal 
#'     resolution is supported for date and time data.
#' @param tz.name
#'     (character) Time zone name used to override parsing of time zone offset
#'     listed in `x`. See `OlsonNames()` for valid options.
#'
#' @return
#'     (POSIXct, POSIXt) A vector of POSIXct POSIXt, unless the format is 
#'     'YYYY', in which case integer class data is returned.
#'
#' @examples 
#' # Time zone are automatically parsed
#' datetimes <- iso8601_read('2012-05-01T13:23:00+05')
#' attributes(datetimes)
#' 
#' # Time zone defaults to 'UTC' when none is specified
#' iso8601_read('2012-05-01T13:23:00')
#' 
#' # Vairance of input temporal resolution is supported
#' iso8601_read(c('2012-05-01T13:45:23+05', '2012-05-01T13:45+05', '2012-05-01T13+05'))
#' 
#' # tz.name overrides input time zone
#' iso8601_read('2012-05-01T13:45+09:30', tz.name = 'Etc/GMT+4')
#' 
#' \dontrun{
#' # Multiple time zones are not yet supported
#' iso8601_read(c('2012-05-01T13-05', '2012-05-01T13-06'))
#' iso8601_read(c('2012-05-01T13-05', '2012-05-01T13'))
#' }
#'
#' @export
#'

iso8601_read <- function(x, tz.name = NULL){

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
      x <- substr(x, start = 1, stop = (nchar(x)-nchar(tz)))
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

  # Parse datetime strings ----------------------------------------------------
  
  if (!is.null(tz)){
    
    tz <- validate_tz(tz)
    
    readable_tz <- onames[!is.na(onames$name), ]
    
    tz_name <- readable_tz$name[
      stringr::str_detect(readable_tz$offset, paste0('\\', tz))
      ]
    
    if (length(tz_name) ==0){
      tz_name <- NA_character_
    }
    
    if (((is.na(tz_name))) & (is.null(tz.name))){
      
      warning(
        paste(
          'Time zone offset is not recognized.',
          'Defaulting to UTC.',
          'Use the tz.name argument to supply a time zone name for parsing these data.'
        )
      )
      
      tz_name <- 'UTC'
      
    } else if ((is.na(tz_name)) & (!is.null(tz.name))){
      
      if (sum(OlsonNames() == tz.name) == 1){
        
        message(
          paste(
            'Parsing data with time zone offset =',
            tz.name
          )
        )
        
        tz_name <- tz.name
        
      } else {
        
        stop(
          paste(
            'tz.name =',
            tz.name,
            'is not a valid time zone (see OlsonNames()).'
          )
        )
        
      }

    }

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
        '"',
        ' Default time zone "UTC" will be used.'
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
