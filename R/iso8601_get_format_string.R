#' Get the format used in a vector of ISO 8601 strings
#'
#' @description
#'     Get the format (e.g. 'YYYY-MM-DD') used in a vector of ISO 8601 strings.
#'     Supported formats are output by `iso8601_convert`.
#'     
#' @details
#'     `iso8601_get_format_string` uses regular expressions to parse input data 
#'     and identify the most common format present in a vector of ISO 8601 
#'     strings.
#'
#' @usage iso8601_get_format_string(x)
#'
#' @param x
#'     (character) A vector of ISO 8601 strings created with `iso8601_convert`.
#'     Vector contents must be one of dates and times, dates, or times. A mix 
#'     of more than one type is not supported.
#'
#' @return
#'     (character) The date time format representing user supplied ISO 8601 
#'     data. If more than one date time format is present, then the mode
#'     is returned along with a warning message. An error is issued when input 
#'     data is a mix of datetime, date, or time data.
#'
#' @examples 
#' # Get format strings
#' iso8601_get_format_string('2012-05-01')
#' iso8601_get_format_string('2012-05-01T13')
#' iso8601_get_format_string('2012-05-01T13:29:54+05')
#' 
#' # Expect a warning when more than one format is present.
#' iso8601_get_format_string(c('2012-05-01', '2012-05-02', '2012-05-03T13:45:30'))
#' 
#' # Expect an error when a mix of datetime, date, or time data are present.
#' iso8601_get_format_string(c('2012-05-01', '2012-05-03T13:45:30'))
#' iso8601_get_format_string(c('2012-05-01', '13:45:30'))
#' iso8601_get_format_string(c('2012-05-03T13:45:30', '13:45:30'))
#' 
#' @export
#'

iso8601_get_format_string <- function(x){

  # Load helper functions -----------------------------------------------------
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  
  if (sum(is.na(x)) == length(x)){
    stop('Input argument "x" cannot be entirely NA.')
  }
  
  vfr <- validate_format_rules(x)

  # Detect data type and time zone presence -----------------------------------
  
  x <- x[!is.na(x)]
  
  if (Mode(stringr::str_count(x, 'T')) > 0){
    
    data_type <- 'datetime'
    
    t_sample <- stringr::str_remove_all(x, pattern = '.+[T]')
    
    if (Mode(stringr::str_count(t_sample, '\\-|\\+')) > 0){
      
      tz_sign <- '\u00B1'
      
    } else {
      
      tz_sign <- NULL
      
    }

  } else if (Mode(stringr::str_count(x, ':')) > 0){
    
    data_type <- 'time'
    
    if (stringr::str_count(x, '\\-|\\+') == 1){
      
      tz_sign <- '\u00B1'

    } else {
      
      tz_sign <- NULL
      
    }
    
  } else if (Mode(stringr::str_count(x, '\\-')) == 2){
    
    data_type <- 'date'
    
    tz_sign <- NULL
    
  } else {
    
    if (Mode(nchar(x)) == 4){
      
      data_type <- 'date'
      
      tz_sign <- NULL
      
    } else {
      
      data_type <- 'time'
      
      if (Mode(stringr::str_count(x, '\\-|\\+') == 1)){
        
        tz_sign <- '\u00B1'
        
      } else {
        
        tz_sign <- NULL
        
      }
      
    }

  }

  # Get format string specifier -----------------------------------------------

  nc <- stringr::str_count(x, pattern = ":")
  nt <- stringr::str_count(x, pattern = "T")
  nd <- stringr::str_count(x, pattern = "-")
  nd <- nd[!nd == 3]

  if ((length(unique(nc)) > 1) | (length(unique(nt)) > 1) | (length(unique(nd)) > 1)){
    warning('More than one date and time format was found. The returned value is the mode of the detected formats.')
  }

  nc <- Mode(nc)
  nt <- Mode(nt)
  nd <- Mode(nd)
  
  if (data_type == 'datetime'){
    
    if (nc == 2){
      
      output <- 'YYYY-MM-DDThh:mm:ss'
      
    } else if (nc == 1){
      
      output <- 'YYYY-MM-DDThh:mm'
      
    } else if (nc == 0){
      
      output <- 'YYYY-MM-DDThh'
      
    }
    
  } else if (data_type == 'date'){
    
    if (nd == 2){
      
      output <- 'YYYY-MM-DD'
      
    } else if (nd == 0){
      
      output <- 'YYYY'
      
    }
    
  } else if (data_type == 'time'){
    
    if (nc == 2){
      
      output <- 'hh:mm:ss'
      
    } else if (nc == 1){
      
      output <- 'hh:mm'
      
    } else if (nc == 0){
      
      output <- 'hh'
      
    }
    
  }

  # Add timezone offset -------------------------------------------------------
  
  if (!is.null(tz_sign)){
    
    output <- paste0(
      output,
      tz_sign,
      'hh'
    )
    
  }
  
  # Output --------------------------------------------------------------------

  output

}






# A function for checking format rules (i.e. datetime, date, or time only 
# vector).
#
# x = vector of ISO 8601 data supplied to iso8601_get_format_string() or
#     iso8601_read().
#
# Return NULL if one type of input data is present, otherwise issue an error.

validate_format_rules <- function(x){
  
  type_datetime <- FALSE
  
  type_date <- FALSE
  
  type_time <- FALSE
  
  x <- x[!is.na(x)]
  
  if (sum(stringr::str_count(x, 'T') > 0) > 0){
    
    type_datetime <- TRUE
    
    x[stringr::str_detect(x, 'T')] <- NA_character_
    
  }
  
  if (sum(stringr::str_count(x, '\\-') == 2, na.rm = T) > 0){
    
    type_date <- TRUE
    
    x[stringr::str_detect(x, '\\-')] <- NA_character_
    
  }
  
  if (sum(stringr::str_count(x, ':') > 0, na.rm = T) > 0){
    
    type_time <- TRUE
    
    x[stringr::str_detect(x, ':')] <- NA_character_
    
  }
  
  if (sum(nchar(x) == 4, na.rm = T) > 0){
    
    type_date <- TRUE
    
    x[nchar(x) == 4] <- NA_character_
    
  }
  
  if (sum(nchar(x) == 2, na.rm = T) > 0){
    
    type_time <- TRUE
    
    x[nchar(x) == 2] <- NA_character_
    
  } else if (sum(nchar(x) == 5, na.rm = T) > 0){
    
    type_time <- TRUE
    
    x[nchar(x) == 5] <- NA_character_
    
  }
  
  if (sum(c(type_datetime, type_date, type_time)) != 1){
    
    stop(
      paste0(
        'Input "x" must be only "datetime", "date", or "time" data.',
        ' More than one type is not supported.'
      )
    )
    
  }
  
  NULL
  
}
