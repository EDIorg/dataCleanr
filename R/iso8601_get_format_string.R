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
#' @usage iso8601_get_format_string(x, return.format = FALSE)
#'
#' @param x
#'     (character) A vector of ISO 8601 strings created with `iso8601_convert`.
#'     Vector contents must be one of dates and times, dates, or times. A mix 
#'     of more than one type is not supported.
#' @param return.format
#'     (logical) Should format specifiers be returned with the output data? 
#'     This argument supports identification of where differences in output
#'     format occur.
#'
#' @return
#'     (character) The date time format representing user supplied ISO 8601 
#'     data. If more than one date time format is present, then the mode
#'     is returned along with a warning message. An error is issued when input 
#'     data is a mix of datetime, date, or time data.
#'     
#'     (data frame) If `return.format` is `TRUE` then a data frame is returned
#'     containing the input data and format specifiers. This argument supports
#'     identification of where differences in output format occur.
#'
#' @examples 
#' # Get format strings
#' iso8601_get_format_string('13:45')
#' iso8601_get_format_string('13:45:12+06:00')
#' iso8601_get_format_string('2012-05-01')
#' iso8601_get_format_string('2012-05-01T13')
#' iso8601_get_format_string('2012-05-01T13:29:54+05')
#' 
#' # Expect a warning when more than one format is present.
#' iso8601_get_format_string(c('2012-05-01T13:45', '2012-05-02T13', '2012-05-03T13:45:30'))
#' 
#' # return.format = T returns the input data, converted data, and formats
#' iso8601_get_format_string(c('2012-05-01T13:45', '2012-05-02T13', '2012-05-03T13:45:30'), return.format = T)
#' 
#' # Plus and minus sign is returned when more than one time zone sign is present
#' iso8601_get_format_string(x = c('2012-05-01T13:45:23+05:00', '2012-05-01T13:45:23+05:00', '2012-05-01T13:45:23-05:00'))
#' 
#' @export
#'

iso8601_get_format_string <- function(x, return.format = FALSE){
  
  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  
  if (sum(is.na(x)) == length(x)){
    stop('Input argument "x" cannot be entirely NA.')
  }

  # Load helper function ------------------------------------------------------
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Regular expressions for each possible format ------------------------------
  
  x_formats <- rep(NA_character_, length(x))
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*:[:digit:]*\\+[:digit:]*:[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm:ss+hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*:[:digit:]*\\-[:digit:]*:[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm:ss-hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*:[:digit:]*\\+[:digit:]*$'
      )] <- 'YYYY-MM-DDThh:mm:ss+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*:[:digit:]*\\-[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm:ss-hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*:[:digit:]*$'
      )] <- 'YYYY-MM-DDThh:mm:ss'
  
  # x_formats[
  #   stringr::str_detect(
  #     x, 
  #     pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*(\\+|\\-)[:digit:]*$'
  #     )] <- 'YYYY-MM-DDThh:mm\u00B1hh'

  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*\\+[:digit:]*:[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm+hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*\\-[:digit:]*:[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm-hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*\\+[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*\\-[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm-hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*:[:digit:]*$'
    )] <- 'YYYY-MM-DDThh:mm'
  
  # x_formats[
  #   stringr::str_detect(
  #     x, 
  #     pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*(\\+|\\-)[:digit:]*$'
  #     )] <- 'YYYY-MM-DDThh\u00B1hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*\\+[:digit:]*$'
    )] <- 'YYYY-MM-DDThh+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*\\-[:digit:]*$'
    )] <- 'YYYY-MM-DDThh-hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*T[:digit:]*$'
      )] <- 'YYYY-MM-DDThh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '[:digit:]*-[:digit:]*-[:digit:]*$'
      )] <- 'YYYY-MM-DD'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]{4}$'
      )] <- 'YYYY'
  
  # x_formats[
  #   stringr::str_detect(
  #     x, 
  #     pattern = '^[:digit:]*:[:digit:]*:[:digit:]*(\\+|\\-)[:digit:]*$'
  #     )] <- 'hh:mm:ss\u00B1hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*:[:digit:]*\\+[:digit:]*:[:digit:]*$'
    )] <- 'hh:mm:ss+hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*:[:digit:]*\\-[:digit:]*:[:digit:]*$'
    )] <- 'hh:mm:ss-hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*:[:digit:]*\\+[:digit:]*$'
    )] <- 'hh:mm:ss+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*:[:digit:]*\\-[:digit:]*$'
    )] <- 'hh:mm:ss-hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*:[:digit:]*$'
      )] <- 'hh:mm:ss'
  
  #
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*\\+[:digit:]*:[:digit:]*$'
    )] <- 'hh:mm+hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*\\-[:digit:]*:[:digit:]*$'
    )] <- 'hh:mm-hh:mm'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*\\+[:digit:]*$'
    )] <- 'hh:mm+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*\\-[:digit:]*$'
    )] <- 'hh:mm-hh'
  
  # x_formats[
  #   stringr::str_detect(
  #     x, 
  #     pattern = '^[:digit:]*:[:digit:]*(\\+|\\-)[:digit:]*$'
  #     )] <- 'hh:mm\u00B1hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*:[:digit:]*$'
      )] <- 'hh:mm'
  
  # x_formats[
  #   stringr::str_detect(
  #     x, 
  #     pattern = '^[:digit:]*(\\+|\\-)[:digit:]*$'
  #     )] <- 'hh\u00B1hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*\\+[:digit:]*$'
    )] <- 'hh+hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]*\\-[:digit:]*$'
    )] <- 'hh-hh'
  
  x_formats[
    stringr::str_detect(
      x, 
      pattern = '^[:digit:]{2}$'
      )] <- 'hh'

  # Add +- sign when both + and - time zone offsets are present ---------------
  
  if ((sum(stringr::str_detect(x_formats, pattern = '\\+hh$'), na.rm = T) > 0) &
      (sum(stringr::str_detect(x_formats, pattern = '\\-hh$'), na.rm = T) > 0)){
    
    x_formats <- stringr::str_replace_all(
      x_formats, 
      pattern = '(\\+hh|\\-hh)$',
      replacement = '\u00B1hh'
    )
    
  } else if ((sum(stringr::str_detect(x_formats, pattern = '\\+hh:mm$'), na.rm = T) > 0) &
             (sum(stringr::str_detect(x_formats, pattern = '\\-hh:mm$'), na.rm = T) > 0)){
    
    x_formats <- stringr::str_replace_all(
      x_formats, 
      pattern = '(\\+hh:mm|\\-hh:mm)$',
      replacement = '\u00B1hh:mm'
    )
    
  }
  
  # Return the mode of detected formats ---------------------------------------
  
  output <- Mode(x_formats)

  # Issue warning when more than one mode exists
  
  if (length(unique(x_formats)) > 1){
    warning('More than one format was found. The returned value is the mode of the detected formats. Use the argument "return.format = T" to see all detected fomats.')
  }

  # Output --------------------------------------------------------------------

  if (isTRUE(return.format)){
    
    data.frame(
      x = x,
      format = x_formats,
      stringsAsFactors = F
    )
    
  } else {
    
    output
    
  }

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
