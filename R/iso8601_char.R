#' Convert datetime string to ISO 8601 standard
#'
#' @description
#'     Convert a vector of dates and times to the standard ISO 8601 format
#'     YYYY-MM-DDThh:mm:ss. NOTE: Time zones are not yet supported.
#'
#' @usage iso8601_char(x, orders = NULL, tz = NULL)
#'
#' @param x
#'     (character) A vector of dates and times.
#' @param orders
#'     (character) Format of dates and times listed in x. Valid options are:
#'     \itemize{
#'         \item{ymd}
#'         \item{ymd_H}
#'         \item{ymd_HM}
#'         \item{ymd_HMS}
#'         \item{ymd}
#'         \item{mdy_H}
#'         \item{mdy_HM}
#'         \item{mdy_HMS}
#'         \item{dmy}
#'         \item{dmy_H}
#'         \item{dmy_HM}
#'         \item{dmy_HMS}
#'     }
#'     Where:
#'     \itemize{
#'         \item{y} is year
#'         \item{m} is month
#'         \item{d} is day
#'         \item{H} is hour in 24 hour clock
#'         \item{M} is minute
#'         \item{s} is second
#'     }
#' @param tz
#'     (character) Time zone offset with respect to UTC (e.g. '+5', '-11')
#'
#' @return
#'     A vector of dates and times in the ISO 8601 standard to the precision of
#'     the input datetime value.
#'
#' @export
#'

iso8601_char <- function(x, orders = NULL, tz = NULL){

  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  if (!is.character(x)){
    stop('Input argument "x" is not of class "character"!')
  }
  if (!is.null(orders)){
    if (!is.character(orders)){
      stop('Input argument "orders" is not of class "character"!')
    }
    if (('mdy' %in% orders) & ('dmy' %in% orders)){
      warning('There is a known issue in the ability of this function to parse orders "mdy" and "dmy" when used in the same vector of dates. Please revise your data to contain only one or the other formats, then select which order matches with your data.')
    }
  }
  if (!is.null(tz)){
    if (!is.character(tz)){
      stop('Input argument "tz" is not of class "character"!')
    }
    if (!isTRUE(stringr::str_detect(tz, '\\+')) & !isTRUE(stringr::str_detect(tz, '\\-'))){
      stop('Missing "+" or "-" from input argument "tz".')
    }
  }
  
  # If orders are defined -----------------------------------------------------

  if (!is.null(orders)){

    orders <- tolower(orders)

    # Initialize output vector

    x_converted <- rep(NA_character_, length(x))

    # apply list

    # Resolution = date -------------------------------------------------------

    # Format = ymd

    if ('ymd' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'ymd'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%d')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = mdy

    if ('mdy' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'mdy'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%d')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = dmy

    if ('dmy' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'dmy'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%d')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Resolution = date H -----------------------------------------------------

    # Format = ymd_H

    if ('ymd_h' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'ymd_H'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = mdy_H

    if ('mdy_h' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'mdy_H'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = dmy_H

    if ('dmy_h' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'dmy_H'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Resolution = date HM ----------------------------------------------------

    # Format = ymd_HM

    if ('ymd_hm' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'ymd_HM'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = mdy_HM

    if ('mdy_hm' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'mdy_HM'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = dmy_HM

    if ('dmy_hm' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'dmy_HM'
      ))
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Resolution = date HMS ----------------------------------------------------

    # Format = ymd_HMS

    if ('ymd_hms' %in% orders){
      
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'ymd_HMS'
      ))
      use_i <- stringr::str_detect(
        string = x,
        pattern = ':[:digit:]+:|\\s[:digit:]{6}'
      )
      output[!use_i] <- NA
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M:%S')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = mdy_HMS

    if ('mdy_hms' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'mdy_HMS'
      ))
      use_i <- stringr::str_detect(
        string = x,
        pattern = ':[:digit:]+:|\\s[:digit:]{6}'
      )
      output[!use_i] <- NA
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M:%S')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Format = dmy_HMS

    if ('dmy_hms' %in% orders){
      output <- suppressWarnings(lubridate::parse_date_time(
        x = x,
        orders = 'dmy_HMS'
      ))
      use_i <- stringr::str_detect(
        string = x,
        pattern = ':[:digit:]+:|\\s[:digit:]{6}'
      )
      output[!use_i] <- NA
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M:%S')[!is.na(output)]
      x[!is.na(output)] <- NA
    }

    # Send warning if parsing is incomplete -----------------------------------

    if (sum(is.na(x_converted)) > 0){
      warning(
        paste0(
          'Date-times failed to parse at lines:\n',
          paste(seq(length(is.na(x_converted)))[is.na(x_converted)], collapse = ', '),
          '\nYou may need to update the list of formats supplied to the "orders" argument.'
        )
      )

    }
    
    # Add timezone offset -----------------------------------------------------
    
    if (!is.null(tz)){
      hr <- sprintf('%02d', abs(as.numeric(tz)))
      if ((as.numeric(tz) > 0)){
        use_i <- is.na(x_converted)
        x_converted <- paste0(
          x_converted, 
          paste0('+', hr)
          )
        x_converted[use_i] <- NA_character_
      } else if ((as.numeric(tz) < 0)){
        use_i <- is.na(x_converted)
        x_converted <- paste0(
          x_converted, 
          paste0('-', hr)
        )
        x_converted[use_i] <- NA_character_
      }
      
    }

  }

  # If orders are not defined -------------------------------------------------

  if (is.null(orders)){

    x_converted <- NULL

  }

  # Output --------------------------------------------------------------------

  x_converted

}
