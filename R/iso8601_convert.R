#' Convert dates and times to ISO 8601
#'
#' @description
#'    Convert date and time character strings to the standard ISO 8601 format,
#'    with output precision matching inputs, and full support of timezone 
#'    offsets. NOTE: This function does not convert to all ISO 8601 formats.
#'    Currently supported formats include calendar dates, times, and time zone
#'    offsets. Week dates, ordinal dates, and time intervals are not yet 
#'    supported.
#'    
#'    This function is a wrapper to `lubridate::parse_datetime` and supports
#'    a subset of the associated arguments.
#'
#' @usage iso8601_convert(x, orders, tz = NULL, truncated = 0, exact = FALSE, 
#'     train = TRUE, drop = FALSE)
#'
#' @param x
#'     (character) A vector of dates and times.
#' @param orders
#'     (character) A vector of date-time formats. Each order string is a series 
#'     of formatting characters as listed in `base::strptime()` but might not 
#'     include the "%" prefix. For example, "ymd" will match all the possible
#'     dates in year, month, day order. Formatting orders might include 
#'     arbitrary separators. These are discarded. See details for implemented 
#'     formats. \emph{The above definition was copied directly from lubridate
#'     1.7.4.90000 documentation.}.
#'     
#'     The list below contains recognized formats.
#'    \itemize{
#'         \item{y} Year without centry (00--99 or 0--99). Also matches with
#'         centry format (Y).
#'         \item{Y} Year with century.
#'         \item{m} Month as decimal number (01--12 or 1--12).
#'         \item{b} Abbreviated or full month name in the current locale. The 
#'         C parser currently understands only English month names.
#'         \item{d} Day of the month as decimal number (01--31 or 1--31).
#'         \item{H} Hours as decimal number (00--24 or 0--24).
#'         \item{M} Minute as decimal number (00--59 or 0--59)
#'         \item{S} Second as decimal number (00--61 or 0--61), allowing for
#'         up to two leap-seconds.
#'         \item{OS} Fractional second.
#'     }
#' @param tz
#'     (character) Time zone offset with respect to UTC (e.g. '+5', '-11'). 
#'     NOTE: Time zone names abbreviations (e.g. 'UTC') are not supported.
#' @param truncated
#'     (integer) Number of formats that can be missing. The most common type of
#'     irregularity in date-time data is the truncation due to rounding or 
#'     unavailability of the time stamp. If the `truncated` parameter is 
#'     non-zero, then truncated formats are also checked. For example, if the
#'     format order is "ymdHMS" and `truncated = 3`, then incomplete date-times
#'     like `2012-06-01 12:23`, `2012-06-01 12`, and `2012-06-01` are parsed.
#'     \emph{The above definition was slightly modified from lubridate 
#'     1.7.4.90000 documentation.}
#' @param exact
#'     (logical) If `TRUE`, the `orders` parameter is interpreted as an exact 
#'     `base::strptime()` format and no training or guessing are performed 
#'     (i.e. `train`, `drop` parameters are irrelevant). \emph{The above 
#'     definition was copied directly from lubridate 1.7.4.90000 
#'     documentation.}
#' @param train
#'     (logical) Whether to train formats on a subset of the input vector. The
#'     resut of this is that supplied orders are sorted according to 
#'     performance on this training set, which commonly results in increased 
#'     performance. Please note that even when `train = FALSE` (and 
#'     `exact = FALSE`) guessing of the actual formats is still performed on a 
#'     pseudo-random subset of the original input vector. This might result in
#'     `All formats failed to parse` error. \emph{The above definition was 
#'     copied directly from lubridate 1.7.4.90000 documentation.}
#' @param drop
#'     (logical) Whether to drop formats that didn't match on the training set.
#'     If `FALSE`, unmatched on the training set formats are tried as a laast 
#'     resort at the end of the parsing queue. Applies only when 
#'     `train = TRUE`. Setting this parameter to `TRUE` might slightly speed 
#'     up parsing in situations involving many formats. Prior to v1.7.0 this 
#'     parameter was implicitly `TRUE`, which resulted in occasional surprising 
#'     behavior when rare patterns where not present in the training set.
#'     \emph{The above definition was copied directly from lubridate 
#'     1.7.4.90000 documentation.}
#'
#' @return
#'     (character) A vector of dates and times in the ISO 8601 standard to the 
#'     precision of the input date and time value. Convert to POSIXct and 
#'     POSIXlt by passing outputs to `dataCleanr::iso8601_read`.
#'
#' @examples 
#'    # Convert dates and times of varying formats
#'    iso8601_convert(x = '2012-05-01 13:29:54', orders = 'ymd HMS')
#'    iso8601_convert(x = '05/01/2012 13:29', orders = 'mdy HM')
#'    iso8601_convert(x = '20120501 13', orders = '%Y%m%d %H')
#' 
#'    # Convert dates and times and include a time zone offset
#'    iso8601_convert(x = '2012-05-01 13:29:54', orders = 'ymd_HMS', tz = '-3')
#'    iso8601_convert(x = '2012-05-01 13', orders = 'ymd_H', tz = '+5')
#' 
#'    # Variance in input format is supported as long as orders are defined
#'    iso8601_convert(x = c('2012-05-01 13:29:54', '2012-05-01 13:29', '1/5/2012 13'), orders = c('ymd_HMS', 'ymd_HM', 'dmy_H'))
#'
#' @export
#'

iso8601_convert <- function(x, orders, tz = NULL, truncated = 0, exact = FALSE, 
                            train = TRUE, drop = FALSE){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  if (!is.character(x)){
    warning('Input argument "x" is not of class "character"! Coercing to character.')
    x <- as.character(x)
  }
  if (missing(orders)){
    stop('Intput argument "orders" is missing.')
  } else {
    if (!is.character(orders)){
      stop('Input argument "orders" is not of class "character"!')
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

  # Initialize output vector
  
  x_converted <- rep(NA_character_, length(x))
  
  # Resolution = H ------------------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    paste0(
      '^(H|%H)$'
    )
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%H')[!is.na(output)]
    x[!is.na(output)] <- NA
  }
  
  # Resolution = HM -----------------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    '^(H|%H)([:space:]|[:punct:])*(M|%M)$'
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%H:%M')[!is.na(output)]
    x[!is.na(output)] <- NA
  }
  
  # Resolution = HMS ----------------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    '^(H|%H)([:space:]|[:punct:])*(M|%M)([:space:]|[:punct:])*(S|%S)$'
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%H:%M:%S')[!is.na(output)]
    x[!is.na(output)] <- NA
  }
  
  # Resolution = date ---------------------------------------------------------

  use_i <- stringr::str_detect(
    orders, 
    paste0(
      '((%Y|%y)[:punct:]*(%m|%b)[:punct:]*%d$)',
      '|',
      '((Y|y)([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*d$)',
      '|',
      '((%m|%b)[:punct:]*%d[:punct:]*(%Y|%y)$)',
      '|',
      '(m([:space:]|[:punct:])*d([:space:]|[:punct:])*(Y|y)$)',
      '|',
      '(%d[:punct:]*(%m|%b)[:punct:]*(%Y|%y)$)',
      '|',
      '(d([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*(Y|y)$)'
    )
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%d')[!is.na(output)]
    x[!is.na(output)] <- NA
  }
  
  # Resolution = date H -------------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    paste0(
      '((%Y|%y)[:punct:]*(%m|%b)[:punct:]*%d[:blank:]%H$)',
      '|',
      '((Y|y)([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*d([:space:]|[:punct:])*H$)',
      '|',
      '((%m|%b)[:punct:]*%d[:punct:]*(%Y|%y)[:blank:]%H$)',
      '|',
      '(m([:space:]|[:punct:])*d([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H$)',
      '|',
      '(%d[:punct:]*(%m|%b)[:punct:]*(%Y|%y)[:blank:]%H$)',
      '|',
      '(d([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H$)'
    )
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H')[!is.na(output)]
    x[!is.na(output)] <- NA
  }

  # Resolution = date HM ------------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    paste0(
      '((%Y|%y)[:punct:]*(%m|%b)[:punct:]*%d[:blank:]%H[:punct:]*%M$)',
      '|',
      '((Y|y)([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*d([:space:]|[:punct:])*H([:space:]|[:punct:])*M$)',
      '|',
      '((%m|%b)[:punct:]*%d[:punct:]*(%Y|%y)[:blank:]%H[:punct:]*%M$)',
      '|',
      '(m([:space:]|[:punct:])*d([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H([:space:]|[:punct:])*M$)',
      '|',
      '(%d[:punct:]*(%m|%b)[:punct:]*(%Y|%y)[:blank:]%H[:punct:]*%M$)',
      '|',
      '(d([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H([:space:]|[:punct:])*M$)'
    )
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M')[!is.na(output)]
    x[!is.na(output)] <- NA
  }
  
  # Resolution = date HMS -----------------------------------------------------
  
  use_i <- stringr::str_detect(
    orders, 
    paste0(
      '((%Y|%y)[:punct:]*(%m|%b)[:punct:]*%d[:blank:]%H[:punct:]*%M[:punct:]*(%S|%OS)$)',
      '|',
      '((Y|y)([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*d([:space:]|[:punct:])*H([:space:]|[:punct:])*M([:space:]|[:punct:])*(S|OS)$)',
      '|',
      '((%m|%b)[:punct:]*%d[:punct:]*(%Y|%y)[:blank:]%H[:punct:]*%M[:punct:]*(%S|%OS)$)',
      '|',
      '(m([:space:]|[:punct:])*d([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H([:space:]|[:punct:])*M([:space:]|[:punct:])*(S|OS)$)',
      '|',
      '(%d[:punct:]*(%m|%b)[:punct:]*(%Y|%y)[:blank:]%H[:punct:]*%M[:punct:]*(%S|%OS)$)',
      '|',
      '(d([:space:]|[:punct:])*(m|b)([:space:]|[:punct:])*(Y|y)([:space:]|[:punct:])*H([:space:]|[:punct:])*M([:space:]|[:punct:])*(S|OS)$)'
    )
  )
  
  if (sum(use_i) > 0){
    output <- suppressWarnings(
      lubridate::parse_date_time(
        x = x,
        orders = orders[use_i],
        truncated = truncated, 
        exact = exact, 
        train = train, 
        drop = drop
      )
    )
    output[!use_i] <- NA
    if (stringr::str_detect(orders[use_i], '(OS|%OS)$')){
      decsec <- as.character(max(nchar(unlist(stringr::str_extract_all(x, '\\.[:digit:]*$')))-1))
      x_converted[!is.na(output)] <- format(output, paste0('%Y-%m-%dT%H:%M:%OS', decsec))[!is.na(output)]
    } else {
      x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M:%S')[!is.na(output)]
    }
    x[!is.na(output)] <- NA
  }
  
  # Add timezone offset -------------------------------------------------------
  
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
  
  # Output --------------------------------------------------------------------
  
 
  if (sum(is.na(x_converted)) > 0){
    warning(
      paste0(
        'Some data failed to parse. Consider updating your list of orders.'
      )
    )
  }
   
  x_converted
  
}
