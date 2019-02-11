#' Convert dates and times to ISO 8601
#'
#' @description
#'    Convert date and time strings into standard ISO 8601 formatted strings,
#'    with output resolution matching inputs, and full support of timezone 
#'    offsets. 
#'    
#'    NOTE: This function does not convert to all ISO 8601 formats. Currently
#'    supported formats include calendar dates, times, time zones, and valid
#'    combinations of these. Week dates, ordinal dates, and time intervals are 
#'    not yet supported.
#'    
#' @details
#'    `iso8601_convert` leverages the power of `lubridate::parse_date_time` to 
#'    parse dates and times, then uses regular expressions on the user supplied
#'    `orders` argument to identify resolution of the input data, and then 
#'    outputs the converted data in this same resolution. Most of the arguments
#'    available to `lubridate::parse_date_time` can be used with 
#'    `iso8601_convert`.
#'
#' @usage iso8601_convert(x, orders, tz = NULL, truncated = 0, exact = FALSE, 
#'     train = TRUE, drop = FALSE, return.format = FALSE)
#'
#' @param x
#'     (character) A vector of date and time, date, or time strings.
#' @param orders
#'     (character) \emph{From lubridate 1.7.4.90000 documentation:}
#'     A vector of date-time formats. Each order string is a series 
#'     of formatting characters as listed in `base::strptime()` but might not 
#'     include the "%" prefix. For example, "ymd" will match all the possible
#'     dates in year, month, day order. Formatting orders might include 
#'     arbitrary separators. These are discarded. See details for implemented 
#'     formats.
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
#'     NOTE: This argument is different than `tz` supplied to 
#'     `lubridate::parse_date_time`.
#' @param truncated
#'     (integer) \emph{From lubridate 1.7.4.90000 documentation:}
#'     Number of formats that can be missing. The most common type of
#'     irregularity in date-time data is the truncation due to rounding or 
#'     unavailability of the time stamp. If the `truncated` parameter is 
#'     non-zero, then truncated formats are also checked. For example, if the
#'     format order is "ymdHMS" and `truncated = 3`, then incomplete date-times
#'     like `2012-06-01 12:23`, `2012-06-01 12`, and `2012-06-01` are parsed.
#'     \emph{The above definition was slightly modified from lubridate 
#'     1.7.4.90000 documentation.}
#' @param exact
#'     (logical) \emph{From lubridate 1.7.4.90000 documentation:}
#'     If `TRUE`, the `orders` parameter is interpreted as an exact 
#'     `base::strptime()` format and no training or guessing are performed 
#'     (i.e. `train`, `drop` parameters are irrelevant). \emph{The above 
#'     definition was copied directly from lubridate 1.7.4.90000 
#'     documentation.}
#' @param train
#'     (logical) \emph{From lubridate 1.7.4.90000 documentation:}
#'     Whether to train formats on a subset of the input vector. The
#'     resut of this is that supplied orders are sorted according to 
#'     performance on this training set, which commonly results in increased 
#'     performance. Please note that even when `train = FALSE` (and 
#'     `exact = FALSE`) guessing of the actual formats is still performed on a 
#'     pseudo-random subset of the original input vector. This might result in
#'     `All formats failed to parse` error. \emph{The above definition was 
#'     copied directly from lubridate 1.7.4.90000 documentation.}
#' @param drop
#'     (logical) \emph{From lubridate 1.7.4.90000 documentation:}
#'     Whether to drop formats that didn't match on the training set.
#'     If `FALSE`, unmatched on the training set formats are tried as a laast 
#'     resort at the end of the parsing queue. Applies only when 
#'     `train = TRUE`. Setting this parameter to `TRUE` might slightly speed 
#'     up parsing in situations involving many formats. Prior to v1.7.0 this 
#'     parameter was implicitly `TRUE`, which resulted in occasional surprising 
#'     behavior when rare patterns where not present in the training set.
#'     \emph{The above definition was copied directly from lubridate 
#'     1.7.4.90000 documentation.}
#' @param return.format
#'     (logical) Should format specifiers be returned with the output data? 
#'     This argument supports identification of where differences in output
#'     resolution/precision occur.
#'
#' @return
#'     (character) A vector of dates and times in the ISO 8601 standard in the 
#'     resolution of the input date and time strings. The ISO 8601 standard 
#'     format output by this function is a combination of calendar dates, 
#'     times, time zone offsets, and valid combinations of these.
#'     
#'     (data frame) If `return.format` is `TRUE` then a data frame is returned
#'     containing the input data, converted data, and formats of the converted 
#'     data. This supports identification of where differences in output 
#'     resolution/precision occur.
#'
#' @examples 
#'    # Convert dates and times of varying resolution
#'    iso8601_convert(x = '2012', orders = 'y')
#'    iso8601_convert(x = '01/05/2012', orders = 'dmy')
#'    iso8601_convert(x = '01-May-2012', orders = 'dby')
#'    iso8601_convert(x = '132954', orders = 'HMS')
#'    iso8601_convert(x = '132954', orders = 'HMS', tz = '-05')
#'    iso8601_convert(x = '20120501 132954', orders = 'Ymd HMS', tz = '-05')
#' 
#'    # Variance of input format is supported as long as orders are defined.
#'    # NOTE: Output resolution matches input resolution.
#'    iso8601_convert(x = c('2012-05-01 13:29:54', '2012-05-01 13:29', '1/5/2012 13'), orders = c('ymd_HMS', 'ymd_HM', 'dmy_H'))
#'    
#'    # Force output resolution to be the same
#'    iso8601_convert(x = c('2012-05-01 13:29:54', '2012-05-01 13:29', '1/5/2012 13'), orders = c('ymd_HMS', 'ymd_HMS', 'dmy_HMS'), truncated = 3)
#'
#' @export
#'

iso8601_convert <- function(x, orders, tz = NULL, truncated = 0, exact = FALSE, 
                            train = TRUE, drop = FALSE, return.format = FALSE){
  
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

  # Initialize output vector(s)
  
  x_converted <- rep(NA_character_, length(x))
  
  x_formats <- rep(NA_character_, length(x))
  
  x_raw <- x
  
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
    
    x_converted[!is.na(output)] <- format(output, '%H')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'H'
    
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
    
    x_converted[!is.na(output)] <- format(output, '%H:%M')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'HM'
    
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
    
    x_converted[!is.na(output)] <- format(output, '%H:%M:%S')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'HMS'
    
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
    
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%d')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'ymd'
    
    if (!is.null(tz)){
      stop("Adding time zones to date only data is not supported.")
    }
    
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
    
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'ymd H'
    
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
    
    x_converted[!is.na(output)] <- format(output, '%Y-%m-%dT%H:%M')[!is.na(output)]
    
    x[!is.na(output)] <- NA
    
    x_formats[!is.na(output)] <- 'ymd HM'
    
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
    
    if (stringr::str_detect(orders[use_i], '(OS|%OS)$')){
      
      decsec <- as.character(
        max(
          nchar(
            unlist(
              stringr::str_extract_all(
                x, 
                '\\.[:digit:]*$'
              )
            )
          )-1
        )
      )
      
      x_converted[!is.na(output)] <- format(
        output, 
        paste0(
          '%Y-%m-%dT%H:%M:%OS', 
          decsec
        )
      )[!is.na(output)]
      
      x_formats[!is.na(output)] <- 'ymd HMOS'
      
    } else {
      
      x_converted[!is.na(output)] <- format(
        output, 
        '%Y-%m-%dT%H:%M:%S'
      )[!is.na(output)]
      
      x_formats[!is.na(output)] <- 'ymd HMS'
      
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
    
    x_formats <- paste0(
      x_formats,
      'Z'
    )
    
  }
  
  # Output --------------------------------------------------------------------
  
  if (sum(is.na(x_converted)) > 0){
    warning(
      paste0(
        'Some data failed to parse. Consider updating your list of orders.'
      )
    )
  }
  
  if (length(unique(x_formats)) > 1){
    warning(
      paste0(
        'Output data contains multiple levels of precision.',
        ' Use the argument "return.res = T" to see where.'
      )
    )
  }
  
  if (isTRUE(return.format)){
    
    x_converted <- data.frame(
      x = x_raw,
      x_converted = x_converted,
      format = x_formats,
      stringsAsFactors = F
    )
    
    x_converted
    
  } else {
    
    x_converted
    
  }
  
}
