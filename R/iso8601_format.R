#' Get datetime format string from ISO 8601 datetimes
#'
#' @description
#'     Get format of ISO 8601 formatted character string.
#'
#' @usage iso8601_format(x)
#'
#' @param x
#'     (character) A vector of dates and times created with
#'     `iso8601_char`.
#'
#' @return
#'     (character) A datetime format string if only one is found, otherwise
#'     a vector of datetime format strings are returned.
#'
#' @examples 
#' # Get format string
#' datetime <- iso8601_char(x = '2012-05-01 13:29:54', orders = 'ymd_HMS')
#' iso8601_format(datetime)
#' datetime <- iso8601_char(x = '2012-05-01 13:29:54', orders = 'ymd_HMS', tz = '-3')
#' iso8601_format(datetime)
#' datetime <- iso8601_char(x = '2012-05-01 13:29:54', orders = 'ymd_HMS', tz = '+5')
#' iso8601_format(datetime)
#'
#' @export
#'

iso8601_format <- function(x){

  # Check arguments -----------------------------------------------------------

  if (missing(x)){
    stop('Input argument "x" is missing!')
  }
  if (sum(is.na(x)) == length(x)){
    stop('Input argument "x" cannot be entirely NA.')
  }
  x <- x[!is.na(x)]
  if (sum(stringr::str_detect(x, 'T')) > 0){
    x_sample <- x[stringr::str_detect(x, 'T')]
    x_sample <- stringr::str_remove_all(x_sample[1], pattern = '.+[T]')
    if ((sum(stringr::str_detect(x_sample, '\\+')) > 0)){
      tz_sign <- '+'
    } else if ((sum(stringr::str_detect(x_sample, '\\-')) > 0)){
      tz_sign <- '-'
    } else {
      tz_sign <- NULL
    }
  } else {
    tz_sign <- NULL
  }

  # Remove NA -----------------------------------------------------------------

  x <- x[!is.na(x)]

  # Parse datetime strings ----------------------------------------------------

  use_i <- stringr::str_count(x, pattern = ":")
  use_i_t <- stringr::str_count(x, pattern = "T")
  use_i_d <- stringr::str_count(x, pattern = "-")

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  use_i <- Mode(use_i)
  use_i_t <- Mode(use_i_t)
  use_i_d <- Mode(use_i_d)

  if (use_i == 2){
    output <- 'YYYY-MM-DDThh:mm:ss'
  }
  if (use_i == 1){
    output <- 'YYYY-MM-DDThh:mm'
  }
  if ((use_i == 0) & ((use_i_t == 1))){
    output <- 'YYYY-MM-DDThh'
  }
  if ((use_i == 0) & ((use_i_t != 1)) & ((use_i_d != 0))){
    output <- 'YYYY-MM-DD'
  }
  if ((use_i == 0) & ((use_i_t != 1)) & ((use_i_d == 0))){
    output <- 'YYYY'
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
