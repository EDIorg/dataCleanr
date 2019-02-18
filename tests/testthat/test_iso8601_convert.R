context('ISO 8601 convert')
library(dataCleanr)
library(stringr)

# Load data -------------------------------------------------------------------

data <- utils::read.table(
  system.file('datetimes.csv', package = 'dataCleanr'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA"
)

data$index <- seq(nrow(data))

# Test argument rules ---------------------------------------------------------

testthat::test_that('Expect errors', {
  
  expect_error(
    iso8601_convert()
  )
  
  expect_error(
    iso8601_convert(
      x = data$raw,
      orders = c(2, 3, 4)
    )
  )
  
  expect_error(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd',
      tz = 5
    )
  )
  
  expect_error(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd',
      tz = '5'
    )
  )
  
})

testthat::test_that('Expect warnings', {
  
  expect_warning(
    iso8601_convert(
      x = c(2012, 2013),
      orders = 'ymd'
    )
  )
  
  # Inadequately defined orders result in warnings
  
  expect_warning(
    iso8601_convert(
      x = data$raw,
      orders = c('ymd_HMS')
    )
  )
  
})

# Resolution of output should match input -------------------------------------

testthat::test_that('Resolution of output should match input.', {

  use_i <- stringr::str_detect(data$orders, 'ymd_HM|ymd_HMS')
  
  expect_equal(
    suppressWarnings(
      iso8601_convert(
        x = data$raw[use_i],
        orders = c('ymd_HM', 'ymd_HMS')
      )
    ),
    data$iso8601[use_i]
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_convert(
        x = c(
          '13:45:14',
          '13:45',
          '13'
        ),
        orders = c('HMS', 'HM', 'H')
      )
    ),
    c(
      '13:45:14',
      '13:45',
      '13'
    )
  )

})

# Use of "tz" appends timezones to the output ---------------------------------
# Time zones shouldn't be appended to calendar dates without times.

testthat::test_that('Time zone offset should be present and formatted correctly', {
  
  expect_equal(
    iso8601_convert(
      x = '2012-05-01 13:34:57', 
      orders = 'ymd_HMS', 
      tz = '-5'
      ),
    '2012-05-01T13:34:57-05'
  )
  
  expect_equal(
    iso8601_convert(
      x = '2012-05-01 13:34', 
      orders = 'ymd_HM', 
      tz = '-5'
    ),
    '2012-05-01T13:34-05'
  )
  
  expect_equal(
    iso8601_convert(
      x = '2012-05-01 13', 
      orders = 'ymd_H', 
      tz = '+5'
    ),
    '2012-05-01T13+05'
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_convert(
        x = c(
          '13:45:14',
          '13:45',
          '13'
        ), 
        orders = c(
          'HMS',
          'HM',
          'H'
        ), 
        tz = '+5'
      )
    ),
    c(
      '13:45:14+05',
      '13:45+05',
      '13+05'
    )
  )
  
  expect_error(
    iso8601_convert(
      x = c(
        '2012-05-01',
        '2012-05-01 13:34',
        '2012-05-01 13:34',
        '2012-05-01 13:34'
      ),
      orders = c('ymd', 'ymd HM'),
      tz = '+5'
    )
  )
  
  expect_error(
    iso8601_convert(
      x = c(
        '2012',
        '2012-05-01 13:34',
        '2012-05-01 13:34',
        '2012-05-01 13:34'
      ),
      orders = c('y', 'ymd', 'ymd HM'),
      tz = '+5'
    )
  )
  
  expect_equal(
    iso8601_convert(
      x = c(
        '2012-05-01 13:34',
        '2012-05-01 13:34',
        '2012-05-01 13:34'
      ),
      orders = 'ymd HM',
      tz = 'Z'
    ),
    c(
      '2012-05-01T13:34Z',
      '2012-05-01T13:34Z',
      '2012-05-01T13:34Z'
    )
  )
  
  expect_equal(
    iso8601_convert(
      x = c(
        '2012-05-01 13:34',
        '2012-05-01 13:34',
        '2012-05-01 13:34'
      ),
      orders = 'ymd HM',
      tz = '+10:00'
    ),
    c(
      '2012-05-01T13:34+10:00',
      '2012-05-01T13:34+10:00',
      '2012-05-01T13:34+10:00'
    )
  )
  
  
  
})

# ymd -------------------------------------------------------------------------

testthat::test_that('orders = ymd', {
  
  # orders = 'ymd'
  
  use_i <- stringr::str_detect(data$orders, 'ymd$')
  
  expect_equal(
    iso8601_convert(
      x = data$raw[use_i],
      orders = c('ymd')
    ),
    data$iso8601[use_i]
  )
  
  # orders = '%Y-%m-%d' matches orders 'ymd'
  
  use_i <- stringr::str_detect(data$orders, 'ymd$')
  
  expect_equal(
    iso8601_convert(
      x = data$raw[use_i],
      orders = '%Y-%m-%d'
    ),
    data$iso8601[use_i]
  )
  
  # orders = '%Y-%m-%d', exact = T matches exactly
  
  use_i <- stringr::str_detect(data$orders, 'ymd$')
  
  expect_equal(
    suppressWarnings(
      iso8601_convert(
        x = data$raw[use_i],
        orders = '%Y-%m-%d',
        exact = T
      )
    ),
    c(
      '2012-05-01',
      NA_character_,
      NA_character_
    )
  )

})

# mdy -------------------------------------------------------------------------

testthat::test_that('orders = mdy', {
  
  use_i <- stringr::str_detect(data$orders, 'mdy$')
  
  expect_equal(
    iso8601_convert(
      x = data$raw[use_i],
      orders = 'mdy'
    ),
    data$iso8601[use_i]
  )
  
})

# dmy -------------------------------------------------------------------------

testthat::test_that('orders = dmy', {
  
  # orders = 'dmy' can't parse 2 digit year
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'dmy'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy']
    )
  )
  
  # orders = '%d-%m-%Y', exact = T matches exactly
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = '%d-%m-%y',
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$raw == '1-5-12']
  )
  
})

# ymd_H -------------------------------------------------------------------------

testthat::test_that('orders = ymd_H', {
  
  # orders = 'ymd_H' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd_H'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'ymd_H']
    )
  )
  
  # Refined order definitions produce better matches
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('%Y-%m-%d %H', '%Y/%m/%d %H', '%Y%m%d %H'),
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$orders == 'ymd_H']
  )
  
})

# mdy_H -------------------------------------------------------------------------

testthat::test_that('orders = mdy_H', {
  
  # orders = 'mdy_H' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'mdy_H'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'mdy_H']
    )
  )
  
  # Refined order definitions produce better matches
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = '%m/%d/%Y %H',
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$orders == 'mdy_H']
  )
  
})

# dmy_H -------------------------------------------------------------------------

testthat::test_that('orders = dmy_H', {
  
  # orders = 'dmy_H' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'dmy_H'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy_H']
    )
  )
  
  # Refined order definitions produce better matches
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = '%d/%m/%y %H',
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$orders == 'dmy_H']
  )
  
})

# ymd_HM -------------------------------------------------------------------------

testthat::test_that('orders = ymd_HM', {
  
  # orders = 'ymd_HM' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd_HM'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'ymd_HM']
    )
  )
  
  # Refined order definitions produce better matches
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('%Y-%m-%d %H:%M', '%Y/%m/%d %H:%M', '%Y%m%d %H%M'),
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$orders == 'ymd_HM']
  )
  
})

# mdy_HM -------------------------------------------------------------------------

testthat::test_that('orders = mdy_HM', {
  
  # orders = 'mdy_HM' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'mdy_HM'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'mdy_HM']
    )
  )
  
  # Refined order definitions don't produce better matches when month and day
  # are < 12
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('%m-%d-%Y %H:%M', '%m/%d/%Y %H:%M'),
      exact = T
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'mdy_HM']
    )
  )
  
})

# dmy_HM -------------------------------------------------------------------------

testthat::test_that('orders = dmy_HM', {
  
  # orders = 'dmy_H' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'dmy_HM'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy_HM']
    )
  )
  
  # Refined order definitions typically produces better matches, but ambiguity
  # arises when day and month are < 12.
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = '%d/%m/%Y %H:%M',
      exact = T
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy_HM']
    )
  )
  
})

# ymd_HMS -------------------------------------------------------------------------

testthat::test_that('orders = ymd_HMS', {
  
  # orders = 'ymd_HMS' correctly parses when other orders are not needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd_HMS'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'ymd_HMS']
    ) 
  )
  
  # Refined order definitions correctly identify
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('%Y-%m-%d %H:%M:%S', '%Y/%m/%d %H:%M:%S', '%Y%m%d %H%M%S'),
      exact = T
    )
  )
  
  expect_equal(
    data$index[!is.na(output)],
    data$index[data$orders == 'ymd_HMS']
  )
  
})

# mdy_HMS -------------------------------------------------------------------------

testthat::test_that('orders = mdy_HMS', {
  
  # orders = 'mdy_HMS' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'mdy_HMS'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'mdy_HMS']
    )
  )
  
  # Refined order definitions doesn't help when month and day are < 12
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('%m-%d-%Y %H:%M:%S', '%m/%d/%Y %H:%M:%S', '%m%d%Y %H:%M:%S'),
      exact = T
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'mdy_HMS']
    )
  )
  
})

# dmy_HMS -------------------------------------------------------------------------

testthat::test_that('orders = dmy_HMS', {
  
  # orders = 'dmy_HMS' incorrectly parses when other orders are needed
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'dmy_HMS'
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy_HMS']
    )
  )
  
  # Refined order definitions typically produces better matches, but ambiguity
  # arises when day and month are < 12.
  
  output <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c(
        '%d-%m-%Y %H:%M:%S',
        '%d-%m-%y %H:%M:%S',
        '%d-%m-%y %H:%M:%S',
        '%d/%m/%Y %H:%M:%S',
        '%d/%m/%y %H:%M:%S',
        '%d/%m/%y %H:%M:%S',
        '%d%m%Y %H:%M:%S'
      ),
      exact = T
    )
  )
  
  expect_error(
    expect_equal(
      data$index[!is.na(output)],
      data$index[data$orders == 'dmy_HMS']
    )
  )
  
})

# date HMS --------------------------------------------------------------------

testthat::test_that('date HMS', {
  
  # ymd_HMS
  use_i <- data$orders == 'ymd_HMS'
  x_standard <- rep(
    NA, 
    length(use_i)
  )
  x_standard[use_i] <- data$iso8601[use_i]
  x_converted <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = 'ymd_HMS'
    )
  )
  expect_error(
    expect_equal(
      x_standard, 
      x_converted
    )
  )
  
  # *_HMS does not parse datetimes with lower time resolution
  use_i <- data$orders %in% c('ymd_HMS', 'mdy_HMS', 'dmy_HMS')
  x_standard <- rep(
    NA, 
    length(use_i)
  )
  x_standard[use_i] <- data$iso8601[use_i]
  x_converted <- suppressWarnings(
    iso8601_convert(
      x = data$raw,
      orders = c('ymd_HMS', 'mdy_HMS', 'dmy_HMS')
    )
  )
  expect_error(
    expect_equal(
      is.na(x_converted), 
      is.na(x_standard)
    )
  )
  
})

# ybd ----------------------------------------------

testthat::test_that('Use abbreviated or full month name', {
  
  expect_equal(
    iso8601_convert(
      x = '15 January 2012',
      orders = 'dby'
    ),
    '2012-01-15'
  )
  
  expect_equal(
    iso8601_convert(
      x = '15-Jan-2012',
      orders = 'dby'
    ),
    '2012-01-15'
  )
  
  expect_equal(
    iso8601_convert(
      x = '15-Jan-2012 13',
      orders = 'dby H'
    ),
    '2012-01-15T13'
  )
  
  expect_equal(
    iso8601_convert(
      x = '15-Jan-2012 13:59',
      orders = 'dby HM'
    ),
    '2012-01-15T13:59'
  )
  
  expect_equal(
    iso8601_convert(
      x = '15-Jan-2012 13:59:30',
      orders = 'dby HMS'
    ),
    '2012-01-15T13:59:30'
  )

})

# date HMOS -------------------------------------------------------------

testthat::test_that('Use decimal seconds', {

  output <- iso8601_convert(
    x = '15-Jan-2012 13:59:30.45',
    orders = 'dby HMOS'
  )
  
  expect_equal(
    output,
    '2012-01-15T13:59:30.45'
  )
  
})

# H, HM, HMS, -----------------------------------------------------------------

testthat::test_that('Time only data', {
  
  expect_equal(
    iso8601_convert(
      x = '5',
      orders = 'H'
    ),
    '05'
  )
  
  expect_equal(
    iso8601_convert(
      x = '1345',
      orders = 'HM'
    ),
    '13:45'
  )
  
  expect_equal(
    iso8601_convert(
      x = '134510',
      orders = 'HMS'
    ),
    '13:45:10'
  )
  
})

# return.format ---------------------------------------------------------------

testthat::test_that('return.format = T outputs a data frame', {
  
  expect_equal(
    class(
      iso8601_convert(
        x = data_iso8601$date_2_formats,
        orders = c('mdy', 'dmy'),
        return.format = T
      )
    ), 
    'data.frame'
  )
  
  expect_equal(
    class(
      iso8601_convert(
        x = data_iso8601$date_2_formats,
        orders = c('mdy', 'dmy'),
        return.format = T
      )
    ), 
    'data.frame'
  )

})


# Warn when output resolution varies ----------------------------------------------------

testthat::test_that('return.format = T outputs a data frame', {
  
  use_i <- stringr::str_detect(data$orders, 'ymd_HM|ymd_HMS')
  
  expect_warning(
    iso8601_convert(
      x = data$raw[use_i],
      orders = c('ymd_HM', 'ymd_HMS')
    )
  )
  
  expect_warning(
    iso8601_convert(
      x = c(
        '13:45:14',
        '13:45',
        '13'
      ),
      orders = c('HMS', 'HM', 'H')
    )
  )

})

# Validate time zone offset ---------------------------------------------------

testthat::test_that('Validate tz', {
  
  expect_equal(
    validate_tz('Z'),
    'Z'
  )
  
  expect_equal(
    validate_tz('-5'),
    '-05'
  )
  
  expect_equal(
    validate_tz('-05'),
    '-05'
  )
  
  expect_equal(
    validate_tz('-05:00'),
    '-05:00'
  )
  
  expect_error(
    suppressWarnings(
      validate_tz('-05:55')
    )
  )
  
  expect_error(
    validate_tz('-05:55')
  )
  
  expect_equal(
    validate_tz('+10'),
    '+10'
  )
  
  expect_equal(
    validate_tz('+10:00'),
    '+10:00'
  )
  
  expect_error(
    validate_tz('+19:00')
  )
  
  expect_error(
    validate_tz('06:00')
  )

})


# Invalid time zones are not used ---------------------------------------------

testthat::test_that('Invalid time zones result in error', {
  
  expect_error(
    iso8601_convert(
      x = '2012-05-01 13:45',
      orders = 'ymd HM',
      tz = '+18'
    )
  )
  
})


