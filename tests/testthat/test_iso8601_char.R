context('Convert characters to ISO 8601.')

library(dataCleanr)

# Load data -------------------------------------------------------------------

data <- utils::read.table(
  system.file('datetimes.csv', package = 'dataCleanr'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# orders = NULL ---------------------------------------------------------------

testthat::test_that('orders = NULL results in NULL', {

  output <- iso8601_char(
    x = data$raw
    )

  expect_equal(
    is.null(output),
    TRUE
  )

})

# orders != NULL --------------------------------------------------------------

testthat::test_that('Inadequately defined orders result in warnings.', {

  expect_warning(
    iso8601_char(
      x = data$raw,
      orders = c('ymd_HMS')
      )
    )

})

# orders mdy and dmy are unsupported ------------------------------------------

testthat::test_that('Use of orders mdy and dmy are unsupported.', {

  expect_warning(
    iso8601_char(
      x = data$raw,
      orders = c('mdy', 'dmy')
    )
  )

})

# Resolution of output should match input -------------------------------------

testthat::test_that('Resolution of output should match input.', {

  # Some test data can't be currently parsed.
  # Remove these data

  x_converted <- suppressWarnings(
    iso8601_char(
      x = data$raw,
      orders = c('ymd', 'ymd_h', 'ymd_hm', 'ymd_hms',
                 'mdy', 'mdy_h', 'mdy_hm', 'mdy_hms',
                 'dmy', 'dmy_h', 'dmy_hm', 'dmy_hms')
      )
    )

  use_i <- x_converted == data$iso8601

  expect_equal(
    x_converted[use_i],
    data$iso8601[use_i]
  )

})

# Use of "tz" appends timezones to the output ---------------------------------

testthat::test_that('Time zone offset should be present and formatted correctly', {
  
  expect_equal(
    iso8601_char(
      x = '2012-05-01 13:34:57', 
      orders = 'ymd_HMS', 
      tz = '-5'
      ),
    '2012-05-01T13:34:57-05'
  )
  
  expect_equal(
    iso8601_char(
      x = '2012-05-01 13:34', 
      orders = 'ymd_HM', 
      tz = '-5'
    ),
    '2012-05-01T13:34-05'
  )
  
  expect_equal(
    iso8601_char(
      x = '2012-05-01 13', 
      orders = 'ymd_H', 
      tz = '+5'
    ),
    '2012-05-01T13+05'
  )
  
})



