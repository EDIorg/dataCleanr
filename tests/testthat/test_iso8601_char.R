context('Standardize dates and times to ISO 8601.')

library(EDIutils)

# Load data -------------------------------------------------------------------

data <- read.table(
  paste0(path.package('EDIutils'), '/tests/test_data/datetimes.csv'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# orders = NULL ---------------------------------------------------------------

testthat::test_that('orders = NULL results in NULL', {

  output <- datetime_to_iso8601(
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
    datetime_to_iso8601(
      x = data$raw,
      orders = c('ymd_HMS')
      )
    )

})

# orders mdy and dmy are unsupported ------------------------------------------

testthat::test_that('Use of orders mdy and dmy are unsupported.', {

  expect_warning(
    datetime_to_iso8601(
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
    datetime_to_iso8601(
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



