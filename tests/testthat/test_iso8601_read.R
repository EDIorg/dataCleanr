context('Get format string for ISO 8601 datetime')

library(EDIutils)

# Load data -------------------------------------------------------------------

data <- read.table(
  paste0(path.package('EDIutils'), '/tests/test_data/datetimes.csv'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# Test possible formats -------------------------------------------------------

testthat::test_that('Test possible formats.', {

  expect_equal(
    get_datetime_format(
      x = '2012-05-01T13:45:23'
    ),
    'YYYY-MM-DDThh:mm:ss'
    )

  expect_equal(
    get_datetime_format(
      x = '2012-05-01T13:45'
    ),
    'YYYY-MM-DDThh:mm'
  )

  expect_equal(
    get_datetime_format(
      x = '2012-05-01T13'
    ),
    'YYYY-MM-DDThh'
  )

  expect_equal(
    get_datetime_format(
      x = '2012-05-01'
    ),
    'YYYY-MM-DD'
  )

})

