context('Get format string for ISO 8601 datetime')

library(dataCleanr)

# Load data -------------------------------------------------------------------

data <- utils::read.table(
  system.file('datetimes.csv', package = 'dataCleanr'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# Test possible formats -------------------------------------------------------

testthat::test_that('Test possible formats.', {

  expect_equal(
    iso8601_format(
      x = '2012-05-01T13:45:23'
    ),
    'YYYY-MM-DDThh:mm:ss'
    )

  expect_equal(
    iso8601_format(
      x = '2012-05-01T13:45'
    ),
    'YYYY-MM-DDThh:mm'
  )

  expect_equal(
    iso8601_format(
      x = '2012-05-01T13'
    ),
    'YYYY-MM-DDThh'
  )

  expect_equal(
    iso8601_format(
      x = '2012-05-01'
    ),
    'YYYY-MM-DD'
  )
  
  # Add time zones ------------------------------------------------------------
  
  expect_equal(
    iso8601_format(
      x = '2012-05-01T13:45:23+05'
    ),
    'YYYY-MM-DDThh:mm:ss+hh'
  )
  
  expect_equal(
    iso8601_format(
      x = '2012-05-01T13:45:23-05'
    ),
    'YYYY-MM-DDThh:mm:ss-hh'
  )

})


testthat::test_that('Assume all timezone offsets are identical', {
  
  expect_equal(
    iso8601_format(
      x = c(
        '2012-05-01T13:45:23-05',
        NA_character_,
        '2012-05-01T13:45:23+05'
      )
    ),
    'YYYY-MM-DDThh:mm:ss-hh'
  )
  
  expect_equal(
    iso8601_format(
      x = c(
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45:23-05'
      )
    ),
    'YYYY-MM-DDThh:mm:ss+hh'
  )
  
})
