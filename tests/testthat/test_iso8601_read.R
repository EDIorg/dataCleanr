context('Read ISO 8601 datetime strings into R')

library(dataCleanr)

# Test possible formats -------------------------------------------------------

testthat::test_that('Read possible formats', {

  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23-00',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'Etc/GMT-0')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23-05',
        NA_character_
        )
      ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'Etc/GMT+5')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23+05',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'Etc/GMT-5')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'UTC')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:45:00', tz = 'UTC')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:00:00', tz = 'UTC')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01', tz = 'UTC')
  )
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012',
        NA_character_
      )
    ),
    as.integer('2012')
  )

})



# date HMS --------------------------------------------------------------------

testthat::test_that('Test date HMS', {
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45:23+05'
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'Etc/GMT-5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45:23-05'
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'Etc/GMT+5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45:23'
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'UTC')
  )
  
})

# date HM ---------------------------------------------------------------------

testthat::test_that('Test date HM', {
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45+05'
    ),
    as.POSIXct('2012-05-01 13:45', tz = 'Etc/GMT-5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45-05'
    ),
    as.POSIXct('2012-05-01 13:45', tz = 'Etc/GMT+5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13:45'
    ),
    as.POSIXct('2012-05-01 13:45', tz = 'UTC')
  )
  
})

# date H ----------------------------------------------------------------------

testthat::test_that('Test date H', {
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13+05'
    ),
    as.POSIXct('2012-05-01 13:00:00', tz = 'Etc/GMT-5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13-05'
    ),
    as.POSIXct('2012-05-01 13:00:00', tz = 'Etc/GMT+5')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01T13'
    ),
    as.POSIXct('2012-05-01 13:00:00', tz = 'UTC')
  )
  
})

# date ------------------------------------------------------------------------

testthat::test_that('Test date', {
  
  expect_equal(
    iso8601_read(
      x = '2012-05-01'
    ),
    as.POSIXct('2012-05-01', tz = 'UTC')
  )
  
  expect_equal(
    iso8601_read(
      x = '2012'
    ),
    2012
  )
  
})

# Multiple formats ------------------------------------------------------------

testthat::test_that('Test multiple formats', {
  
  # Multiple time zones throw an error
  
  expect_error(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45+05',
        '2012-05-01T13:45:23+05'
      )
    )
  )
  
  expect_error(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45',
        '2012-05-01T13:45:23+05'
      )
    )
  )

  # Variance in precision is supported
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45+05',
        '2012-05-01T13+05'
      )
    ),
    as.POSIXct(
      c(
        '2012-05-01 13:45:23',
        '2012-05-01 13:45:00',
        '2012-05-01 13:00:00'
      ),
      tz = 'Etc/GMT-5'
    )
  )
  
  # date HMS & date HM
  
  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23',
        '2012-05-01T13:45',
        '2012-05-01T13:45',
        '2012-05-01T13'
      )
    ),
    as.POSIXct(
      c(
        '2012-05-01 13:45:23',
        '2012-05-01 13:45:00',
        '2012-05-01 13:45:00',
        '2012-05-01 13:00:00'
      ),
      tz = 'UTC'
    )
  )
  
  # date
  
  expect_warning(
    iso8601_read(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012-05-01',
        '2012'
      )
    )
  )
  
  expect_warning(
    iso8601_read(
      x = c(
        '2012-05-01',
        '2012',
        '2012',
        '2012'
      )
    )
  )
  
  # time
  
  expect_error(
    iso8601_read(
      x = c(
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )
  
})

