context('Read ISO 8601 datetime strings into R')

library(dataCleanr)

# Load data -------------------------------------------------------------------

data <- read.table(
  system.file('datetimes.csv', package = 'dataCleanr'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# Test possible formats -------------------------------------------------------

testthat::test_that('Read possible formats', {

  expect_equal(
    iso8601_read(
      x = c(
        '2012-05-01T13:45:23-00',
        NA_character_
      )
    ),
    as.POSIXct('2012-05-01 13:45:23', tz = 'GMT')
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

})

