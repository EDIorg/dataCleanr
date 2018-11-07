context('Read ISO 8601 datetime characters')

library(dataCleanr)

# Load data -------------------------------------------------------------------

data <- read.table(
  paste0(path.package('dataCleanr'), '/tests/test_data/datetimes.csv'),
  header = T,
  sep = ",",
  as.is = T,
  na.strings = "NA")

# Test possible formats -------------------------------------------------------

testthat::test_that('Read possible formats', {

  # expect_equal(
  #     iso8601_read(
  #       x = data$iso8601
  #       ),
  #     'test'
  #     )

})

