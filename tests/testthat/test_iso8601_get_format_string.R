context('Get format string for ISO 8601 datetime')

library(dataCleanr)

# Load test data --------------------------------------------------------------

data <- c(
  '2012-05-01T13:45:23+05',
  '2012-05-01T13:45:23-05',
  '2012-05-01T13:45:23',
  '2012-05-01T13:45+05',
  '2012-05-01T13:45-05',
  '2012-05-01T13:45',
  '2012-05-01T13+05',
  '2012-05-01T13-05',
  '2012-05-01T13',
  '2012-05-01',
  '2012',
  '13:45:23+05',
  '13:45:23-05',
  '13:45:23',
  '13:45+05',
  '13:45-05',
  '13:45',
  '13+05',
  '13-05',
  '13'
)

# date HMS --------------------------------------------------------------------

testthat::test_that('Test date HMS', {

  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45:23+05:00'
    ),
    'YYYY-MM-DDThh:mm:ss+hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45:23-05:00'
    ),
    'YYYY-MM-DDThh:mm:ss-hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45:23+05'
    ),
    'YYYY-MM-DDThh:mm:ss+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45:23-05'
    ),
    'YYYY-MM-DDThh:mm:ss-hh'
  )

  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45:23'
    ),
    'YYYY-MM-DDThh:mm:ss'
  )
  
})

# date HM ---------------------------------------------------------------------

testthat::test_that('Test date HM', {
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45+05:30'
    ),
    'YYYY-MM-DDThh:mm+hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45-05:30'
    ),
    'YYYY-MM-DDThh:mm-hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45+05'
    ),
    'YYYY-MM-DDThh:mm+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45-05'
    ),
    'YYYY-MM-DDThh:mm-hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13:45'
    ),
    'YYYY-MM-DDThh:mm'
  )
  
})

# date H ----------------------------------------------------------------------

testthat::test_that('Test date H', {

  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13+05:00'
    ),
    NA_character_
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13-05:00'
    ),
    NA_character_
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13+05'
    ),
    'YYYY-MM-DDThh+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13-05'
    ),
    'YYYY-MM-DDThh-hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01T13'
    ),
    'YYYY-MM-DDThh'
  )
  
})

# date ------------------------------------------------------------------------

testthat::test_that('Test date', {
  
  expect_equal(
    iso8601_get_format_string(
      x = '2012-05-01'
    ),
    'YYYY-MM-DD'
  )

  expect_equal(
    iso8601_get_format_string(
      x = '2012'
    ),
    'YYYY'
  )
  
})

# time ------------------------------------------------------------------------

testthat::test_that('Test time', {
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45:13+05:00'
    ),
    'hh:mm:ss+hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45:13-05:00'
    ),
    'hh:mm:ss-hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45:13+05'
    ),
    'hh:mm:ss+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45:13-05'
    ),
    'hh:mm:ss-hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45:13'
    ),
    'hh:mm:ss'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45+05:00'
    ),
    'hh:mm+hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45-05:00'
    ),
    'hh:mm-hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45+05'
    ),
    'hh:mm+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45-05'
    ),
    'hh:mm-hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13:45'
    ),
    'hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13+05:00'
    ),
    NA_character_
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13-05:00'
    ),
    NA_character_
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13+05'
    ),
    'hh+hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13-05'
    ),
    'hh-hh'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = '13'
    ),
    'hh'
  )
  
})
  
# Multiple formats ------------------------------------------------------------

testthat::test_that('Test multiple formats', {
  
  # date HMS & date HM w/time zone
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23-05:00',
        '2012-05-01T13:45:23+05:00',
        '2012-05-01T13:45+05:00',
        '2012-05-01T13:45:23+05:00'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45+05',
        '2012-05-01T13:45:23+05'
      )
    )
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23-05:00',
          '2012-05-01T13:45:23+05:00',
          '2012-05-01T13:45+05:00',
          '2012-05-01T13:45:23+05:00'
        )
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh:mm'
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23-05',
          '2012-05-01T13:45:23+05',
          '2012-05-01T13:45+05',
          '2012-05-01T13:45:23+05'
        )
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh'
  )
  
  # date HMS & date HM
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23',
        '2012-05-01T13:45',
        '2012-05-01T13:45',
        '2012-05-01T13'
      )
    )
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23',
          '2012-05-01T13:45',
          '2012-05-01T13:45',
          '2012-05-01T13'
        )
      )
    ),
    'YYYY-MM-DDThh:mm'
  )
  
  # date HMS permutations & date
  
  expect_equal(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23-05:00',
        '2012-05-01T13:45:23+05:00'
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh:mm'
  )
  
  expect_equal(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23+05'
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh'
  )
  
  # date
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012-05-01',
        '2012'
      )
    )
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01',
          '2012-05-01',
          '2012-05-01',
          '2012'
        )
      )
    ),
    'YYYY-MM-DD'
  )
  
  # time
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '13:45:23',
          '13:45',
          '13',
          '13'
        )
      )
    ),
    'hh'
  )

})

# Check format rules ----------------------------------------------------------

testthat::test_that('Test format rules', {

  # Valid format combinations
  
  expect_equal(
    validate_format_rules(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012-05-01'
      )
    ),
    NULL
  )
  
  expect_equal(
    validate_format_rules(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012'
      )
    ),
    NULL
  )
  
  expect_equal(
    validate_format_rules(
      x = c(
        '2012-05-01T13',
        '2012-05-01T13:45',
        '2012-05-01T13:45:23',
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23+05'
      )
    ),
    NULL
  )
  
  expect_equal(
    validate_format_rules(
      x = c(
        '13:45:23+05',
        '13:45-05',
        '13+05',
        '13:45:23',
        '13:45',
        '13'
      )
    ),
    NULL
  )
  
  # Invalid format combinations
  
  expect_error(
    validate_format_rules(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012-05-01',
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )
  
  expect_error(
    validate_format_rules(
      x = c(
        '2012-05-01',
        '2012-05-01T13:45',
        '2012-05-01',
        '13:45:23',
        '13:45',
        '13',
        '13',
        '2012',
        '2012'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23',
        '2012-05-01T13:45',
        '2012-05-01',
        '2012-05-01'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23',
        '2012-05-01T13:45',
        '2012-05-01',
        '2012-05-01',
        '2012-05-01'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01',
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01',
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )
  
  expect_warning(
    iso8601_get_format_string(
      x = c(
        '2012-05-01',
        '2012-05-01',
        '2012-05-01',
        '13:45:23',
        '13:45',
        '13',
        '13'
      )
    )
  )

})

# return.format = T -----------------------------------------------------------

testthat::test_that('Test for output when return.format = T', {

  expect_equal(
    attributes(
      iso8601_get_format_string(
        x = c(
          '2012-05-01',
          '2012-05-01',
          '2012-05-01'
        ),
        return.format = TRUE
      )
    ),
    list(
      names = c('x', 'format'),
      class = 'data.frame',
      row.names = c(1, 2, 3)
    )
  )
  
})

# + and - time zone offsets present -------------------------------------------

testthat::test_that('+ and - time zone offsets present', {
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23+05:00',
          '2012-05-01T13:45:23+05:00',
          '2012-05-01T13:45:23-05:00',
          '2012-05-01T13:45:23'
        )
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh:mm'
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23+05',
          '2012-05-01T13:45:23+05',
          '2012-05-01T13:45:23-05',
          '2012-05-01T13:45:23'
        )
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh'
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '2012-05-01T13:45:23+05',
          '13:45:23+05',
          '2012-05-01T13:45:23-05',
          '2012-05-01T13:45:23'
        )
      )
    ),
    'YYYY-MM-DDThh:mm:ss\u00B1hh'
  )
  
  expect_equal(
    suppressWarnings(
      iso8601_get_format_string(
        x = c(
          '13:45:23+05',
          '13:45:23+05',
          '2012-05-01T13:45:23-05',
          '2012-05-01T13:45:23'
        )
      )
    ),
    'hh:mm:ss\u00B1hh'
  )
  
})


# Use 

expect_equal(
  suppressWarnings(
    iso8601_get_format_string(
      x = c(
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45:23+05',
        '2012-05-01T13:45:23-05',
        '2012-05-01T13:45:23'
      )
    )
  ),
  'YYYY-MM-DDThh:mm:ss\u00B1hh'
)
