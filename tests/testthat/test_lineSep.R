

test_that("lineSep repeats single character", {
  old <- options(width = 20)
  on.exit(options(old))
  
  x <- lineSep("-")
  expect_equal(nchar(cli::ansi_strip(x)), 18)
})

test_that("lineSep keeps multi-character strings", {
  expect_equal(lineSep("=="), "==")
})

test_that("ANSI styles are ignored in width calculation", {
  old <- options(width = 20)
  on.exit(options(old))
  
  styled <- cli::col_red("-")
  x <- lineSep(styled)
  
  expect_equal(nchar(cli::ansi_strip(x)), 18)
})
