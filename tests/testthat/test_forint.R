# context("Forint function to check number and add forint suffix")
# omitted: function file name used instead

library(mr)

test_that("forint returns number passed and Ft", {
  expect_equal(forint(15), "15 Ft")
  expect_equal(forint(120), "120 Ft")
  
})

#> Test passed 😸

test_that("forint raises error", {
  expect_error(forint("15"), "Must be of type")
  expect_error(forint(), "missing, with no default")
  
})

