library(lipidomicsUtils)
context("formula mathematics")

## tests for exact mass to ion mass --------------------------------------------
test_that("correct formula mathematics", {
  
  # check for sub formula
  expect_equal(contains_formula("C6H12O6", "H2O"), TRUE)
  expect_equal(contains_formula("C6H12O6", "NH3"), FALSE)
  
  # subtraction of formula
  expect_equal(formula_subtraction("C6H12O6", "H2O"), "C6H10O5")
  expect_error(formula_subtraction("C6H12O6", "NH3"))
  
  # addition of formula
  expect_equal(formula_addition("C6H10O5", "H2O"), "C6H12O6")

})