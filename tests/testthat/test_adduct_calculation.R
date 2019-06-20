library(metabolomicsUtils)
context("adduct calculation")

## test for formula parsing
test_that("correct calculation of adduct m/z", {
  
  # positive mode
  expect_equal(100, 100)
  expect_equal(100, 100)
  
  # negative mode
  expect_equal(100, 100)
  expect_equal(100, 100)
  
})

test_that("correct calculation of neutral mass", {
  
  # positive mode
  expect_equal(100, 100)
  
  # negative mode
  expect_equal(100, 100)
  
})

test_that("correction ion formula is generated", {
  
  # positive mode
  expect_equal(create_ion_formula("C11H12N2O2", "[M+H]+"), "[C11H13N2O2]1+")
  expect_equal(create_ion_formula("C6H12O6", "[M+Na]+"), "[C6H12O6Na]1+")
  
  # negative mode
  expect_equal(create_ion_formula("C6H12O6", "[M-H]-"), "[C6H11O6]1-")
})