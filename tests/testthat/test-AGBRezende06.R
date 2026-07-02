# ---------------------------------------------
# Notes for Martin
# ---------------------------------------------
# Use this file to add and run package tests.
#
# 1. Write a test using the format:
#    test_that("description of test", {
#      expectations
#    })
#
# 2. Make sure the required packages are installed:
#    install.packages("usethis")
#    install.packages("devtools")
#
# 3. After adding your test, run the following in the Console:
#    devtools::test()
#
# This will run all tests in the package and show whether they pass or fail.

# ---------------------------------------------
# Example 1: Simple calculation test
# ---------------------------------------------
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# ---------------------------------------------
# Example 2: Function runs successfully
# ---------------------------------------------
# test_that("SummaryAGWP runs without error", {
#
#   # Load or create some example data
#   data_example <- BiomasaFP::your_example_data
#
#   # Run the function
#   result <- SummaryAGWP(data_example)
#
#   # Check that the output is a data frame
#   expect_true(is.data.frame(result))
# })

# ---------------------------------------------
# Add your tests below
# ---------------------------------------------
test_that("AGBRezende06 works 1", {
  expect_equal(AGBRezende06(d=121, h=3.9), 0.0161362)
})
