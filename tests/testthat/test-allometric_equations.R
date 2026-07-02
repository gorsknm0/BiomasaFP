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
library(dplyr)
library(readr)


# 1. test the equation works for 1 tree with specified d and h
test_that("AGBRezende06 works 1", {
  expect_equal(round(AGBRezende06(d=121, h=3.9),4), round(0.01613620088,4)) ### allow to 4dp
})


# 2. test the equation works for CalcAGB() for test plot 5 with diameter and height specified


# 3. test the equation works for CalcAGB() for test plot 5 with default d (should be Extra.D4 for this equation)


