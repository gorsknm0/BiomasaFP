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
library(usethis)
library(devtools)

#-----------------------------------------------
# 1. test the equation works for 1 tree with specified d and h
#------------------------------------------------
test_that("AGBRezende06 works 1", {
  expect_equal(round(AGBRezende06(d=121, h=3.9),4), round(0.01613620088,4)) ### allow to 4dp
})



# --------------------------------------------------
# 2.a. test that mergefp() function runs successfully
# --------------------------------------------------
#
test_that("mergefp runs without error", {
#
#   # Load or create some example data
  trees<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/treedata_cerrado.csv")
#str(trees)
#head(trees$`Census Date`)
  md<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/plotmd_cerrado.csv")
  wd<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/wd_cerrado.csv")


#
#   # Run the function
   result <- mergefp(trees, md, wd)
#
#   # Check that the output is a data frame
   expect_true(is.data.frame(result))
 })




# ------------------------------------------------
# 2.b. test that calcAGB() function runs successfully
# ------------------------------------------------

test_that("CalcAGB runs without error for Rezende06", {
  #
  #   # Load or create some example data
      # str(result2a)
 trees<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/treedata_cerrado.csv")
  #str(trees)
  #head(trees$`Census Date`)
 md<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/plotmd_cerrado.csv")
 wd<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/wd_cerrado.csv")
 dat <- mergefp(trees, md, wd)
 plot5 <-dat %>%
   filter(PlotCode == "TGP-29") # this plot only has ExtraD (no D)

  #    Run the function
 result <- CalcAGB(plot5, AGBFun = AGBRezende06)#, dbh = "Extra.D4") #, height.data = "Height")
  #
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  })



#---------------------------------------------
# 3. test the equation results for CalcAGB() for test plot 5 with extra diameter specified is the same as default (no d specified)
#---------------------------------------------

test_that("CalcAGB runs without error for Rezende06", {
  #
  #   # Load or create some example data
  # str(result2a)
  trees<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/treedata_cerrado.csv")
  #str(trees)
  #head(trees$`Census Date`)
  md<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/plotmd_cerrado.csv")
  wd<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/cerrado/wd_cerrado.csv")
  dat <- mergefp(trees, md, wd)
  plot5 <-dat %>%
    filter(PlotCode == "TGP-29")# this plot only has ExtraD (no D)

  #    Run the function
  result1 <- CalcAGB(plot5, AGBFun = AGBRezende06)
  result2 <- CalcAGB(plot5, AGBFun = AGBRezende06, dbh = "Extra.D4")
  #
  #   # Check that the output is a data frame
  expect_equal(result1$AGBInd, result2$AGBInd)
})





#---------------------------------------------
# 4. test the equation works for CalcAGB() with real heights specified (and check results match manual workings)
#---------------------------------------------



#------------------------------------------
# 5. test the equation works for SummaryAGWP
#------------------------------------------



#-----------------------------------------
# 6. test the equation results for summary AGWP match the query library
# ----------------------------------------


#----------------------------------------
# 7. test if work for a plot with no extra D
# --------------------------------------


#----------------------------------------
# 8. test if work for multiple plots different Ds etc
# ---------------------------------------


####------------------------------------------------------------####
####------------------------------------------------------------####

## REPEAT ALL TESTS BUT FOR Chv14 and check get same results in old verion of BiomasaFP
