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

## data read here (shared external folder) for now (move to test data in package once permissions sorted)
trees<-read.csv("C:/Users/georg/OneDrive - University of Leeds/FPTeam_Research/Martin_Sullivan_R/Test_Data_Dry/biomasafp_test_data/cerrado/treedata_cerrado.csv")
md<-read.csv("C:/Users/georg/OneDrive - University of Leeds/FPTeam_Research/Martin_Sullivan_R/Test_Data_Dry/biomasafp_test_data/cerrado/plotmd_cerrado.csv")
wd<-read.csv("C:/Users/georg/OneDrive - University of Leeds/FPTeam_Research/Martin_Sullivan_R/Test_Data_Dry/biomasafp_test_data/cerrado/wd_cerrado.csv")
dat<-mergefp(trees, md, wd)


#---- Test AGBRezende06 (newly added dry forest equation) ----


##---- 1. test the equation works for 1 tree with specified d and h
#
##
test_that("AGBRezende06 works for 1 tree", {
  expect_equal(round(AGBRezende06(d=121, h=3.9),4), round(0.01613620088,4)) ### allow to 4dp
})


# ----pre test2:  test that mergefp() function runs successfully ### MOVE TO mergefp testing?
##
#
#
test_that("mergefp runs without error", {
#   # Load or create some example data
  trees
  md
  wd
#   # Run the function
   result <- mergefp(trees, md, wd)
#   # Run the function
   result <- mergefp(trees, md, wd)
#   # Check that the output is a data frame with correct number of rows and columns
   expect_true(is.data.frame(result))
   expect_equal(nrow(result), nrow(trees))
   expect_equal(ncol(result), 47) # 47 = prev version of BiomasaFP + 3 new cols
 })


# ---- 2. test that calcAGB() function runs successfully
#
##

##---- 2a. CalcAGB REZENDE06 1 plot with only ExtraD no issues single census
###----   i) default settings
test_that("CalcAGB runs without error for Rezende06 1 plot with only ExtraD no issues, default settings", {
  #   # Load or create some example data
 plot5 <-dat %>%
   filter(PlotCode == "TGP-29") # this plot only has ExtraD (no D)
  #    Run the function
 result <- CalcAGB(plot5, AGBFun = AGBRezende06)#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot5))
  expect_equal(ncol(result), (47+18)) # 47 = input data plus 18 extra cols
  })

#####
###----   ii) select settings (Extra D4)
test_that("CalcAGB runs without error for Rezende06 1 plot no issues, specified Extra.D4", {
  #   # Load or create some example data
  plot5 <-dat %>%
    filter(PlotCode == "TGP-29") # this plot only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot5, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame with correct no of cols and rows
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot5))
  expect_equal(ncol(result), (47+18)) # 47 = input data plus 18 extra cols
})

#####
###---- 2a. iii) select settings (Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
###---- 2a. iv)  select settings (Extra D4 AND Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
##---- 2b. CalcAGB REZENDE06 1 MULTICENSUS plot with Extra D only no issues
###----  i) default settings
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot with extra.D4 only, default settings", {
  #   # Load or create some example data
  plot2 <-dat %>%
    filter(PlotCode == "TGP-26") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot2, AGBFun = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot2))
  expect_equal(ncol(result), (47+18))
})

###---- 2b. ii) select settings ExtraD
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot with extra.D4 only, specified Extra.D4", {
  #   # Load or create some example data
  plot2 <-dat %>%
    filter(PlotCode == "TGP-26") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot2, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot2))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2b. iii)  select settings (Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
###---- 2b. iv)  select settings (Extra D4 AND Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
##---- 2c. CalcAGB REZENDE06 1 MULTICENSUS plot with both Ds no issues
###----  i) default settings
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot both Ds, default settings", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot1, AGBFun = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot1))
  expect_equal(ncol(result), (47+18))
})

#####
###----2c.ii) specified dbh = Extra.D4
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot both Ds, dbh= Extra.D4", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot1, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot1))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2c. iii)  select settings (Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot both Ds, local height", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36")# this plot is multicensus and only has ExtraD (no D)
  heightplot1<-plot1 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                              TRUE ~ 5))
  hts<-local.heights(heightplot1, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot1, AGBFun = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot1))
  expect_equal(ncol(result), (47+18))
})

# NOTE THAT THIS WORKS BUT PERHAPS IS PULLING FROM D4, check in later tests

#####
###---- 2c. iv)  select settings (Extra D4 AND Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot both Ds, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36")# this plot is multicensus and only has ExtraD (no D)
  heightplot1<-plot1 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot1, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot1, AGBFun = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot1))
  expect_equal(ncol(result), (47+18))
})

####
##---- 2d. CalcAGB REZENDE06 1 MULTICENSUS plot without ExtraDs
###----  i) default settings
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot lacking ExtraD4, default settings", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot3, AGBFun = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot3))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2d.ii) specified dbh = Extra.D4
test_that("CalcAGB runs without error for Rezende06 1 multicensus lacking Extra.D4, dbh= Extra.D4", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot3, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot3))
  expect_equal(ncol(result), (47+18))
})


#####
###---- 2d. iii)  select settings (Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot lacking extra.D4, local height", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27")# this plot is multicensus and only has ExtraD (no D)
  heightplot3<-plot3 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot3, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot3, AGBFun = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot3))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2d. iv)  select settings (Extra D4 AND Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot lacking Extra.D4, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27")# this plot is multicensus and only has ExtraD (no D)
  heightplot3<-plot3 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot3, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot3, AGBFun = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot3))
  expect_equal(ncol(result), (47+18))
})


####
##---- 2e. CalcAGB REZENDE06 1 MULTICENSUS plot With issues: NO RECRUITS
###----  i) default settings
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot no recruits, default settings", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot7, AGBFun = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot7))
  expect_equal(ncol(result), (47+18))
})


#####
###---- 2e.ii) specified dbh = Extra.D4
test_that("CalcAGB runs without error for Rezende06 1 multicensus no recruits, dbh= Extra.D4", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- CalcAGB(plot7, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot7))
  expect_equal(ncol(result), (47+18))
})


#####
###---- 2e. iii)  select settings (Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus no recruits, local height", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37")# this plot is multicensus and only has ExtraD (no D)
  heightplot7<-plot7 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot7, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot7, AGBFun = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot7))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2e. iv)  select settings (Extra D4 AND Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot no recruits, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37")# this plot is multicensus and only has ExtraD (no D)
  heightplot7<-plot7 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot7, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightplot7, AGBFun = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(plot7))
  expect_equal(ncol(result), (47+18))
})


####
##---- 2f. CalcAGB REZENDE06 ALL PLOTS together
###----  i) default settings
test_that("CalcAGB runs without error for Rezende06 all test plots, default settings", {
  #   # Load or create some example data
  dat
  #    Run the function
  result <- CalcAGB(dat, AGBFun = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(dat))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2f.ii) specified dbh = Extra.D4
test_that("CalcAGB runs without error for Rezende06 1 multicensus no recruits, dbh= Extra.D4", {
  #   # Load or create some example data
  dat
  #    Run the function
  result <- CalcAGB(dat, AGBFun = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(dat))
  expect_equal(ncol(result), (47+18))
})


#####
###---- 2f. iii)  select settings (Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus no recruits, local height", {
  #   # Load or create some example data

  heightdat<-dat %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightdat, AGBFun = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(dat))
  expect_equal(ncol(result), (47+18))
})

#####
###---- 2f. iv)  select settings (Extra D4 AND Local Height)
test_that("CalcAGB runs without error for Rezende06 1 multicensus plot no recruits, select Extra.D4 and local height", {
  #   # Load or create some example data
  heightdat<-dat%>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightdat, AGBFun = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(dat))
  expect_equal(ncol(result), (47+18))
})

############################################

#----3. test the equation results for CalcAGB() for all plots with extra diameter specified is the same as default (no d specified)
#
#-

test_that("CalcAGB uses correct default diameter (Extra.D4) for Rezende06", {
  #
  #   # Load or create some data
  dat
  #    Run the function
  result1 <- CalcAGB(dat, AGBFun = AGBRezende06)
  result2 <- CalcAGB(dat, AGBFun = AGBRezende06, dbh = "Extra.D4")
  #
  #   # Check that the output is a data frame
  expect_equal(result1$AGBInd, result2$AGBInd)
})


################### NOTE THAT THIS WILL NEED REWRITING TO CHECK DEFAULT HEIGHTS CORRECT TOO ONCE MS DEVELOPED

###########################################

#---- test the equation results for CalcAGB() all plots

##---- 4 a. test the equation results for CalcAGB() all plots match manual workings in the same file default settings (unable to test real heights as will only run on height parameters (not heights)
#

test_that("CalcAGB results for Rezende06 match the manual workings in the same file (default settings)", {
  #
  #   # Load or create some data
  dat
  pre_manualworkings<-CalcAGB(dat, AGBFun = AGBRezende06)
  #    Run the function
  result <- CalcAGB(dat, AGBFun = AGBRezende06)
  resultmanualworkings<- pre_manualworkings %>%
    mutate( AGBInd_manual = case_when(Extra.D4==0 ~ NA,
                            F1 == 0 ~ NA,
                            TRUE ~ (-0.49129 + 0.02912 * ((Extra.D4/10) ^2 ) * HtF) / 1000))
  #   # Check that the output is a data frame
  expect_equal(round(result$AGBind,6), round(resultmanualworkings$AGBInd_manual,6))
})

#write.csv(resultmanualworkings, "C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/coding_checks_cerrado/resultmanualworkings_RezendeCalcAGB.csv")


##----4 b. test the equation results for CalcAGB() all plots match manual workings in the same file local heights(unable to test real heights as it )
#
#

test_that("CalcAGB results for Rezende06 match the manual workings in the same file (local heights)", {
  #
  #   # Load or create some data
  dat
  heightdat<-dat %>%
    mutate(F5 = case_when(is.na(F5) ~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  pre_manualworkings<-CalcAGB(heightdat, AGBFun = AGBRezende06, height.data = h.params)
  #    Run the function
  result <- CalcAGB(heightdat, AGBFun = AGBRezende06, height.data = h.params)
  resultmanualworkings<- pre_manualworkings %>%
    mutate( AGBInd_manual = case_when(Extra.D4==0 ~ NA,
                                      F1 == 0 ~ NA,
                                      TRUE ~ (-0.49129 + 0.02912 * ((Extra.D4/10) ^2 ) * HtF) / 1000))
  #   # Check that the output is a data frame
  expect_equal(round(result$AGBind,6), round(resultmanualworkings$AGBInd_manual,6))
})



##----4 c. test the equation results for CalcAGB() all plots default settings only contain NA for trees that are dead or have Extra.D4 = 0 or Extra.D4 = NA
#
#

test_that("CalcAGB results for Rezende06 (default settings) only have NA for dead trees or trees with 0 or NA Extra.D4", {
  result <- CalcAGB(dat, AGBFun = AGBRezende06)
  na_condition <- result$F1 == 0 | is.na(result$Extra.D4) | result$Extra.D4 == 0
  # Rows meeting the condition should have NA AGBind
  expect_true(all(is.na(result$AGBind[na_condition])))
  # All other rows should have non-NA AGBind
  expect_true(all(!is.na(result$AGBind[!na_condition])))
})

##----4 d. test the equation results for CalcAGB() all plots local heights only contain NA for trees that are dead or have Extra.D4 = 0 or Extra.D4 = NA
#
#

test_that("CalcAGB results for Rezende06 (local heights) only have NA for dead trees or trees with 0 or NA Extra.D4", {
  dat
  heightdat<-dat %>%
    mutate(F5 = case_when(is.na(F5) ~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- CalcAGB(heightdat, AGBFun = AGBRezende06, height.data = h.params)
  na_condition <- result$F1 == 0 | is.na(result$Extra.D4) | result$Extra.D4 == 0
  # Rows meeting the condition should have NA AGBind
  expect_true(all(is.na(result$AGBind[na_condition])))
  # All other rows should have non-NA AGBind
  expect_true(all(!is.na(result$AGBind[!na_condition])))
})

#write.csv(result, "C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/Biomasa_Test_MSVisit/BiomasaFP_test_GP_NG_May2026/input_11June26/coding_checks_cerrado/result_RezendeCalcAGB_localheights.csv")


##----4 e. test the equation results for CalcAGB() DEAD STEMS all plots local heights only contain NA for trees that are dead or have Extra.D4 = 0 or Extra.D4 = NA
#
#

#### NOT YET DONE, ask MS how it works?

################################################

#----5. test the equation works for SummaryAGWP
#
#
##---- 5.a test the SummaryAGWP runs successfully for Rezende06, 1 plot singlecensus extraD only
###----     i) default settings
test_that("SummaryAGWP() runs without error for Rezende06 1 singlecensus plot with only ExtraD no issues, default settings", {
  #   # Load or create some example data
  plot5 <-dat %>%
    filter(PlotCode == "TGP-29") # this plot only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot5, AGBEquation =AGBRezende06)#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 20)
})


###---- 5a. ii) select settings ExtraD
test_that("SummaryAGWP() runs without error for Rezende06 1 singlecensus plot with extra.D4 only, specified Extra.D4", {
  #   # Load or create some example data
  plot5 <-dat %>%
    filter(PlotCode == "TGP-29") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot5, AGBEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result),1)
  expect_equal(ncol(result), 20)
})

#####
###---- 5a. iii) select settings (Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
###---- 5a. iv)  select settings (Extra D4 AND Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
##---- 5b. CalcAGB REZENDE06 1 MULTICENSUS plot with Extra D only no issues
###----  i) default settings
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot with extra.D4 only, default settings", {
  #   # Load or create some example data
  plot2 <-dat %>%
    filter(PlotCode == "TGP-26") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot2, AGBEquation = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

###---- 5b. ii) select settings ExtraD
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot with extra.D4 only, specified Extra.D4", {
  #   # Load or create some example data
  plot2 <-dat %>%
    filter(PlotCode == "TGP-26") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot2, AGBEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

#####
###---- 5b. iii)  select settings (Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
###---- 5b. iv)  select settings (Extra D4 AND Local Height)

## NOT YET ABLE TO DO FOR EXTRA.D4 predictor local heights

#####
##---- 5c. SummaryAGWP REZENDE06 1 MULTICENSUS plot with both Ds no issues
###----  i) default settings
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot both Ds, default settings", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot1, AGBEquation = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result),2)
  expect_equal(ncol(result), 20)
})

#####
###----5c.ii) specified dbh = Extra.D4
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot both Ds, dbh= Extra.D4", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot1, AGBEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

#####
###---- 5c. iii)  select settings (Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot both Ds, local height", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36")# this plot is multicensus and only has ExtraD (no D)
  heightplot1<-plot1 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot1, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot1, AGBEquation = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result),2)
  expect_equal(ncol(result), 20)
})

# NOTE THAT THIS WORKS BUT PERHAPS IS PULLING FROM D4, check in later tests

#####
###---- 5c. iv)  select settings (Extra D4 AND Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot both Ds, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot1 <-dat %>%
    filter(PlotCode == "TGP-36")# this plot is multicensus and only has ExtraD (no D)
  heightplot1<-plot1 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot1, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot1, AGBEquation = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

####
##---- 5d. SUMMARYAGWP REZENDE06 1 MULTICENSUS plot without ExtraDs
###----  i) default settings
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot lacking ExtraD4, default settings", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot3, AGBEquation = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

#####
###---- 5d.ii) specified dbh = Extra.D4
test_that("SUmmaryAGWP runs without error for Rezende06 1 multicensus lacking Extra.D4, dbh= Extra.D4", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot3, AGBFEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})


#####
###---- 5d. iii)  select settings (Local Height)
test_that("SUmmaryAGWP runs without error for Rezende06 1 multicensus plot lacking extra.D4, local height", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27")# this plot is multicensus and only has ExtraD (no D)
  heightplot3<-plot3 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot3, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot3, AGBEquation = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result),20)
})

#####
###---- 5d. iv)  select settings (Extra D4 AND Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot lacking Extra.D4, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot3 <-dat %>%
    filter(PlotCode == "TGP-27")# this plot is multicensus and only has ExtraD (no D)
  heightplot3<-plot3 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot3, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot3, AGBFun = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})


####
##---- 5e. CalcAGB REZENDE06 1 MULTICENSUS plot With issues: NO RECRUITS
###----  i) default settings
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot no recruits, default settings", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot7, AGBEquation = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})


#####
###---- 5e.ii) specified dbh = Extra.D4
test_that("SUmmaryAGWP runs without error for Rezende06 1 multicensus no recruits, dbh= Extra.D4", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37") # this plot is multicensus and only has ExtraD (no D)
  #    Run the function
  result <- SummaryAGWP(plot7, AGBEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result),2)
  expect_equal(ncol(result), 20)
})


#####
###---- 5e. iii)  select settings (Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus no recruits, local height", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37")# this plot is multicensus and only has ExtraD (no D)
  heightplot7<-plot7 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot7, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot7, AGBEquation = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 20)
})

#####
###---- 5e. iv)  select settings (Extra D4 AND Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot no recruits, select Extra.D4 and local height", {
  #   # Load or create some example data
  plot7 <-dat %>%
    filter(PlotCode == "TGP-37")# this plot is multicensus and only has ExtraD (no D)
  heightplot7<-plot7 %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightplot7, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightplot7, AGBEquation = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result),2)
  expect_equal(ncol(result), 20)
})


####
##---- 5f. CalcAGB REZENDE06 ALL PLOTS together
###----  i) default settings
test_that("Summary AGWP runs without error for Rezende06 all test plots, default settings", {
  #   # Load or create some example data
  dat
  #    Run the function
  result <- SummaryAGWP(dat, AGBEquation = AGBRezende06 )#, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 13)
  expect_equal(ncol(result), 20)
})

#####
###---- 5f.ii) specified dbh = Extra.D4
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus no recruits, dbh= Extra.D4", {
  #   # Load or create some example data
  dat
  #    Run the function
  result <- SummaryAGWP(dat, AGBEquation = AGBRezende06, dbh = "Extra.D4") #, height.data = "Height")
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 13)
  expect_equal(ncol(result), 20)
})

#####
###---- 5f. iii)  select settings (Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus no recruits, local height", {
  #   # Load or create some example data

  heightdat<-dat %>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightdat, AGBEquation = AGBRezende06, height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 13)
  expect_equal(ncol(result), 20)
})

#####
###---- 5f. iv)  select settings (Extra D4 AND Local Height)
test_that("SummaryAGWP runs without error for Rezende06 1 multicensus plot no recruits, select Extra.D4 and local height", {
  #   # Load or create some example data
  heightdat<-dat%>%
    mutate(F5 = case_when(is.na(F5)~ NA,
                          TRUE ~ 5))
  hts<-local.heights(heightdat, no.plot=FALSE)
  h.params<-hd.simplify(hts[[1]])
  #    Run the function
  result <- SummaryAGWP(heightdat, AGBEquation = AGBRezende06, dbh= "Extra.D4", height.data = h.params)
  #   # Check that the output is a data frame
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 13)
  expect_equal(ncol(result), 20)
})





##############################################


#----6. test the equation results for Summary() for all plots with extra diameter specified is the same as default (no d specified)
#
#-

test_that("SummaryAGWP uses correct default diameter (Extra.D4) for Rezende06", {
  #
  #   # Load or create some data
  dat
  #    Run the function
  result1 <- SummaryAGWP(dat, AGBEquation = AGBRezende06)
  result2 <- SummaryAGWP(dat, AGBEquation = AGBRezende06, dbh = "Extra.D4")
  #
  #   # Check that the output is a data frame
  expect_equal(result1, result2)
})

################### NOTE THAT THIS WILL NEED REWRITING TO CHECK DEFAULT HEIGHTS CORRECT TOO ONCE MS DEVELOPED

#############################################

#---- 7. test the equation results for summary AGWP match the ForestPlots query library (when feldpausch heights used)

test_that("SummaryAGWP AGB all plots for Rezende06 matches the results in ForestPlots Query Library to 3dp", {
  #
  #   # Load or create some data
  dat
  QL<-read.csv("C:/Users/georg/OneDrive - University of Leeds/Git_link_research/ForestPlotsTeam/TeamMembers/Georgia/BiomasaFP/BiomasaFP_tests/QL_Cerrado_AGB_by_census_on_Extra_D4_Rezende_FPlotsQL.csv")
  #    Run the function
  result <- SummaryAGWP(dat, AGBEquation = AGBRezende06)
  resultsmatched<-result %>%
    full_join(QL, by=c('PlotCode', 'Census.No'='CensusNo'))

  #   # Check that the output is a data frame
  expect_equal(round(resultsmatched$AGB.ha,3), resultsmatched$AGB.Rezende.et.al.2006...Mg.DW.Ha.1.)
})



#---- 8. test the equation results for summary AGWP are correct

###### NOT YET DONE. MS to do?


####------------------------------------------------------------####

## REPEAT ALL TESTS BUT FOR Chv14 and check get same results in old version of BiomasaFP
