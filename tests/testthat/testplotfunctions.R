context("testplotfunctions")  # Our file is called "test-check_output.R"
library(testthat)             # load testthat package
library(KOSMOSplotsR)         # load our package


#KOSMOStestdata=da

KOSMOStimeplot(KOSMOStestdata,"Parameter",control="Fjord")
