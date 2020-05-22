Sys.setenv("R_TESTS" = "")

library("sophistication")
library("spacyr")
spacy_initialize()

library(testthat)
test_check("sophistication")
