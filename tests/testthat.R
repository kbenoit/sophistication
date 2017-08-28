Sys.setenv("R_TESTS" = "")

library(sophistication)
library(testthat)
test_check("sophistication")
