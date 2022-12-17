library(testthat)
library(rstac)
library(magrittr)

if (Sys.getenv("RSTAC_TESTS", unset = 0) == 1) {
  test_check("rstac")
}
