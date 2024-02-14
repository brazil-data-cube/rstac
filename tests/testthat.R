library(testthat)
library(rstac)
library(magrittr)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("rstac")
}
