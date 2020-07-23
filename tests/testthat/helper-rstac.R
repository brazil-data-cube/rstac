library("vcr")
library("magrittr")
invisible(vcr::vcr_configure(
  dir = "../fixtures"
))
vcr::check_cassette_names()
