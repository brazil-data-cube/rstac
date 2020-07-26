library("vcr")
library("magrittr")
invisible(vcr::vcr_configure(
  dir    = "../fixtures",
  record = "new_episodes"
))
vcr::check_cassette_names()
