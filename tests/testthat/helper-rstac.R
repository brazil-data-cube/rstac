library("vcr")
library("magrittr")
invisible(vcr::vcr_configure(
  dir    = "../fixtures",
  record = "new_episodes",
  write_disk_path = "."
))
vcr::check_cassette_names()
