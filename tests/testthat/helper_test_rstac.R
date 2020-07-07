library("vcr")

invisible(vcr::vcr_configure(dir = "../fixtures/vcr_cassettes",
                             write_disk_path = "../files"))
