context("test_assets_download")

testthat::test_that("assets download", {
  vcr::use_cassette("assets_download", {
    # skip cran check test
    testthat::skip_on_cran()

    # error - zero items
     testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        stac_search(
          collections = "MOD13Q1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
          limit       = 0) %>%
        get_request() %>%
        assets_download(assets_name = c("blue", "evi")))

    # error - given another object
     testthat::expect_error(
       stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
            force_version = "0.9.0") %>%
         get_request(.) %>%
         assets_download(., assets_name = c("blue", "evi")))

   # error - wrong path
   testthat::expect_error(
     stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
          force_version = "0.9.0") %>%
       stac_search(
         collections = "CB4_64-1",
         datetime    = "2019-09-01/..",
         limit       = 1) %>%
       get_request() %>%
       assets_download(assets_name = c("thumbnail"),
                       output_dir = "./non-existing-dir/"))

   # verify output object
   testthat::expect_equal(
     object = {
       x <- stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                 force_version = "0.9.0") %>%
         stac_search(
           collections = "CB4_64-1",
           datetime    = "2019-09-01/..",
           limit       = 1) %>%
         get_request() %>%
         assets_download(assets_name = c("thumbnail"), output_dir = ".")
       class(x)
     },
     expected = "stac_items")
  })
})
