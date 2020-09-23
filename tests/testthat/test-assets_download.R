context("test_assets_download")

testthat::test_that("assets download", {
  vcr::use_cassette("assets_download", {
    # skip cran check test
    testthat::skip_on_cran()

    # error - zero items
     testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(
          collections = "CB4_64_16D_STK-1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-47.02148, -12.98314, -42.53906, -17.35063),
          limit       = 0) %>%
        get_request() %>%
        assets_download(assets_name = c("blue", "evi")))

    # error - given another object
     testthat::expect_error(
       stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
         get_request(.) %>%
         assets_download(., assets_name = c("blue", "evi")))

   # error - wrong path
   testthat::expect_error(
     stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
       stac_search(
         collections = "CB4_64_16D_STK-1",
         datetime    = "2019-09-01/2019-11-01",
         limit       = 1) %>%
       get_request() %>%
       assets_download(assets_name = c("thumbnail"),
                       output_dir = "./non-existing-dir/"))

   # verify output object
   testthat::expect_equal(
     object = {
       x <- stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/") %>%
         stac_search(
           collections = "CB4_64_16D_STK-1",
           datetime    = "2019-09-01/2019-11-01",
           limit       = 1) %>%
         get_request() %>%
         assets_download(assets_name = c("thumbnail"), output_dir = ".")
       subclass(x)
     },
     expected = "STACItemCollection")
  })
})
