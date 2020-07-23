context("test_assets_download")

testthat::test_that("assets download", {
  vcr::use_cassette("assets_download", {
    # skip cran check test
    testthat::skip_on_cran()

    # error - zero items
     testthat::expect_error(
      rstac::stac_search(
        url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        collections = "MOD13Q1",
        datetime    = "2019-09-01/2019-11-01",
        bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
        limit       = 0) %>%
      rstac::get_request(.) %>%
      rstac::assets_download(., assets_name = c("blue", "evi")))

    # error - given another object
     testthat::expect_error(
       rstac::stac(
         url   = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
       rstac::get_request(.) %>%
       rstac::assets_download(., assets_name = c("blue", "evi")))

   # error - wrong path
   testthat::expect_error(
     rstac::stac_search(
       url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
       collections = "MOD13Q1",
       datetime    = "2019-09-01/2019-11-01",
       bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
       limit       = 1) %>%
     rstac::get_request(.) %>%
     rstac::assets_download(., assets_name = c("thumbnail"),
                            output_dir = "./my-dir/"))

    # ok - return a stac_items
    testthat::expect_equal(
      object = class(
        rstac::stac_search(
          url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
          collections = "MOD13Q1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
          limit       = 1) %>%
        rstac::get_request(.) %>%
        rstac::assets_download(., assets_name = c("thumbnail"))),
      expected = "stac_items"
    )
  })
  vcr::use_cassette("assets_item", {
    testthat::skip_on_cran()

    # error - given another object
    testthat::expect_error(
     rstac::stac(url   = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
     rstac::get_request(.) %>%
     rstac::items_assets(.))

    # error - zero items
    testthat::expect_error(
      rstac::stac_search(
        url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        collections = "MOD13Q1",
        datetime    = "2019-09-01/2019-11-01",
        bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
        limit       = 0) %>%
      rstac::get_request(.) %>%
      rstac::items_assets(.))

    # error - wrong object
    testthat::expect_error(
      rstac::stac_search(
        url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        collections = "MOD13Q1",
        datetime    = "2019-09-01/2019-11-01",
        bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
        limit       = 0) %>%
      rstac::get_request(.) %>% list(.) %>%
      rstac::items_assets(.))

    # ok - return a stac_items
    testthat::expect_equal(
      object = class(
        rstac::stac_search(
          url         = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
          collections = "MOD13Q1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-55.16335, -4.26325, -49.31739, -1.18355),
          limit       = 1) %>%
        rstac::get_request(.) %>%
        rstac::items_assets(.)),
      expected = "list"
    )
  })
})
