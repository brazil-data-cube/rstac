context("test_items_functions")

testthat::test_that("items functions", {
  vcr::use_cassette("items_functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac_search(
                  url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                  collections = "MOD13Q1",
                  bbox  = c(-55.16335, -4.26325, -49.31739, -1.18355),
                  limit = 500) %>%
          rstac::stac_request(.)

    # items_fetch---------------------------------------------------------------
    # error - given another object
    testthat::expect_error(rstac::items_fetch(list(res)))

   # ok - stac_items object
    testthat::expect_equal(
      object   = class(rstac::items_fetch(res)),
      expected = "stac_items")

    # items_length--------------------------------------------------------------
    # error - given another object
    testthat::expect_error(rstac::items_length(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(rstac::items_length(res)))

    # items_matched-------------------------------------------------------------
    # error - given another object
    testthat::expect_error(rstac::items_matched(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(rstac::items_matched(res)))
  })
})
