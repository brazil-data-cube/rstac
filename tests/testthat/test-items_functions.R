context("test_items_functions")

testthat::test_that("items functions", {
  vcr::use_cassette("items_functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                       force_version = "0.9.0") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        bbox  = c(-48.19039, -16.00871, -41.6341, -11.91345),
        limit = 500) %>%
      get_request(.)

    # items_fetch---------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_fetch(list(res)))

   # ok - stac_collection_list object
    testthat::expect_equal(
      object   = class(items_fetch(res)),
      expected = "stac_item_collection")

    # items_length--------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_length(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(items_length(res)))

    # items_matched-------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_matched(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(items_matched(res)))
  })
})
