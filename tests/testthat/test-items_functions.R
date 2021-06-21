context("test_items_functions")

testthat::test_that("items functions", {
  vcr::use_cassette("items_functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 500) %>%
      get_request(.)

    # items_fetch---------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_fetch(list(res)))

   # ok - stac_collection_list object
    testthat::expect_equal(
      object   = subclass(items_fetch(res)),
      expected = "STACItemCollection")

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
