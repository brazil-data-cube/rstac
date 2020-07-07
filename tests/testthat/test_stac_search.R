context("test_stac_search")

testthat::test_that("stack get response", {
  vcr::use_cassette("get_tests", {
  # skip cran check test
  testthat::skip_on_cran()

  # error in bbox
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     bbox = c(-55.16335, -4.26325, -49.31739)))

  # datetime fixed
  res_stac <- stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
              datetime = "2018-02-12T23:20:50Z")

  testthat::expect_equal(res_stac$status_code, 200)

  # TODO: wrong date time interval

  # TODO: provided bbox and intersects in the same query

  })
})

