context("test_stac_search")

testthat::test_that("stack get response", {
  vcr::use_cassette("get_tests", {
  # skip cran check test
  testthat::skip_on_cran()

  # error in bbox
  #testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
  #                                   bbox = c(-55.16335, -4.26325, -49.31739)))
  # datetime fixed - not error
  #testthat::expect(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
  #                             datetime = "2018-02-12T23:20:50Z"))
  # datetime closed
  #testthat::expect(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
  #                             datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"))
  # datetime opened
  #testthat::expect(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
  #                             datetime = "../2018-03-18T12:31:12Z"))

  #testthat::expect(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
  #                             datetime = "2018-02-12T00:00:00Z/.."))
  })
})

