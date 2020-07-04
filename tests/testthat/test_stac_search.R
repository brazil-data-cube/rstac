context("test_stac_search")

testthat::test_that("stack get response", {
  vcr::use_cassette("get_tests", {
  # skip cran check test
  testthat::skip_on_cran()

  #stac_new("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/stac")

  # error in bbox
  testthat::expect_error(stac_search_new(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/stac/search",
                                     query = list(bbox = c(-55.16335, -4.26325, -49.31739))))

  #testthat::expect_error(stac_search_new(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/stac/search",
  #                                           query = list(datetime = c("12/10/2019"))))

  # datetime fixed
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                    datetime = "2018-02-12T23:20:50Z"))

  # datetime interval - the error is normal here?
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     datetime = c("2018-02-12T00:00:00Z/2018-03-18T12:31:12Z")))
  # datetime interval - the error is normal here?
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     datetime = c("2018-02-12T00:00:00Z/..")))
  # datetime interval - the error is normal here?
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     datetime = c("../2018-03-18T12:31:12Z")))

  # searching b y
  # datetime interval - the error is normal here?
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     collections = "MOD13Q1",
                                     datetime = c("../2018-03-18T12:31:12Z")))


  })
})

