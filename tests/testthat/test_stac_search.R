context("test_stac_search")

testthat::test_that("stack get response", {
  vcr::use_cassette("default_method", {
  # skip cran check test
  testthat::skip_on_cran()

  # error in bbox
  testthat::expect_error(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     bbox = c(-55.16335, -4.26325, -49.31739)))


  testthat::expect_type(stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
                                     datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"))
  })
})

# stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#             bbox = c(-55.16335, -4.26325, -49.31739))


#' '   \item A date-time: \code{"2018-02-12T23:20:50Z"}
#' #'   \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#' #'   \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#' #'     \code{"../2018-03-18T12:31:12Z"}
#
# stac_search_new(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0/stac/search",
#                 query = list(bbox = c(-55.16335, -4.26325, -49.31739) ))
