testthat::test_that("internals functions", {

  # skip cran check test
  testthat::skip_on_cran()

  stac_obj <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/")

  # check_subclass object
  testthat::expect_null(
    object = check_subclass(stac_obj, subclasses = c("stac"))
  )

  # check for query for wrong verb
  testthat::expect_error(
    check_query_verb(q = stac_obj, verbs = c("DDDDD"))
  )

  testthat::expect_equal(
    object = subclass(before_request(stac_obj)),
    expected = "stac"
  )

  testthat::expect_error(
    object = {
      mock_obj <- stac_obj
      class(mock_obj) <- "RSTACQuery"
      after_response(mock_obj, res = NULL)
    }
  )

  testthat::expect_error(
    object = {
      mock_obj <- stac_obj
      class(mock_obj) <- "RSTACQuery"
      endpoint(mock_obj)
    }
  )

  # subclass object
  testthat::expect_equal(
    object = subclass(stac_obj),
    expected = "stac"
  )

  # .error function
  testthat::expect_error(
    .error("error function")
  )

  # .warning function
  testthat::expect_warning(
    .warning("warning function")
  )

  # .message function
  testthat::expect_message(
    .message("message function")
  )

  testthat::expect_error(
    .make_url("aaa", params = list(1))
  )
})

# testthat::test_that("internals format functions", {
#
#   # skip cran check test
#   testthat::skip_on_cran()
#
#   stac_obj <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/")
#
#
# })
