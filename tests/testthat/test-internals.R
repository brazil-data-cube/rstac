testthat::test_that("internals functions", {
  # skip cran check test
  testthat::skip_on_cran()

  stac_obj <- stac("https://data.inpe.br/bdc/stac/v1/")

  # check_query object
  testthat::expect_null(
    object = check_query(stac_obj, classes = c("stac"))
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
      class(mock_obj) <- "rstac_query"
      after_response(mock_obj, res = NULL)
    }
  )

  testthat::expect_error(
    object = {
      mock_obj <- stac_obj
      class(mock_obj) <- "rstac_query"
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
})

testthat::test_that("internals response", {
    bdc_catalog <- httr::GET("https://data.inpe.br/bdc/stac/v1/")
    bdc_wrong_path <- httr::GET("https://data.inpe.br/bdc/stac/v1/dddd")

    testthat::expect_error(
      content_response(res = bdc_catalog,
                       status_codes = 300,
                       content_types = "application/.*json",
                       key_message = c("message", "description", "detail"))
    )

    testthat::expect_error(
      content_response(res = bdc_wrong_path,
                       status_codes = 300,
                       content_types = "application/.*json",
                       key_message = c("message", "description", "detail"))
    )
})
