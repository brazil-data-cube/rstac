testthat::test_that("internals functions", {

  # skip cran check test
  testthat::skip_on_cran()

  stac_obj <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/")

  # check_subclass object
  testthat::expect_null(
    object = check_subclass(stac_obj, subclasses = c("stac"))
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
})
