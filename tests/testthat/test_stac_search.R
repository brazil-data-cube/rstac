context("test_stac_search")
# TODO: change name of context and cassete

testthat::test_that("stac new object", {
  vcr::use_cassette("stac_new_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # Error when creating the stac object by parameter bbox
    testthat::expect_error(
      rstac::stac_search(
        url  = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        bbox = c(-55.16335, -4.26325, -49.31739))
      )

    # providing the bbox and intersects parameters
    testthat::expect_warning(
      rstac::stac_search(
        url        = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        bbox       = c(-55.16335, -4.26325, -49.31739, -1.18355),
        intersects = toJSON(list(type = "Point",
                                 "coordinates" = c(-55.16335, -4.26325),
                                 "bbox" = c(-55.16335,
                                            -4.26325,
                                            -49.31739,
                                            -1.18355)))
        )
    )

    # check object class
    testthat::expect_equal(
      object   = class(rstac::stac_search(
        url    = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        bbox   = c(-55.16335, -4.26325, -49.31739, -1.18355))),
      expected = "stac"
      )

    # check fixed date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "20-02-2012T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "20-02-2012")
    )

    # check closed date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2019-02-12T00:00:00Z/2018-03-18T12:31:12Z")
    )

    # check interval date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "./2018-03-18T12:31:12Z")
    )

    # check interval date time  - wrong pattern
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "../1008-03-2018T12:31:12Z")
    )

    # check interval date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2018-03-18T12:31:12Z/.")
    )

    # check interval date time - wrong pattern
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "20-03-2018T12:31:12Z/..")
    )
  })
})
