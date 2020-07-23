context("test_stac_search")

testthat::test_that("stac new object", {
  vcr::use_cassette("stac_new_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # check object class of stac
    testthat::expect_equal(
      object   = class(rstac::stac(
        url    = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0")),
      expected = "stac"
    )

    # check object class of stac_search
    testthat::expect_equal(
      object   = class(rstac::stac_search(
        url    = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        bbox   = c(-55.16335, -4.26325, -49.31739, -1.18355))),
      expected = "stac"
    )

    # check object class of stac collections
    s_col <- rstac::stac_collections(
      url  = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0")

    testthat::expect_equal(
      object   = s_col$expected_responses$post$responses$`200`$`application/json`,
      expected = ""
    )

    # check object class of stac collections
    testthat::expect_equal(
      object   = class(s_col),
      expected = "stac"
    )

    # check object class of stac collections
    s_colid <- rstac::stac_collections(
      url           = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
      collection_id = "CB_64_16D_STK")

    testthat::expect_equal(
      object   = s_colid$expected_responses$post$responses$`200`$`application/json`,
      expected = "stac_collection"
    )

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

    ########### Check errors in fixed date time
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
        datetime = "20-2012-20T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "20-02-2012")
    )

    # check fixed date time
    testthat::expect_equal(
      object     = class(rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2019-02-12T00:00:00Z")),
      expected   = "stac"
    )

    ########### Check errors in closed date time
    # check closed date time
    testthat::expect_error(
      rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2019-02-12T00:00:00Z/2018-03-18T12:31:12Z")
    )

    # check fixed date time
    testthat::expect_equal(
      object     = class(rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z")),
      expected   = "stac"
    )

    ########### Check errors in interval date time
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

    # check fixed date time
    testthat::expect_equal(
      object     = class(rstac::stac_search(
        url      = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
        datetime = "2018-03-20T12:31:12Z/..")),
      expected   = "stac"
    )
  })
})
