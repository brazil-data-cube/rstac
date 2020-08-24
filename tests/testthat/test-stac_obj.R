context("test_stac_obj")

testthat::test_that("stac search object", {
  vcr::use_cassette("stac_search_obj",{
    # skip cran check test
    testthat::skip_on_cran()

    # check object class of stac_search
    testthat::expect_equal(
      object  = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                      -41.6341, -11.91345))),
      expected = c("search", "stac")
    )

    # check GET request from stac_search object
    testthat::expect_equal(
      object   = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                      -41.6341, -11.91345)) %>%
          get_request()),
      expected = "stac_item_collection"
    )

    testthat::expect_equal(
      object   = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2018-01-01/..") %>%
          rstac::post_request()),
      expected = "stac_item_collection"
    )

    # Check extensions ---------------------------------------------------------

    # check extension query - wrong contruction
    testthat::expect_error(
      object = stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
        extension_query("bdc:tile" == "022024") %>%
        rstac::stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong contruction
    testthat::expect_error(
      object = stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        extension_query("bdc:tile" == "022024") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong query
    testthat::expect_error(
      object = stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        extension_query("bdc:tile" == "022024", teste = "teste") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong parameter
    testthat::expect_error(
      object = stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        extension_query("bdc:tile" + "022024") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong request
    testthat::expect_error(
      object = stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        extension_query("bdc:tile" == "022024") %>%
        get_request()
    )

    stac_search_obj <-
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
      rstac::stac_search(datetime = "2018-01-01/..") %>%
      extension_query("bdc:tile" == "022024")

    # expected class
    testthat::expect_equal(
      object   =  class(stac_search_obj),
      expected = c("ext_query", "stac")
    )

    # expect http method
    testthat::expect_true(
      object = is.null(stac_search_obj$expected_responses[["get"]]))

    # expect class
    testthat::expect_equal(
      object = class(stac_search_obj %>% post_request()),
      expect = "stac_item_collection"
    )

    # Check print function------------------------------------------------------

    # show only one object
    testthat::expect_output(
      object   = print(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2018-01-01/..", limit = 10) %>%
          get_request(), n = 1),
      regexp = "> … with 9 more feature\\(s\\)"
    )

    # show all of them
    testthat::expect_output(
      object   = print(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2018-01-01/..", limit = 10) %>%
          get_request(), n = 10),
      regexp = "numberMatched: "
    )

    # error when setting itens equal or less than 0
    testthat::expect_output(
      object = print(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2018-01-01/..", limit = 10) %>%
          get_request(), n = 0),
      regexp = "> … with 10 more feature\\(s\\)")

    # Error when creating the stac object by parameter bbox
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(bbox = c(-55.16335, -4.26325, -49.31739))
    )

    # Check errors in fixed date time-------------------------------------------
    # check fixed date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "20-02-2012T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "20-2012-20T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "20-02-2012")
    )

    # check fixed date time
    testthat::expect_equal(
      object     = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2019-02-12T00:00:00Z")),
      expected   = c("search", "stac")
    )

    # Check errors in closed date time------------------------------------------
    # check closed date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(
          datetime = "2019-02-12T00:00:00Z/2018-03-18T12:31:12Z")
    )

    # check fixed date time
    testthat::expect_equal(
      object = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(
            datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z")),
      expected = c("search", "stac")
    )

    # Check errors in interval date time----------------------------------------
    # check interval date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "./2018-03-18T12:31:12Z")
    )

    # check interval date time  - wrong pattern
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "../1008-03-2018T12:31:12Z")
    )

    # check interval date time
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::search(datetime = "2018-03-18T12:31:12Z/.")
    )

    # check interval date time - wrong pattern
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        rstac::stac_search(datetime = "20-03-2018T12:31:12Z/..")
    )

    # check fixed date time
    testthat::expect_equal(
      object   = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          rstac::stac_search(datetime = "2018-03-20T12:31:12Z/..")),
      expected = c("search", "stac")
    )
  })
})

testthat::test_that("stac collection object", {
  vcr::use_cassette("stac_collection_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # stac_collections----------------------------------------------------------
    # check object class of stac collections
    s_col <- rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                         force_version = "0.9.0") %>%
      rstac::collections()

    testthat::expect_equal(
      object   =  s_col$endpoint,
      expected = "/collections"
    )

    # check object class of stac collections
    testthat::expect_equal(
      object   = class(s_col),
      expected = c("collections", "stac")
    )

    # check object class of stac collections
    s_colid <-
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
      rstac::collections(collection_id = "CB4_64_16D_STK-1")

    testthat::expect_equal(
      object   = s_colid$endpoint,
      expected = "/collections/CB4_64_16D_STK-1"
    )

    # check print stac object
    testthat::expect_output(
      object = print(s_colid),
      regexp = "### STAC"
    )

    # check request from stac collections object
    testthat::expect_equal(
      object   = class(s_colid %>% get_request()),
      expected = "stac_collection"
    )

    # check print stac_collection object
    testthat::expect_output(
      object   = print((s_colid %>% get_request()), n = 1),
      regexp   = "> … with 4 more links"
    )

    # check print stac_collection object
    testthat::expect_output(
      object   = print((s_colid %>% get_request())),
      regexp   = "### STAC Collection"
    )
  })
})

testthat::test_that("stac object", {
  vcr::use_cassette("stac_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # check object class of stac
    testthat::expect_equal(
      object   = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0")),
      expected = "stac"
    )

    # check request from stac object
    testthat::expect_equal(
      object   = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                    force_version = "0.9.0") %>%
          get_request()),
      expected = "stac_catalog"
    )

    # check print stac_collection object
    testthat::expect_output(
      object   = print(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                    force_version = "0.9.0") %>%
          get_request()),
      regexp   = "### STAC Catalog"
    )
  })
})

testthat::test_that("stac item object", {
  vcr::use_cassette("stac_item_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # not provide collection id
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                  force_version = "0.9.0") %>%
        items(bbox = c(-48.19039, -16.00871, -41.6341, -11.91345))
    )

    # wrong date
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                  force_version = "0.9.0") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          datetime      = "2018-02-01/.",
          collection_id = "CB4_64-1")
    )

    # wrong bbox
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                  force_version = "0.9.0") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          datetime      = "2018-02-01/..",
          collection_id = "CB4_64-1")
    )

    # stac_collection_list object
    testthat::expect_equal(
      object = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/",
                    force_version = "0.9.0") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..")),
      expected = c("items", "stac")
    )

    # stac_item object
    testthat::expect_equal(
      object = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                    force_version = "0.9.0") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..",
            feature_id    = "MOD13Q1.A2019241.h13v09.006.2019262164754")),
      expected = c("items", "stac")
    )

    # test request for stac_collection_list
    testthat::expect_equal(
      object = class(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                    force_version = "0.9.0") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..",
            feature_id    = "CB4_64_022024_2019-12-31") %>%
          get_request()),
      expected = "stac_item"
    )

    # output test
    testthat::expect_output(
      object   = print(
        rstac::stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac",
                    force_version = "0.9.0") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..",
            feature_id    = "CB4_64_022024_2019-12-31") %>%
          get_request()),
      regexp   = "### STAC Item"
    )
  })
})
