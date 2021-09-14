context("test_stac_obj")

testthat::test_that("stac search object", {
  vcr::use_cassette("stac_search_obj", {
    # skip cran check test
    testthat::skip_on_cran()


    # Error when creating the stac object by parameter bbox
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(bbox = c(-55.16335, -4.26325, -49.31739))
    )

    # check object class of stac_search
    testthat::expect_equal(
      object  = class(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                      -41.6341, -11.91345))),
      expected = c("search", "RSTACQuery")
    )

    # check GET request from stac_search object
    testthat::expect_equal(
      object   = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(bbox = c(-48.19039, -16.00871,
                                      -41.6341, -11.91345)) %>%
          get_request()),
      expected = "STACItemCollection"
    )

    # Check extensions ---------------------------------------------------------

    # check extension query - wrong construction
    testthat::expect_error(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        ext_query("bdc:tile" == "022024") %>%
        rstac::stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong contruction
    testthat::expect_error(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/",
                    force_version = "0.9.0") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        ext_query("bdc:tile" == "022024") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong query
    testthat::expect_error(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        ext_query("bdc:tile" == "022024", test = "test") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong parameter
    testthat::expect_error(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        ext_query("bdc:tile" + "022024") %>%
        stac_search(datetime = "2018-01-01/..")
    )

    # check extension query - wrong request
    testthat::expect_error(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(datetime = "2018-01-01/..") %>%
        ext_query("bdc:tile" == "022024") %>%
        get_request()
    )

    stac_search_obj <-
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::stac_search() %>%
      ext_query("bdc:tile" == "022024")

    # expected class
    testthat::expect_equal(
      object   =  subclass(stac_search_obj),
      expected = c("ext_query")
    )

    # error in try to use get method for ext query
    testthat::expect_error(
      object = stac_search_obj %>% get_request()
    )

    # Check print function------------------------------------------------------

    # check object
    testthat::expect_output(
      object   = print(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(datetime = "2018-01-01/2018-07-01", limit = 10) %>%
          get_request(), n = 10),
      regexp = "###STACItemCollection"
    )


    # Check errors in fixed date time-------------------------------------------
    # check fixed date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "20-02-2012T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "20-2012-20T00:00:00Z")
    )

    # check fixed date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "20-02-2012")
    )

    # check fixed date time
    testthat::expect_equal(
      object     = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(datetime = "2019-02-12T00:00:00Z")),
      expected   = c("search")
    )

    # Check errors in closed date time------------------------------------------
    # check closed date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(
          datetime = "2019-02-12T00:00:00Z/2018-03-18T12:31:12Z")
    )

    # check fixed date time
    testthat::expect_equal(
      object = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(
            datetime = "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z")),
      expected = c("search")
    )

    # Check errors in interval date time----------------------------------------
    # check interval date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "./2018-03-18T12:31:12Z")
    )

    # check interval date time  - wrong pattern
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "../1008-03-2018T12:31:12Z")
    )

    # check interval date time
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::search(datetime = "2018-03-18T12:31:12Z/.")
    )

    # check interval date time - wrong pattern
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::stac_search(datetime = "20-03-2018T12:31:12Z/..")
    )

    # check fixed date time
    testthat::expect_equal(
      object   = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          rstac::stac_search(datetime = "2018-03-20T12:31:12Z/..")),
      expected = c("search")
    )
  })
})

testthat::test_that("stac collection object", {
  vcr::use_cassette("stac_collection_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # stac_collections----------------------------------------------------------
    # check object class of stac collections
    s_col <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::collections() %>%
      get_request()

    testthat::expect_equal(
      object   =  subclass(s_col),
      expected = "STACCollectionList"
    )

    testthat::expect_equal(
      object   =  stac_version(s_col),
      expected = "0.9.0"
    )

    # check print stac object
    testthat::expect_output(
      object = print(rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
                       rstac::collections()),
      regexp = "###RSTACQuery"
    )


    testthat::expect_equal(
      object   =  attributes(s_col)$query$endpoint,
      expected = "/collections"
    )

    # check object class of stac collections
    s_colid <-
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      rstac::collections(collection_id = "CB4_64_16D_STK-1")

    testthat::expect_null(
      object   = s_colid$endpoint
    )

    # check print stac object
    testthat::expect_output(
      object = print(s_colid),
      regexp = "- collection_id"
    )

    # check request from stac collections object
    testthat::expect_equal(
      object   = subclass(s_colid %>% get_request()),
      expected = "STACCollection"
    )

    # check print stac_collection object
    testthat::expect_output(
      object   = print((s_colid %>% get_request())),
      regexp   = "###STACCollection"
    )
  })
})

testthat::test_that("stac object", {
  vcr::use_cassette("stac_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # check object class of stac
    testthat::expect_equal(
      object   = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/")),
      expected = "stac"
    )

    # check request from stac object
    testthat::expect_equal(
      object   = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          get_request()),
      expected = "STACCatalog"
    )

    # check print stac_collection object
    testthat::expect_output(
      object   = print(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac") %>%
          get_request()),
      regexp   = "###STACCatalog"
    )
  })
})

testthat::test_that("stac item object", {
  vcr::use_cassette("stac_item_obj", {
    # skip cran check test
    testthat::skip_on_cran()

    # not provide collection id
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        items(bbox = c(-48.19039, -16.00871, -41.6341, -11.91345))
    )

    # wrong date
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        items(
          bbox       = c(-48.19039, -16.00871, -41.6341, -11.91345),
          datetime   = "2018-02-01/.",
          feature_id = "CB4_64-1")
    )

    # wrong bbox
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        items(
          bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
          datetime      = "2018-02-01/..",
          feature_id = "CB4_64-1")
    )

    # stac_collection_list object
    testthat::expect_equal(
      object = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/",
                    force_version = "0.9.0") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..")),
      expected = c("items")
    )

    # stac_item object
    testthat::expect_equal(
      object = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..",
            feature_id    = "MOD13Q1.A2019241.h13v09.006.2019262164754")),
      expected = c("item_id")
    )

    # stac_item object
    testthat::expect_equal(
      object = stac_version(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64-1") %>%
          items(
            bbox          = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit         = 10,
            datetime      = "2018-02-01/..",
            feature_id    = "MOD13Q1.A2019241.h13v09.006.2019262164754")),
      expected = "0.9.0"
    )

    # output test
    testthat::expect_output(
      object   = print(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items(
            bbox       = c(-48.19039, -16.00871, -41.6341, -11.91345),
            limit      = 10,
            datetime   = "2018-02-01/..",
            feature_id = "CB4_64_16D_STK_v001_021027_2020-07-11_2020-07-26") %>%
          get_request()),
      regexp   = "###STACItem"
    )
  })
})
