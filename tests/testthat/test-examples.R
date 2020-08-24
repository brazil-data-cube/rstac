context("tests_examples_0.8.1")

testthat::test_that("examples rstac", {
  vcr::use_cassette("tests_examples",{
    # skip cran check test
    testthat::skip_on_cran()

    #### tests in objects ####

    # test collections - /collections/
    testthat::expect_s3_class(
      object = rstac::stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                           force_version = "0.8.1") %>%
        rstac::collections() %>%
        rstac::get_request(),
      class = "stac_collection_list")

    # test collections items - /collections/{collection_id}
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        collections(collection_id = "MOD13Q1") %>%
        get_request(),
      class = "stac_collection")

    # test items collection - /collections/{collection_id}/items
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        collections("MOD13Q1") %>%
        items(bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
        get_request(),
      class = "stac_item_collection")

    # test items collection - /search/
    testthat::expect_s3_class(
      stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
           force_version = "0.8.1") %>%
        stac_search(collections = "MOD13Q1",
                    bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
        get_request(),
      class = "stac_item_collection")

    # test items collection - /search/
    testthat::expect_s3_class(
      stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
           force_version = "0.8.1") %>%
        stac_search(collections = "MOD13Q1",
                    bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
        post_request(),
      class = "stac_item_collection")

    # test stac item - /collections/{collection_id}/items/{feature_id}
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        collections("MOD13Q1") %>%
        items("MOD13Q1.A2019241.h13v09.006.2019262164754") %>%
        get_request(),
      class = "stac_item")

    # test stac catalog - /
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        get_request(),
      class = "stac_catalog")

    #### tests in items ####

    # test items_fetch
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        stac_search(collections = "MOD13Q1",
                    bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
                    limit = 500) %>%
        get_request() %>%
        items_fetch(),
      class = "stac_item_collection")

    # test item_length
    testthat::expect_equal(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        stac_search(collections = "MOD13Q1",
                    bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
                    limit = 500) %>%
        get_request() %>%
        items_length(),
      expected = 500)

    # test item_length
    testthat::expect_equal(
      object = stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
                    force_version = "0.8.1") %>%
        stac_search(collections = "MOD13Q1",
                    bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
                    limit = 500) %>%
        get_request() %>%
        items_matched(),
      expected = 908)
  })
})
