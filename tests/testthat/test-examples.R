context("tests_examples_0.9.0")

testthat::test_that("examples rstac", {
  vcr::use_cassette("tests_examples",{
    # skip cran check test
    testthat::skip_on_cran()

    #### tests in objects ####

    # test collections - /collections/
    testthat::expect_s3_class(
      object = rstac::stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        rstac::collections() %>%
        rstac::get_request(),
      class = c("STACCollectionList", "RSTACDocument"))

    # test collections items - /collections/{collection_id}
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        collections(collection_id = "CB4_64_16D_STK-1") %>%
        get_request(),
      class = c("STACCollection", "STACCatalog", "RSTACDocument"))

    # test items collection - /collections/{collection_id}/items
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64_16D_STK-1") %>%
        items(bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        get_request(),
      class = c("STACItemCollection", "RSTACDocument"))

    # test items collection - /search/
    testthat::expect_s3_class(
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        get_request(),
      class = c("STACItemCollection", "RSTACDocument"))

    # test items collection - /search/
    testthat::expect_s3_class(
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        post_request(),
      class = c("STACItemCollection", "RSTACDocument"))

    # test stac item - /collections/{collection_id}/items/{feature_id}
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64_16D_STK-1") %>%
        items("CB4_64_16D_STK_v001_022024_2020-07-11_2020-07-26") %>%
        get_request(),
      class = c("STACItem", "RSTACDocument"))

    # test stac catalog - /
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request(),
      class = c("STACCatalog", "RSTACDocument"))


    #### tests in extensions ####

    # test extension query
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
              stac_search(collections = "CB4_64_16D_STK-1") %>%
              ext_query("bdc:tile" == "021027") %>%
              post_request(),
      class = c("STACItem", "RSTACDocument"))

    #### tests in items ####

    # test items_fetch
    testthat::expect_s3_class(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    bbox = c(-47.02148, -12.98314, -42.53906, -17.35063),
                    limit = 500) %>%
        get_request() %>%
        items_fetch(),
      class = c("STACItemCollection", "RSTACDocument"))

    # test item_length
    testthat::expect_equal(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    limit = 500) %>%
        get_request() %>%
        items_length(),
      expected = 500)

    # test item_length
    testthat::expect_equal(
      object = stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    limit = 500) %>%
        get_request() %>%
        items_matched(),
      expected = 1785)

    # test items_group
    testthat::expect_equal(
      object = typeof(
        stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4_64_16D_STK-1",
                    limit = 100,
                    bbox = c(-48.206,-14.195,-45.067,-12.272),
                    datetime = "2017-08-01/2018-03-01") %>%
        get_request() %>% items_fetch(progress = FALSE) %>%
        items_group(field = c("properties", "bdc:tiles"))),
      expected = "list")

    # test items_reap
    testthat::expect_equal(
      object = typeof(
        stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "CB4_64_16D_STK-1",
                      limit = 10,
                      datetime = "2017-08-01/2018-03-01") %>%
          get_request() %>%
          items_reap(field = c("properties", "datetime"))),
      expected = "character")

    # test items_fields
    testthat::expect_equal(
      object = typeof(
        stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "CB4_64_16D_STK-1",
                      limit = 10,
                      datetime = "2017-08-01/2018-03-01") %>%
          get_request() %>%
          items_fields(field = c("properties"))),
      expected = "character")

    # test assets_list
    testthat::expect_equal(
      object = typeof(
        stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "CB4_64_16D_STK-1",
                      limit = 10,
                      datetime = "2017-08-01/2018-03-01") %>%
          get_request() %>%
          assets_list(assets_names = c("EVI", "NDVI"))),
      expected = "list")
  })
})
