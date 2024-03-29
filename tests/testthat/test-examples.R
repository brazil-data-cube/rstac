testthat::test_that("examples rstac", {
    # skip cran check test
    testthat::skip_on_cran()

    #### tests in objects ####

    # test collections - /collections/
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections() %>%
        get_request(),
      class = c("doc_collections", "rstac_doc"))

    # test collections items - /collections/{collection_id}
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections(collection_id = "CB4-16D-2") %>%
        get_request(),
      class = c("doc_collection", "doc_catalog", "rstac_doc"))

    # test items collection - /collections/{collection_id}/items
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4-16D-2") %>%
        items(bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        get_request(),
      class = c("doc_items", "rstac_doc"))

    # test items collection - /search/
    testthat::expect_s3_class(
      stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4-16D-2",
                    bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        get_request(),
      class = c("doc_items", "rstac_doc"))

    # test items collection - /search/
    testthat::expect_s3_class(
      stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4-16D-2",
                    bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
        post_request(),
      class = c("doc_items", "rstac_doc"))

    # test stac item - /collections/{collection_id}/items/{feature_id}
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4-16D-2") %>%
        items("CB4-16D_V2_000002_20230509") %>%
        get_request(),
      class = c("doc_item", "rstac_doc"))

    # test stac catalog - /
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request(),
      class = c("doc_catalog", "rstac_doc"))

    #### tests in extensions ####

    # test extension query
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
              stac_search(collections = "CB4-16D-2") %>%
              ext_query("bdc:tile" == "021027") %>%
              post_request(),
      class = c("doc_item", "rstac_doc"))

    #### tests in items ####

    # test items_fetch
    testthat::expect_s3_class(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "LCC_C4_64_1M_STK_GO_PA-SPC-AC-NA-1",
                    limit = 500) %>%
        get_request() %>%
        items_fetch(),
      class = c("doc_items", "rstac_doc"))

    # test item_length
    testthat::expect_equal(
      object = stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(collections = "CB4-16D-2",
                    limit = 10) %>%
        get_request() %>%
        items_length(),
      expected = 10)

    # test items_reap
    testthat::expect_equal(
      object = typeof(
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "CB4-16D-2",
                      limit = 10,
                      datetime = "2017-08-01/2018-03-01") %>%
          get_request() %>%
          items_reap(field = c("properties", "datetime"))),
      expected = "character")

    # test items_fields
    testthat::expect_equal(
      object = typeof(
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "CB4-16D-2",
                      limit = 10,
                      datetime = "2017-08-01/2018-03-01") %>%
          get_request() %>%
          items_fields(field = "properties")),
      expected = "character")
})
