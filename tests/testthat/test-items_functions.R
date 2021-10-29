testthat::test_that("items functions", {
  vcr::use_cassette("items_functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 10) %>%
      get_request(.)

    item_stac <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections(collection_id = "CB4_64_16D_STK-1") %>%
      items(feature_id = "CB4_64_16D_STK_v001_019022_2021-02-02_2021-02-17") %>%
      get_request(.)

    # items_fetch---------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_fetch(list(res)))

    # ok - stac_collection_list object
    testthat::expect_equal(
      object   = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "LCC_C4_64_1M_STK_GO_PA-SPC-AC-NA-1",
                      limit = 500) %>%
          get_request(.) %>%
          items_fetch()),
      expected = "STACItemCollection")

    # items_length--------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_length(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(items_length(res)))

    # items_datetime------------------------------------------------------------
    # STACItemCollection
    testthat::expect_length(items_datetime(res), n = 10)

    # STACItem
    testthat::expect_vector(items_datetime(item_stac), ptype = character())

    # provide wrong object
    testthat::expect_error(
      object = items_datetime(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections(collection_id = "CB4_64_16D_STK-1") %>%
          get_request()
      )
    )

    # items_bbox----------------------------------------------------------------
    # STACItemCollection
    testthat::expect_length(items_bbox(res), n = 10)

    # STACItem
    testthat::expect_vector(items_bbox(item_stac), ptype = double())

    # provide wrong object
    testthat::expect_error(
      object = items_bbox(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections(collection_id = "CB4_64_16D_STK-1") %>%
          get_request()
      )
    )

    # items_assets---------------------------------------------------------------
    # STACItemCollection
    testthat::expect_length(items_assets(res), n = 10)

    # STACItem
    testthat::expect_vector(items_assets(item_stac), ptype = character())

    # provide wrong object
    testthat::expect_error(
      object = items_assets(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections(collection_id = "CB4_64_16D_STK-1") %>%
          get_request()
      )
    )

    # items_matched-------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_matched(list(res)))

    # ok - return a numeric
    testthat::expect_true(is.numeric(items_matched(res)))

    # items_filter--------------------------------------------------------------
    testthat::expect_s3_class(
      object = items_filter(
        res, filter_fn = function(x) {x[["eo:cloud_cover"]] < 10}
      ),
      class = "STACItemCollection"
    )

    testthat::expect_s3_class(
      object = items_filter(res, `eo:cloud_cover` < 10),
      class = "STACItemCollection"
    )

    testthat::expect_s3_class(
      object = items_filter(res),
      class = "STACItemCollection"
    )

    testthat::expect_error(
      object = items_filter(item_stac, `eo:cloud_cover` < 10)
    )

    testthat::expect_error(
      object = items_filter(res, list(`eo:cloud_cover` < 10))
    )

    # items_assets--------------------------------------------------------------
    testthat::expect_equal(
      object = class(items_assets(res)),
      expected = "list"
    )

    testthat::expect_equal(
      object = class(items_assets(item_stac)),
      expected = "character"
    )

    # items_bands---------------------------------------------------------------
    testthat::expect_equal(
      object = class(items_bands(res)),
      expected = "list"
    )

    testthat::expect_equal(
      object = class(items_bands(item_stac)),
      expected = "character"
    )

    # items_next----------------------------------------------------------------
    testthat::expect_s3_class(
      object = items_next(item_stac),
      class = "STACItem"
    )

    testthat::expect_s3_class(
      object = items_next(res),
      class = "STACItemCollection"
    )

    testthat::expect_equal(
      object = items_length(items_next(res)),
      expected = 20
    )
  })
})
