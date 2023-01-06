testthat::test_that("items functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 10) %>%
      get_request()

    res_bbox <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 1,
        datetime = "2017-01-01/2017-03-01",
        bbox = c(-52.5732, -12.5975, -51.4893, -11.6522)) %>%
      get_request()

    intersects_geojson <- list(
      type = "Polygon",
      coordinates = structure(c(-52.5732, -51.4893,
                                -51.4893, -52.5732,
                                -52.5732, -12.5975,
                                -12.5975, -11.6522,
                                -11.6522, -12.5975),
                              .Dim = c(1L, 5L, 2L))
    )

    res_geo <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 1,
        datetime = "2017-01-01/2017-03-01",
        intersects = intersects_geojson) %>%
      post_request()

    res_ext <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(collections = "CB4_64_16D_STK-1",
                  limit = 10) %>%
      ext_query("bdc:tile" %in% "022024") %>%
      post_request()

    item_stac <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections(collection_id = "CB4_64_16D_STK-1") %>%
      items(feature_id = "CB4_64_16D_STK_v001_019022_2021-02-02_2021-02-17") %>%
      get_request()

    items_ms <- stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
      stac_search(
        collections = "sentinel-2-l2a",
        datetime = "2020-01-01/2020-01-31",
        limit = 1) %>%
      post_request()

    # items_fetch---------------------------------------------------------------
    # error - given another object
    testthat::expect_error(items_fetch(list(res)))

    # ok - stac_collection_list object
    testthat::expect_equal(
      object = subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "LCC_C4_64_1M_STK_GO_PA-SPC-AC-NA-1",
                      limit = 500) %>%
          get_request(.) %>%
          items_fetch()),
      expected = "STACItemCollection"
    )

    testthat::expect_error(
      object = {
        mock_obj <- res_bbox
        mock_obj$context$matched <- 0
        items_fetch(mock_obj)
      }
    )

    testthat::expect_equal(
      object = subclass(
        suppressWarnings(
          stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
            stac_search(collections = "io-lulc", limit = 1) %>%
            ext_query("io:tile_id" %in% "60W") %>%
            post_request() %>%
            items_fetch())),
      expected = "STACItemCollection"
    )

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
    testthat::expect_length(items_assets(res), n = 11)

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

    # ok - return a null
    testthat::expect_null(suppressWarnings(items_matched(items_ms)))

    # empty vector for invalid field
    testthat::expect_length(
      items_matched(items_ms, matched_field = "1232"),
      n = 0
    )

    # invalid field
    testthat::expect_warning(
      items_matched(items_ms, matched_field = FALSE)
    )

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
      expected = "character"
    )

    testthat::expect_message(
      items_assets(res, simplify = FALSE),
      regexp = "deprecated"
    )

    testthat::expect_equal(
      object = class(items_assets(item_stac)),
      expected = "character"
    )

    # items_next----------------------------------------------------------------
    testthat::expect_s3_class(
      object = items_next(item_stac),
      class = "STACItem"
    )

    testthat::expect_s3_class(
      object = items_next(res_geo),
      class = "STACItemCollection"
    )

    testthat::expect_s3_class(
      object = items_next(res_bbox),
      class = "STACItemCollection"
    )

    testthat::expect_s3_class(
      object = items_next(res),
      class = "STACItemCollection"
    )

    testthat::expect_s3_class(
      object = items_next(res_ext),
      class = "STACItemCollection"
    )

    testthat::expect_equal(
      object = items_length(items_next(res)),
      expected = 20
    )

    testthat::expect_error(
      object = {
        mock_obj <- res_geo
        attributes(mock_obj)$query <- list(NULL)

        items_next(mock_obj)
      }
    )

    # items_reap----------------------------------------------------------------
    # STACItemCollection
    testthat::expect_equal(
      object = class(items_reap(item_stac, field = c("properties", "datetime"))),
      expected = "character"
    )

    testthat::expect_length(
      object = items_reap(item_stac, field = c("properties", "datetime")),
      n = 1
    )

    testthat::expect_error(items_reap(item_stac, FALSE))
    testthat::expect_error(items_reap(item_stac, FALSE, field = FALSE))

    testthat::expect_equal(
      object = subclass(items_reap(item_stac)),
      expected = "STACItem"
    )

    # STACItemCollection
    testthat::expect_equal(
      object = class(items_reap(res, field = c("properties", "datetime"))),
      expected = "character"
    )

    testthat::expect_length(
      object = items_reap(res, field = c("properties", "datetime")),
      n = 10
    )

    copy_res <- res
    copy_res$features <- NULL
    testthat::expect_null(items_reap(copy_res))

    testthat::expect_error(items_reap(res, FALSE))
    testthat::expect_error(items_reap(res, FALSE, field = FALSE))

    testthat::expect_equal(
      object = class(items_reap(res)),
      expected = "list"
    )
})
