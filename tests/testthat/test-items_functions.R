testthat::test_that("items functions", {
  vcr::use_cassette("items_functions", {
    # skip cran check test
    testthat::skip_on_cran()

    res <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        limit = 10) %>%
      get_request(.)

    res_ext <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(collections = "CB4_64_16D_STK-1",
                  limit = 10) %>%
      ext_query("bdc:tile" %in% "022024") %>%
      post_request(.)

    item_stac <- rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections(collection_id = "CB4_64_16D_STK-1") %>%
      items(feature_id = "CB4_64_16D_STK_v001_019022_2021-02-02_2021-02-17") %>%
      get_request(.)

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

    testthat::expect_equal(
      object   = rstac::subclass(
        rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(collections = "S2-SEN2COR_10_16D_STK-1", limit = 10) %>%
          ext_query("bdc:tile" %in% "078086") %>%
          post_request() %>%
          items_fetch()),
      expected = "STACItemCollection"
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

    testthat::expect_warning(
      object = stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
        stac_search(collections = "io-lulc", limit = 1) %>%
        ext_query("io:tile_id" %in% "60W") %>%
        post_request() %>%
        items_fetch()
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
      expected = "list"
    )

    testthat::expect_equal(
      object = class(items_assets(item_stac)),
      expected = "character"
    )

    # items_bands---------------------------------------------------------------
    testthat::expect_equal(
      object = class(suppressMessages(items_bands(item_stac))),
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

    testthat::expect_s3_class(
      object = items_next(res_ext),
      class = "STACItemCollection"
    )

    testthat::expect_equal(
      object = items_length(items_next(res)),
      expected = 20
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

    # items_apply---------------------------------------------------------------
    testthat::expect_equal(
      object = subclass(items_apply(res)),
      expected = "STACItemCollection"
    )

    testthat::expect_equal(
      object = subclass(items_apply(res)),
      expected = "STACItemCollection"
    )

    testthat::expect_error(
      items_apply(res,
                  field = c("properties", "datetime"),
                  apply_fn = function(x) {x})
    )

    testthat::expect_error(
      items_apply(res,
                  field = c("propertiessdddd"),
                  apply_fn = function(x) {x})
    )

    items_applied <- suppressMessages(
      items_apply(
        items = res,
        field = c("properties"),
        apply_fn = function(x) {
          x[["eo:cloud_cover"]] <- x[["eo:cloud_cover"]] * 1.10
          x
        })
    )

    item_applied <- suppressMessages(
      items_apply(
        items = item_stac,
        field = c("properties"),
        apply_fn = function(x) {
          x[["eo:cloud_cover"]] <- x[["eo:cloud_cover"]] * 1.10
          x
        })
    )

    testthat::expect_equal(
      object = subclass(items_applied),
      expected = "STACItemCollection"
    )


    testthat::expect_equal(
      object = subclass(item_applied),
      expected = "STACItem"
    )

    cc_origin <- items_reap(
      items = item_stac,
      field = c("properties", "eo:cloud_cover")
    )

    cc_applied <- items_reap(
      items = item_applied,
      field = c("properties", "eo:cloud_cover")
    )

    testthat::expect_equal(
      object = cc_origin * 1.10,
      expected = cc_applied
    )

    testthat::expect_error(
      suppressMessages(
        items_apply(
          items = res,
          field = c("properties"),
          apply_fn = function(x) {
            x[["eo:cloud_cover"]] <- x[["eo:cloud_cover"]] * 1.10
            x[["eo:cloud_cover"]]
          })
      )
    )

    testthat::expect_error(
      suppressMessages(
        items_apply(
          items = item_applied,
          field = c("properties"),
          apply_fn = function(x) {
            x[["eo:cloud_cover"]] <- x[["eo:cloud_cover"]] * 1.10
            x[["eo:cloud_cover"]]
          })
      )
    )
  })
})
