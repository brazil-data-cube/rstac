testthat::test_that("assets functions", {
    # skip cran check test
    testthat::skip_on_cran()

    # assets_download-----------------------------------------------------------

    # error - zero items
    testthat::expect_error(
      rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(
          collections = "CB4_64_16D_STK-1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-47.02148, -12.98314, -42.53906, -17.35063),
          limit       = 0) %>%
        get_request() %>%
        assets_download(asset_names = c("blue", "evi"))
    )

    # error - given another object
    testthat::expect_error(
      stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request(.) %>%
        assets_download(., asset_names = c("blue", "evi"))
    )

    # error - wrong path
    testthat::expect_error(
      stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(
          collections = "CB4_64_16D_STK-1",
          datetime    = "2019-09-01/2019-11-01",
          limit       = 1) %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        output_dir = "./non-existing-dir/")
    )

    # verify output object
    testthat::expect_equal(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(
            collections = "CB4_64_16D_STK-1",
            datetime    = "2019-09-01/2019-11-01",
            limit       = 1) %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"), output_dir = tempdir())
        subclass(x)
      },
      expected = "STACItemCollection"
    )

    # deprec param
    testthat::expect_message(
      object = {
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(
            collections = "CB4_64_16D_STK-1",
            datetime    = "2019-09-01/2019-11-01",
            limit       = 1) %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          fn = function(x) { x },
                          output_dir = tempdir(),
                          overwrite = TRUE)
      },
      regexp = "deprecated"
    )

    testthat::expect_equal(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(
            collections = "CB4_64_16D_STK-1",
            datetime    = "2019-09-01/2019-11-01",
            limit       = 1) %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          items_max = 2,
                          download_fn = function(x) { x },
                          output_dir = tempdir(),
                          overwrite = TRUE)
        subclass(x)
      },
      expected = "STACItemCollection"
    )

    testthat::expect_error(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          stac_search(
            collections = "CB4_64_16D_STK-1",
            datetime    = "2019-09-01/2019-11-01",
            limit       = 1) %>%
          get_request()

        x[["features"]] <- NULL
        assets_download(items = x,
                        asset_names = c("thumbnail"),
                        items_max = 2,
                        download_fn = function(x) { x },
                        output_dir = tempdir(),
                        overwrite = TRUE)
      }
    )

    testthat::expect_equal(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          output_dir = tempdir(),
                          overwrite = TRUE)
        subclass(x)
      },
      expected = "STACItem"
    )

    # deprec fn param
    testthat::expect_message(
      object = {
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          fn = function(x) {x},
                          output_dir = tempdir(),
                          overwrite = TRUE)
      },
      regexp = "deprecated"
    )

    testthat::expect_equal(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          items_max = 2,
                          download_fn = function(x) {x},
                          output_dir = tempdir(),
                          overwrite = TRUE)
        subclass(x)
      },
      expected = "STACItem"
    )

    # file already exists
    testthat::expect_error(
      object = {
        x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          items_max = 2,
                          download_fn = function(x) {x},
                          output_dir = tempdir(),
                          overwrite = FALSE)
      }
    )

    stac_items <- stac("https://brazildatacube.dpi.inpe.br/stac") %>%
      stac_search(collections = "CB4_64_16D_STK-1") %>%
      stac_search(limit = 2) %>%
      get_request()

    stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      collections("CB4_64_16D_STK-1") %>%
      items("CB4_64_16D_STK_v001_022023_2020-07-11_2020-07-26") %>%
      get_request()

    # assets_select-----------------------------------------------------------

    # an error is expected if a non-existent asset is provided for
    # STACItemCollection obj
    testthat::expect_error(assets_select(stac_items, asset_names = "DDDDD"))

    # an error is expected if a non-existent asset is provided for
    # STACItem obj
    testthat::expect_error(assets_select(stac_item, asset_names = "DDDDD"))

    # return the same object after select?
    testthat::expect_s3_class(
      object = assets_select(stac_items, asset_names = "BAND13"),
      class = c("STACItemCollection", "RSTACDocument")
    )

    # return the same object after select?
    testthat::expect_s3_class(
      object = assets_select(stac_item, asset_names = "BAND13"),
      class = c("STACItem", "RSTACDocument")
    )

    # were the asset selected?
    testthat::expect_equal(
      object = items_assets(assets_select(stac_items, asset_names = "BAND13"),
                            simplify = TRUE),
      expected = "BAND13"
    )

    # were the asset selected?
    testthat::expect_equal(
      object = items_assets(assets_select(stac_item, asset_names = "BAND13")),
      expected = "BAND13"
    )

    # assets_url----------------------------------------------------------
    testthat::expect_equal(
      object =  class(assets_url(stac_items)),
      expected = "list"
    )

    testthat::expect_equal(
      object = class(assets_url(stac_item)),
      expected = "list"
    )

    testthat::expect_equal(
      object =  gdalvsi_append("s3://abc.com"),
      expected = "/vsis3/abc.com"
    )

    testthat::expect_equal(
      object =  gdalvsi_append("gs://abc.com"),
      expected = "/vsigs/abc.com"
    )

    testthat::expect_equal(
      object = gdalvsi_append("http://abc.com"),
      expected = "/vsicurl/http://abc.com"
    )

    testthat::expect_equal(
      object = gdalvsi_append("https://abc.com"),
      expected = "/vsicurl/https://abc.com"
    )

    testthat::expect_equal(
      object =  gdalvsi_append("httpdds://abc.com"),
      expected = "httpdds://abc.com"
    )

    # return the same object after filter?
    testthat::expect_s3_class(
      object = assets_select(stac_items, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItemCollection", "RSTACDocument")
    )

    testthat::expect_s3_class(
      object = assets_select(stac_item, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItem", "RSTACDocument")
    )

    # return the same object after filter?
    testthat::expect_s3_class(
      object = assets_select(stac_items, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItem", "RSTACDocument")
    )

    # assets_filter-----------------------------------------------------------
    # deprec function assets_filter
    testthat::expect_equal(
      object = {suppressWarnings(class(assets_filter(stac_items, `eo:bands` < 6)))},
      expected = c("STACItemCollection", "RSTACDocument", "list")
    )

    # deprec function assets_filter
    testthat::expect_equal(
      object = {suppressWarnings(class(assets_filter(stac_items, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      })))},
      expected = c("STACItemCollection", "RSTACDocument", "list")
    )

    # deprec function assets_filter
    testthat::expect_equal(
      object = {suppressWarnings(class(assets_filter(stac_item, `eo:bands` < 6)))},
      expected = c("STACItem", "RSTACDocument", "list")
    )

    # deprec function assets_filter
    testthat::expect_error(
      object = suppressWarnings(assets_filter(stac_item, a = `eo:bands` < 6)),
    )

    # deprec function assets_filter
    testthat::expect_error(
      object = suppressWarnings(assets_filter(stac_item, `eo:dbandsd` < 6)),
    )

    # deprec function assets_filter
    testthat::expect_error(
      object = suppressWarnings(assets_filter(stac_items, a = `eo:bands` < 6)),
    )

    # deprec function assets_filter
    testthat::expect_error(
      object = suppressWarnings(assets_filter(stac_items, `eo:dbandsd` < 6)),
    )

    # deprec function assets_filter
    testthat::expect_equal(
      object = {suppressWarnings(class(assets_filter(stac_item, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      })))},
      expected = c("STACItem", "RSTACDocument", "list")
    )
})
