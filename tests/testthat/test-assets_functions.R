testthat::test_that("assets functions", {
  vcr::use_cassette("assets_download", {
    # skip cran check test
    testthat::skip_on_cran()

    # assets_download-----------------------------------------------------------

    # error - zero items
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
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
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request(.) %>%
        assets_download(., asset_names = c("blue", "evi"))
    )

    # error - wrong path
    testthat::expect_error(
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
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
        x <- stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/") %>%
          stac_search(
            collections = "CB4_64_16D_STK-1",
            datetime    = "2019-09-01/2019-11-01",
            limit       = 1) %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"), output_dir = ".")
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
          assets_download(assets_name = c("thumbnail"),
                          output_dir = ".",
                          overwrite = TRUE)
      },
      regexp = "deprecated"
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
                          output_dir = ".",
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
                          output_dir = ".",
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
                        output_dir = ".",
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
                          output_dir = ".",
                          overwrite = TRUE)
        subclass(x)
      },
      expected = "STACItem"
    )

    # deprec param
    testthat::expect_message(
      object = {
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(assets_name = c("thumbnail"),
                          output_dir = ".",
                          overwrite = TRUE)
      },
      regexp = "deprecated"
    )

    testthat::expect_message(
      object = {
        stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
          collections("CB4_64_16D_STK-1") %>%
          items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
          get_request() %>%
          assets_download(asset_names = c("thumbnail"),
                          fn = function(x) {x},
                          output_dir = ".",
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
                          output_dir = ".",
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
                          output_dir = ".",
                          overwrite = FALSE)
      }
    )
  })
  vcr::use_cassette("assets_functions", {

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

    # assets_gdalvfs----------------------------------------------------------
    testthat::expect_s3_class(
      object =  assets_append_gdalvsi(stac_items),
      class = c("STACItemCollection", "RSTACDocument")
    )

    testthat::expect_s3_class(
      object =  assets_append_gdalvsi(stac_item),
      class = c("STACItem", "RSTACDocument")
    )

    testthat::expect_equal(
      object =  {

        mock_s3_obj <- stac_item
        mock_s3_obj[["assets"]][[1]][["href"]] <- "s3://abc.com"

        x <- assets_append_gdalvsi(mock_s3_obj)
        x[["assets"]][[1]][["href"]]
      },
      expected = "/vsis3/abc.com"
    )

    testthat::expect_equal(
      object =  {

        mock_gs_obj <- stac_item
        mock_gs_obj[["assets"]][[1]][["href"]] <- "gs://abc.com"

        x <- assets_append_gdalvsi(mock_gs_obj)
        x[["assets"]][[1]][["href"]]
      },
      expected = "/vsigs/abc.com"
    )

    testthat::expect_equal(
      object =  {

        mock_http_obj <- stac_item
        mock_http_obj[["assets"]][[1]][["href"]] <- "http://abc.com"

        x <- assets_append_gdalvsi(mock_http_obj)
        x[["assets"]][[1]][["href"]]
      },
      expected = "/vsicurl/http://abc.com"
    )

    testthat::expect_equal(
      object =  {

        mock_https_obj <- stac_item
        mock_https_obj[["assets"]][[1]][["href"]] <- "https://abc.com"

        x <- assets_append_gdalvsi(mock_https_obj)
        x[["assets"]][[1]][["href"]]
      },
      expected = "/vsicurl/https://abc.com"
    )

    testthat::expect_equal(
      object =  {

        mock_inv_obj <- stac_item
        mock_inv_obj[["assets"]][[1]][["href"]] <- "httpdds://abc.com"

        x <- assets_append_gdalvsi(mock_inv_obj)
        x[["assets"]][[1]][["href"]]
      },
      expected = "httpdds://abc.com"
    )

    # assets_filter-----------------------------------------------------------

    # return the same object after filter?
    testthat::expect_s3_class(
      object = assets_filter(stac_items, `eo:bands` < 6),
      class = c("STACItemCollection", "RSTACDocument")
    )

    # return the same object after filter?
    testthat::expect_s3_class(
      object =  assets_filter(stac_item, `eo:bands` < 6),
      class = c("STACItem", "RSTACDocument")
    )

    # return the same object after filter?
    testthat::expect_s3_class(
      object = assets_filter(stac_items, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItemCollection", "RSTACDocument")
    )

    testthat::expect_s3_class(
      object = assets_filter(stac_item, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItem", "RSTACDocument")
    )

    # deprec param
    testthat::expect_message(
      object = assets_filter(stac_items, fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      regexp = "deprecated"
    )

    testthat::expect_message(
      object = assets_filter(stac_item, fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      regexp = "deprecated"
    )

    # return the same object after filter?
    testthat::expect_s3_class(
      object = assets_filter(stac_items, filter_fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      class = c("STACItem", "RSTACDocument")
    )

    # return the same object after filter?
    testthat::expect_message(
      object = assets_filter(stac_items, fn = function(x) {
        if ("eo:bands" %in% names(x))
          return(x$`eo:bands` < 6)
        return(FALSE)
      }),
      regexp = "deprecated"
    )

    # an error is expected if the ellipses is named
    testthat::expect_error(
      assets_filter(stac_items, j = "a" < 20)
    )

    # an error is expected if the ellipses is named
    testthat::expect_error(
      assets_filter(stac_item, j = "a" < 20)
    )

    # an error is expected if the ellipses is named
    testthat::expect_error(
      assets_filter(stac_item, j = "a" < 20)
    )
  })
})
