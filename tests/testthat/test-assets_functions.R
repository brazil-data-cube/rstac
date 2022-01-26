testthat::test_that("assets functions", {
  vcr::use_cassette("assets_download", {
    # skip cran check test
    testthat::skip_on_cran()

    # error - zero items
    testthat::expect_error(
      rstac::stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(
          collections = "CB4_64_16D_STK-1",
          datetime    = "2019-09-01/2019-11-01",
          bbox        = c(-47.02148, -12.98314, -42.53906, -17.35063),
          limit       = 0) %>%
        get_request() %>%
        assets_download(asset_names = c("blue", "evi")))

    # error - given another object
    testthat::expect_error(
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        get_request(.) %>%
        assets_download(., asset_names = c("blue", "evi")))

    # error - wrong path
    testthat::expect_error(
      stac("http://brazildatacube.dpi.inpe.br/stac/") %>%
        stac_search(
          collections = "CB4_64_16D_STK-1",
          datetime    = "2019-09-01/2019-11-01",
          limit       = 1) %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        output_dir = "./non-existing-dir/"))

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
      expected = "STACItemCollection")

    vcr::use_cassette("assets_functions", {

      stac_items <- stac("https://brazildatacube.dpi.inpe.br/stac") %>%
        stac_search(collections = "CB4_64_16D_STK-1") %>%
        stac_search(limit = 2) %>%
        get_request()

      stac_item <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64_16D_STK-1") %>%
        items("CB4_64_16D_STK_v001_022023_2020-07-11_2020-07-26") %>%
        get_request()

      # ---- assets_select ----#

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

      # ---- assets_filter ----#

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
        object = assets_filter(stac_items, fn = function(x) {
          if ("eo:bands" %in% names(x))
            return(x$`eo:bands` < 6)
          return(FALSE)
        }),
        class = c("STACItemCollection", "RSTACDocument")
      )

      # return the same object after filter?
      testthat::expect_s3_class(
        object = assets_filter(stac_items, fn = function(x) {
          if ("eo:bands" %in% names(x))
            return(x$`eo:bands` < 6)
          return(FALSE)
        }),
        class = c("STACItem", "RSTACDocument")
      )

      # an error is expected if the ellipses is named
      testthat::expect_error(
        assets_filter(stac_items, j = "a" < 20)
      )

      # an error is expected if the ellipses is named
      testthat::expect_error(
        assets_filter(stac_item, j = "a" < 20)
      )
    })
  })
})
