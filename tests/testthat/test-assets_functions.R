testthat::test_that("assets functions", {
  # skip cran check test
  testthat::skip_on_cran()

  # assets_download-----------------------------------------------------------
  testthat::expect_equal(
    object = rstac::stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
      stac_search(
        collections = "CB4_64_16D_STK-1",
        datetime    = "2019-09-01/2019-11-01",
        bbox        = c(-47.02148, -12.98314, -42.53906, -17.35063),
        limit       = 0) %>%
      get_request() %>%
      assets_download(asset_names = c("blue", "evi"),
                      create_json = FALSE) %>%
      items_length(),
    expected = 0
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
        assets_download(asset_names = c("thumbnail"),
                        output_dir = tempdir(),
                        create_json = FALSE)
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
                        create_json = FALSE,
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
                        create_json = FALSE,
                        overwrite = TRUE)
      subclass(x)
    },
    expected = "STACItemCollection"
  )

  testthat::expect_equal(
    object = {
      x <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
        collections("CB4_64_16D_STK-1") %>%
        items("CB4_64_16D_STK_v001_020024_2019-11-01_2019-11-16") %>%
        get_request() %>%
        assets_download(asset_names = c("thumbnail"),
                        output_dir = tempdir(),
                        create_json = FALSE,
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
                        create_json = FALSE,
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
                        create_json = FALSE,
                        overwrite = TRUE)
      subclass(x)
    },
    expected = "STACItem"
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
    object = items_assets(assets_select(stac_items, asset_names = "BAND13")),
    expected = "BAND13"
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(assets_select(stac_items,
                                        asset_names = c("BAND14", "EVI"),
                                        `eo:bands` == 9)),
    expected = "EVI"
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(assets_select(stac_item,
                                        asset_names = c("BAND14", "EVI"),
                                        `eo:bands` == 9)),
    expected = "EVI"
  )

  # were the asset selected?
  testthat::expect_equal(
    object = items_assets(assets_select(stac_item, asset_names = "BAND13")),
    expected = "BAND13"
  )

  # assets_rename-------------------------------------------------------------
  selected_items <- assets_select(stac_items, c("BAND13", "BAND14"))
  selected_item <- assets_select(stac_item, c("BAND13", "BAND14"))

  testthat::expect_error(
    assets_rename(selected_item)
  )

  testthat::expect_error(
    assets_rename(selected_item, "BAND13" = "B14", "B14")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items, c("BAND13" = "B13")),
    class = c("STACItemCollection", "RSTACDocument")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item, c("BAND13" = "B13")),
    class = c("STACItem", "RSTACDocument")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items, list("BAND13" = "B13")),
    class = c("STACItemCollection", "RSTACDocument")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item, list("BAND13" = "B13")),
    class = c("STACItem", "RSTACDocument")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_items,
                           list(BAND13 = "B13"),
                           BAND14 = "B14"),
    class = c("STACItemCollection", "RSTACDocument")
  )

  testthat::expect_s3_class(
    object = assets_rename(selected_item,
                           list(BAND13 = "B13"),
                           BAND14 = "B14"),
    class = c("STACItem", "RSTACDocument")
  )

  testthat::expect_equal(
    object = items_assets(assets_rename(selected_items,
                                        list(BAND13 = "B13"),
                                        BAND14 = "B14")),
    expected = c("B13", "B14")
  )

  testthat::expect_equal(
    object = items_assets(assets_rename(selected_item,
                           list(BAND13 = "B13"),
                           BAND14 = "B14")),
    expected = c("B13", "B14")
  )

  testthat::expect_equal(
    object = items_assets(
      assets_rename(selected_item,mapper = function(x) paste0(x[["eo:bands"]]))
    ),
    expected = c("3", "4")
  )

  # assets_url----------------------------------------------------------
  testthat::expect_equal(
    object =  class(assets_url(stac_items)),
    expected = "character"
  )

  testthat::expect_equal(
    object = class(assets_url(stac_item)),
    expected = "character"
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

  testthat::expect_error(
    assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    })
  )

  testthat::expect_error(
    assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    })
  )

  testthat::expect_s3_class(
    object = assets_select(stac_item, select_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    }),
    class = c("STACItem", "RSTACDocument")
  )

  # return the same object after filter?
  testthat::expect_error(
    object = assets_select(stac_items, filter_fn = function(x) {
      if ("eo:bands" %in% names(x))
        return(x$`eo:bands` < 6)
      return(FALSE)
    })
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
